
## O Script se destina à extração de dados das listas de distribuição para
  # uma base de dados única em formato "data frame"

## O processo de extração de dados é computacionalmente demandante (~10 min).
  # Preparamos quatro arquivos para que o interessado possa aferir os trabalhos
  # em estágios diferentes:

  # load(here::here("Documentos", "Distribuicao_dl.RData"))
    # Contém uma lista aninhada para cada página de pdf, excluidas as em branco

  # load(here::here("Documentos", "Distribuicao_dlc.RData"))
    # Contém uma lista aninhada para cada cargo que recebeu processos em certa data
    
  # load(here::here("Documentos", "Distribuicao_padrao.RData"))
    # Contém lista aninhada após iteraçao para lidar com processos distribuidos
    # à Secretaria Executiva e não para um cargo específico

  # load(here::here("Documentos", "Distribuicao_df.RData"))
    # Consiste na base de dados já finalizada

## Até o momento a base de dados é constituida por três tabelas distintas:

  # load(here::here("Documentos", "Distribuicao_df.RData"))
    # Contém as informações extraidas diretamente das listas de distribuição

  # load(here::here("Documentos", "municipios_sp.RData"))
    # Contém a lista de todos os municípios de SP o código IBGE e
    # a comarca a que cada um pertence

  # load(here::here("Documentos", "codigos_sp.RData"))
    # Vincula cada código de "unidade judiciária" (OOOO) à
    # Comarca a que pertence, bem como a descrição de Foro

################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "pdftools",
             "tidyverse",
             "tidyr",
             "stringr",
             "purrr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

rm(pacotes)


################################################################################
#                EXTRAÇÃO DE DADOS  A PARTIR DE PDFs                           #
################################################################################

## Cria uma lista que contenha o nome e o caminho até cada pdf
#" pattern = 'pdf$' para que apenas arquivos do tipo PDF seham listados
# "recurse = T" para que a função procure PDFs contidos em subpastas
# "full.names = T" para que a lista armazene o caminho até cada arquivo.
# Caso contrário, as demais funções não serão capazes de encontrar os arquivos
# em suas respectivs subpastas

lista_distribuicao <- list.files("PDFs/Distribuicao", pattern = "pdf$",
                                 recursive = T, full.names = T)


## Cria um arquivo PDF único - Facilita a identificação de erros e permite
# que este trabalho faça referência a uma página específica.
#A extração sequecial dos arquivos com lapply() retorna erros
pdf_combine(input = iconv(lista_distribuicao, to = "latin1//TRANSLIT", from = "UTF-8"), output = "PDFs/Distribuicao_Consolidada.pdf")


rm(lista_distribuicao)

## Cria uma lista de data frames a partir de cada página do pdf
distribuicao_dl <- pdf_data("PDFs/Distribuicao_Consolidada.pdf")

## Vejamos como as informações textuais são armazenadas em cada DF
length(distribuicao_dl)
View(distribuicao_dl[[3]])

## Removendo as colunas sem relevância para a análise
distribuicao_dl <- lapply(distribuicao_dl, select, -c(width, height, space))

## Ordenando os dados a partir do eixo "Y" - ordem crescente
distribuicao_dl <- lapply(distribuicao_dl, arrange, "y")

## Todas as observação serão agrupadas conforme sua posição no eixo "y"
#Isso assegura que o "tipo" e "natureza" do processo estejam posicionados
# Na mesma linha que o processo a que se referem.

distribuicao_dl <- lapply(distribuicao_dl, spread, x, text)

## O eixo "y" só era relevante para odernar as informações textuais
# conforme a posição em que apareciam no pdf.
# como o conteúdo já foi ordenado por "linha", podemos excluir a coluna "y"
distribuicao_dl <- lapply(distribuicao_dl, select, -c(y))

## Todos as colunas serão aglutinadas em apenas uma
distribuicao_dl <- lapply(distribuicao_dl, unite, "Linha",
                          sep = " ", na.rm = T)

## Cada sublista será nomeada conforme seu número de página em
# "Distribuicao_Consolidada.pdf"
# Isso facilitará a conferência de dados mesmo que alguns dataframes
# sejam filtrados a seguir.

# ATENÇÃO!!!! Esse procedimento não é estritamente necessário
# abertura do pdf exige muita memória
# Apenas utilizamos o processo para assegurar a consistência dos dados,
# facilitar a auditoria por interessados e esclarecer os próximos passos.

names(distribuicao_dl) <- seq_along(distribuicao_dl)

## Vejamos como as observações passaram a ser armazenadas:

View(distribuicao_dl$'3')
View(distribuicao_dl$'580')

## Nota-se que a página 580 não contém qualquer número de processo.
#Trata-se de uma "página em branco" e resulta de um erro no momento da 
# distribuição.
# Podemos identificar as páginas em branco como seguinte algoritmo:

pagina_vazia <- purrr::keep(distribuicao_dl, ~all(!str_detect(.x$Linha, "^\\d{7}.+")))
rm(pagina_vazia)

## Desejamos manter apenas as páginas que contenham pelo menos um número de processo

distribuicao_dl <- purrr::keep(distribuicao_dl, ~any(str_detect(.x$Linha, "^\\d{7}.+")))

## Vejamos agora mais um desafio a ser soluncionado:

View(distribuicao_dl$'98')

## É possível notar que a página 98 não descreve para qual cargo os
# processos foram distribuídos.
# Trata-se da continuação da página 97.

# Podemos identificar as "continuações" com o seguinte algoritomo:

continuacao <- purrr::keep(distribuicao_dl, ~all(!str_detect(.x$Linha,
                                                             "Distribu.+a\\(o\\) Dr\\(a\\)\\..*")))

#Pretendemos adicionar as observações armazenadas nas "continuações" com o
# data frame imediatamente anterior (i.e., com o início da lista de distribuição).

#Em seguida, pretendemos excluir as "continuações" para eliminar a duplicidade.

#Para tanto, empregaremos uma iteração de 1 até o resultado da subtração
# entre o número de listas aninhadas em "distribuicao_dl" e as sublistas do
# objeto "continuacão".
# 
#Há que se atentar para o fato de o código eliminar listas à medida que
#identifica uma "continuação". Em razão disso, o comprimento (length)
# do objeto "distribuicao_dl" reduz Ao longo das iterações.

#É certo que o número final de listas aninhadas corresponde ao número inicial, 
# menos as subtraídas, que, no caso, são todas as sublistas constantes
# do objeto "continuacao".

save(distribuicao_dl, file="Documentos/Distribuicao_dl.RData")

distribuicao_dlc <- distribuicao_dl
n_iteracoes = length(distribuicao_dl)-length(continuacao)+1
n = 1
while(n < n_iteracoes){
  if(all(!str_detect(distribuicao_dlc[[n]]$Linha,"Distribu.+a\\(o\\) Dr\\(a\\)\\..*"))){
    distribuicao_dlc[[n]]$Linha <-  str_remove(distribuicao_dlc[[n]]$Linha,"Data da Dis.+")
    distribuicao_dlc[[n-1]] <- bind_rows(distribuicao_dlc[[n-1]], distribuicao_dlc[[n]])
    distribuicao_dlc[[n]] <- NULL
    n <-  n - 1
  }
  n <- n + 1
}

# Podemos confirmar que os dados foram manipulados corretamente:

View(continuacao$'98') # As observações armazenadas em uma "continuação" ...
View(distribuicao_dlc$'97') #Foram aglutinadas ao final da lista precedente...
View(distribuicao_dlc$'98') #Enquanto a "continuação" foi eliminada.
length(purrr::keep(distribuicao_dlc, ~all(!str_detect(.x$Linha,
                                                      "Distribu.+a\\(o\\) Dr\\(a\\)\\..*"))))

rm(continuacao, distribuicao_dl, n, n_iteracoes)

##Distribuição de ciência - A partir de dezembro de 2020
#O Ministério Público também armazena informações quanto a processos que foram
#encaminhados para a instituição APÓS a decisão do Tribunal de Justiça

#Ao final de 2020, tais dados passaram a ser armazenados em conjunto com as
#listas de processos distribuidos.

#Isso pode provocar duplicidade e confusão com os processos que foram efetivamente
#encaminhados para o oferecimento de parecer jurídico peo "Parquet".

View(distribuicao_padrao$'15366')

ciencia <- purrr::keep(distribuicao_dlc, ~any(str_detect(.x$Linha,
                                                         ".*Processos - CIÊNCIA")))
n_iteracoes = length(distribuicao_dlc)-length(ciencia)+1
n = 1
while(n < n_iteracoes){
  if(any(str_detect(distribuicao_dlc[[n]]$Linha, ".*Processos - CIÊNCIA"))){
    distribuicao_dlc[[n]] <- NULL
    n <-  n - 1
  }
  n <- n + 1
}

rm(ciencia, n, n_iteracoes)
save(distribuicao_dlc, file="Documentos/Distribuicao_dlc.RData")

## Resta um último desafio antes da extração de dados.

# Vejamos o formato padrão das listas
View(distribuicao_dlc$'1')

# Note que os processos não foram distribuidos a um cargo específico
  # São casos atribuidos à Secretaria Executiva
View(distribuicao_dlc$'32') 

# Note o destinatário dos processos aparece em linha separada:
View(distribuicao_dlc$'11972') 

# É possível isolar as listas a serem modificadas com o seguinte algoritmo:
outros <- purrr::keep(distribuicao_dlc, ~all(!str_detect(.x$Linha,
                                                         "^\\d{2}º Procurador.+")))
distribuicao_padrao <- distribuicao_dlc


for(n in 1:length(distribuicao_padrao)){
  if(all(!str_detect(distribuicao_padrao[[n]]$Linha,"^\\d{2}º Procurador.+"))){
    
    distribuicao_padrao[[n]]$Linha[6] <- "99º Procurador de Justiça da Proc de Justiça Cível"
  } 
}

rm(outros, n)
save(distribuicao_padrao, file="Documentos/Distribuicao_padrao.RData")

################################################################################
#                    EXTRAÇÃO DE VARIÁVEIS RELEVANTES                          #
################################################################################

# As informações desejadas são:
# a) A Data da Distribuição: "DD/MM/AA"
# b) O Cargo que recebeu o processo.
# c) O Numero do Processo
# d) O Tipo de manifestação (ex. "apelação", "agravo de instrumento")
# e) A natureza da causa (ex. "alimentos", "guarda", "falência")
# f) O Código da Unidade Jundicáira (os 4 últimos dígitos do processo)
# g) O Código do Tribunal de origem (O TJSP é repesentado como "8.26")

#  Também seria desejável obter o número de feitos que permaneceram acumulados 
# na secretaria sem distribuição (o "accervo" de processos acumulados)
# Infelizmente, tal informação não é padronizada e sequer é informada em
# muitos casos.

# É preciso ser o mais explícito possível em algumas expressões regulares
# Veja o exemplo a seguir:
View(distribuicao_padrao$'246')
# Note que existe mensão a um processo não distribuido
# Mais do que isso, ele não é seguido de outras informações como Tipo e Natureza
# Uma expressão regular como "^\\d{7}.+?[:upper:]+.+?([:upper:]+).*" não exclui 

View(distribuicao_padrao$'246')
View(distribuicao_padrao$'13245')

# Adicionou-se uma coluna para "Mídias":
View(distribuicao_padrao[[13245]]) 
View(distribuicao_padrao$'354')
 
# A ordem das colunas "Tipo" e "Retorno" estão invertidas em algumas tabelas
# Note ainda que algumas das "Naturezas" são acompanhadas de dígitos "Saúde2"
View(distribuicao_padrao$'7231')

distribuicao_df <- distribuicao_padrao |>
  
  # "map_dfr" cria um df a partir da junção de todas a linhas de todas as listas
  # ".id" cria uma variável contendo o nome de cada sub lista
  map_dfr(~ .x, .id = "pagina") |> 

  # Modificando o tipo da Variável "Página" para valores numéricos  
  mutate(pagina = as.numeric(pagina)) |>

  # Estamos agrupando os dados conforme a variável "Página" recém criada
  group_by(pagina) |>
  
  
  summarise(cargo = str_replace(Linha[str_detect(Linha,
                    "(^\\d{2})º Procurador.+")],
                    "(^\\d{2})º Procurador.+", "\\1") |>
                    as.factor(),
            
            processo = str_replace(Linha[str_detect(Linha,
                    "(^\\d{7}.+?\\d{4}.+?\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                    "(^\\d{7}.+?\\d{4}.+?\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1"),
                    # "?" Serve como "Lazy Quantifier" na expressão regular
            
            propositura = str_replace(Linha[str_detect(Linha,
                    "^\\d{7}.+?(\\d{4}).+?\\d{4}.+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                    "^\\d{7}.+?(\\d{4}).+?\\d{4}.+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1") |>
                    as.numeric(),
            
            codigo = str_replace(Linha[str_detect(Linha,
                    "^\\d{7}.+?\\d{4}.+?(\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                    "^\\d{7}.+?\\d{4}.+?(\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1"),
            
            tribunal = str_replace(Linha[str_detect(Linha,
                     "^\\d{7}.+?\\d{4}\\.(\\d{1}\\.\\d{2}).+\\d{4}.+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                     "^\\d{7}.+?\\d{4}\\.(\\d{1}\\.\\d{2}).+\\d{4}.+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1"),           

            digital = str_replace(Linha[str_detect(Linha,
                    "^\\d{7}.+?\\d{4}.+?\\d{4}.+?([:upper:]{3,}+).+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                    "^\\d{7}.+?\\d{4}.+?\\d{4}.+?([:upper:]{3,}+)+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1") |>
                    as.factor(),
            
            tipo = str_replace(Linha[str_detect(Linha,
                   "^\\d{7}.+?\\d{4}.+?\\d{4}.+?[:upper:]{3,}.[:digit:]+.*?([:upper:]+[^Não|^Sim]).+\\s+\\d+")],
                   "^\\d{7}.+?\\d{4}.+?\\d{4}.+?[:upper:]{3,}.[:digit:]+.*?([:upper:]+[^Não|^Sim]).+\\s+\\d+", "\\1") |>
                   as.factor(), 
            
            natureza = str_replace(Linha[str_detect(Linha,
                   "^\\d{7}.+?\\d{4}.+?\\d{4}.+?[:upper:]{3,}+.[:digit:]+.*?[:upper:]+\\S+.+?[:upper:]+.+?([:upper:]{2,}\\D*).*\\s+\\d+$")],
                   "^\\d{7}.+?\\d{4}.+?\\d{4}.+?[:upper:]{3,}+.[:digit:]+.*?[:upper:]+\\S+.+?[:upper:]+.+?([:upper:]{2,}\\D*).*\\s+\\d+$", "\\1")|>
                   as.factor(),  
            
            data = first(str_replace(Linha[str_detect(Linha,
                   "^.*\\d{2}/\\d{2}/\\d{2}$")],
                   "^.*(\\d{2}/\\d{2}/\\d{2})$", "\\1")) |>
                   as.Date("%d/%m/%y"),

            # dia = first(str_replace(Linha[str_detect(Linha,
            #        "^.*\\d{2}/\\d{2}/\\d{2}$")],
            #        "^.*(\\d{2})/\\d{2}/\\d{2}$", "\\1")),
            # mes = first(str_replace(Linha[str_detect(Linha,
            #        "^.*\\d{2}/\\d{2}/\\d{2}$")],
            #        "^.*\\d{2}/(\\d{2})/\\d{2}$", "\\1")),
            # ano = first(str_replace(Linha[str_detect(Linha,
            #        "^.*\\d{2}/\\d{2}/\\d{2}$")],
            #        "^.*\\d{2}/\\d{2}/(\\d{2})$", "\\1")),
            .groups = "drop")|>
  select(cargo, processo, propositura, codigo, tribunal, digital, tipo, natureza, data, pagina)|>
  arrange(data, pagina, cargo)


rm(distribuicao_padrao, distribuicao_dlc)

## Removendo informações duplicadas
  # Algumas listas apenas retificam distribuições anteriores
  #".keep_all = TRUE" retém todas as colunas da tabela.
distribuicao_df <- distinct(distribuicao_df, processo, data, .keep_all = TRUE)


save(distribuicao_df, file="Documentos/Distribuicao_df.RData")

