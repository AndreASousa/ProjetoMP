
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

## Esse trabalho intenta seguir as formas normais para a criação do banco de dados:
  # 1ª Forma normal - Cada linha traz uma informação diferente
  # 2ª Forma normal - Todas as colunas que não participam da chave primária são
    # dependentes desta.

  # Por essa razão, os dados não estão aglutinados em uma só tabela, mas 
    # separados em mais de um data.frame conectável pelas chaves primárias.

  # Os "sujeitos" do bando de dados são a) os números de Processo; b) as Comarcas;
    # e c) as cidades (identificadas pelo código do IBGE).

## Até o momento a base de dados é constituida pelas seguitnes tabelas:

  # load(here::here("Documentos", "Distribuicao_df.RData"))
    # Contém as informações extraidas diretamente das listas de distribuição

  # load(here::here("Documentos", "municipios_sp.RData"))
    # Contém a lista de todos os municípios de SP o código IBGE e
    # a comarca a que cada um pertence

  # load(here::here("Documentos", "codigos_sp.RData"))
    # Vincula cada código de "unidade judiciária" (OOOO) à
    # Comarca a que pertence, bem como a descrição de Foro

  # load(here::here("Documentos", "populacao.RData"))
  # load(here::here("Documentos", "menor_25.RData"))
  # load(here::here("Documentos", "maior_60.RData"))
  # load(here::here("Documentos", "ocupados.RData"))
  # load(here::here("Documentos", "pib.RData"))
  # load(here::here("Documentos", "idh.RData"))
    # Aramazenam dados socioeconômicos dos municípios paulistas

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

# A depender da versão do "R" e do pacote pdftools,
#  é necessário modificar o "encoding" dos endereços dos arquivos:
  # pdf_combine(input = iconv(lista_distribuicao_nd, to = "latin1//TRANSLIT", from = "UTF-8"), output = "PDFs/Distribuicao_New_Data.pdf")
pdf_combine(input = lista_distribuicao, output = "PDFs/Distribuicao_Consolidada.pdf")

rm(lista_distribuicao)

## Cria uma lista de data frames a partir de cada página do pdf
distribuicao_dl <- pdf_data("PDFs/Distribuicao_Consolidada.pdf")

## Vejamos como as informações textuais são armazenadas em cada DF
length(distribuicao_dl)
View(distribuicao_dl[[3]])

## Removendo as colunas sem relevância para a análise
distribuicao_dl <- lapply(distribuicao_dl, dplyr::select, -c(width, height, space))

## Ordenando os dados a partir do eixo "Y" - ordem crescente
distribuicao_dl <- lapply(distribuicao_dl, arrange, "y")

## Todas as observação serão agrupadas conforme sua posição no eixo "y"
#Isso assegura que o "tipo" e "natureza" do processo estejam posicionados
# Na mesma linha que o processo a que se referem.

distribuicao_dl <- lapply(distribuicao_dl, spread, x, text)

## O eixo "y" só era relevante para odernar as informações textuais
# conforme a posição em que apareciam no pdf.
# como o conteúdo já foi ordenado por "linha", podemos excluir a coluna "y"
distribuicao_dl <- lapply(distribuicao_dl, dplyr::select, -c(y))

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
  
            # "?" Serve como "Lazy Quantifier" na expressão regular          
            processo = str_replace(Linha[str_detect(Linha,
                    "(^\\d{7}.+?\\d{4}.+?\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                    "(^\\d{7}.+?\\d{4}.+?\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1"),

            
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
  ungroup () %>% droplevels(.) |>
  dplyr::select(cargo, processo, propositura, codigo, tribunal, digital, tipo, natureza, data, pagina)|>
  arrange(data, pagina, processo)

# Removendo espaçamentos, "-", "_", "." e "/"
distribuicao_df$tipo <- str_trim(distribuicao_df$tipo)
distribuicao_df$tipo <- str_replace_all(distribuicao_df$tipo, "-", "")
distribuicao_df$tipo <- str_replace_all(distribuicao_df$tipo, "_", "")
distribuicao_df$tipo <- str_replace_all(distribuicao_df$tipo, "/", "")
distribuicao_df$tipo <- str_replace_all(distribuicao_df$tipo, "\\.", "")
distribuicao_df$tipo <- as.factor(distribuicao_df$tipo)

distribuicao_df$natureza <- str_replace_all(distribuicao_df$natureza, "\\.", "")

## Removendo informações duplicadas
  # Algumas listas apenas retificam distribuições anteriores

## Contudo,alguns processos podem esnejar mais de uma distribuição em uma mesma data.
  # Por exemplo, as ações de recuperação judicial e de falência costumam envolver
  # grande número de credores, cada qual com seus prórios interesses.

  # Uma mesma decisão judicial pode abarcar pedidos formulados por mais de um credor,
  # Cada um deles pode recorrer para a 2ª Instância para rediscutir sua controvérsia.

  # Por essa razão, não existe duplicidade quando um Procurador recebe a atribuição
  # para oferecer parecer em dois ou mais recursos relativos ao mesmo processo.

## Em termos práticos, não existe duplicidade quando um mesmo processo aprece
  # mais de uma vez na mesma lista de distribuição, o que pode ser controlado
  # pelo número de página no "pdf" geral que criamos.


# Identificando todos os processos "duplicados" em uma mesma página de distribuição
 multiplos_recursos <- distribuicao_df |>
   filter(duplicated(distribuicao_df[ , c("processo", "pagina")]))
 
# Mantendondo apenas valores distintos no df original
#".keep_all = TRUE" retém todas as colunas da tabela.
distribuicao_df <- distinct(distribuicao_df, processo, data, .keep_all = TRUE)

# Devolvendo os processos com mais de um recurso para parecer
distribuicao_df <- distribuicao_df |>
  rbind(multiplos_recursos) |>
  arrange(data, pagina, processo)

# Adiciona comentários à estrutura de dados

comment(distribuicao_df) <- c(
  "Lista os procedimentos distribuidos à Procuradoria Cível do Ministério Público de São Paulo [MP].",
  "Cargo : Representa o Cargo de Procurador de Justiça que recebeu o processo. 99 para secretaria;",
  "Processo: Código Unificado pelo CNJ - Vide Resolução CNJ n. 65/08;",
  "Propositura: O ano de ajuizamento ou interposição do processo, recurso ou insidente processual;",
  "Codigo: Unidade de origem dentro da estrutura administrativa dos Tribunais (OOOO);",
  "Tribunal: Tribunal ou Circunscrição Militar em que o procedimento teve início. 8.26 representa o TJ/SP;",
  "Digital: Indica se o procedimento corre em meio digital ou em autos de papel;",
  "Tipo: O tipo de manifestação encaminhada ao MP (Ex. recurso de apelação, recurso especial, ação rescisória etc.);",
  "      O significado dos acrônimos está registrado em: comment(distribuicao_df$tipo)",
  "Natureza: Representa o assunto geral discutido no processo (saúde, alimentos, recuperação judicial etc.);",
  "      O significado das abreviações está registrado em: comment(distribuicao_df$natureza)",
  "Data: O dia em que o procedimento foi distribuido a um dos Procuradores ou à secretaria;",
  "Pagina - mero controle pessoal para o bom desenvolvimento do trabalho") 



comment(distribuicao_df$tipo) <- c(
  "Lista de abreviações:",
  "ADDRESP - Agravo contra despacho denegatório de recurso especial;",
  "ADDREXT - Agravo contra despacho denegatório de recurso extraordinário;",
  "ADDX - Agravo contra despacho denegatório de recurso extraordinário;",
  "ADRRXT - Pedido de efeito suspsensivo a recurso;",
  "AI - Agravo de Instrumento;",
  "AIDDESP - Agravo de instrumento contra despacho denegatório de recurso especial;",
  "AIDDEX - Agravo de instrumento contra despacho denegatório de recurso extraordinário;",
  "AINT - Agravo Interno;",
  "AIRESP - Agravo de instrumento contra despacho denegatório de recurso especial;",
  "AIREXT - Agravo de instrumento contra despacho denegatório de recurso extraordinário;",
  "AJ - Apelação judicial;",
  "AP - Recurso de apelação;",
  "AR - Ação rescisória;",
  "AREG - Agravo regimental;",
  "CAUTELAR - Medida cautelar endereçada para o Tribunal;",
  "CONFLITO - Conflito de competência;",
  "CP - Correição parcial cível;",
  "ED - Embargos de Declaração;",
  "EI - Embargos infringentes;",
  "EX - Embargos de declaração com efeito infringente;",
  "EXT - Recurso extraordinário;",
  "HDATA - Habeas data;",
  "IF - Agravo de Instrumento;",
  "IRDR - Incidente de resolução de demandas repetitivas;",
  "MI - Mandado de injunção;",
  "MS - Mandado de segurança;",
  "PETIÇÃO - Pedidos de concessão de efeito suspensivo a recursos;",
  "RECLAM - Ação de Reclamação;",
  "RECURSO - Recurso de Natureza administrativa (vinculado a Registros Públicos);",
  "REEXAME - Reexame necessário;",
  "RESP - Recurso especial;",
  "RESPAD - Recurso Especial interposo pela Administração Pública;",
  "REXT - Recurso extraordinário;",
  "RO - Recurso ordinário constitucional;",
  "SUSP - Pedido de efeito suspsensivo a recurso;",
  "TUTELA - Pedido de tutela de urgência."
  )


comment(distribuicao_df$natureza) <- c(
"ADJ.BENS  - Adjudicação de bens;",                         "ADOÇÃO  - Pedido de adoção;",                             
"AÈREA  - Reparação de danos contra mepresa aérea;",        "ALBC  - Alienação judicial de bens;",                     
"ALFIDUC  - Alienação fiduciária;",                         "ALIMENT  - Ação de alimentos;",                           
"ALIMPRO  - Alimentos provisórios;",                        "ALPAREN  - Alienação parental;",                         
"ALT.CAS  - Regime de bens entre os cônjuges;",             "ANUL.CAS  - Anulação de casamento;",                      
"ANULAT  - Ação anulatória de ato ou negócio jurídico;",    "ANULMAT  - Anulação de matrícula;",                       
"APOSSAM  - Ação de restabelecimento de pensão por morte;", "ARROLA  - Arrolamento de Bens;",                          
"ARROLAM  - Arrolamento de bens;",                          "ATO ADM  - Ato administrativo;",                          
"AUSÊNCI  - Declaração de ausência;",                       "BUSCAME  - Busca e apreensão de menor;",                  
"BUSCBEN  - Busca e apreensão de bem;",                     "CAUTELA  - Ação cautelar;",                               
"CO/VEN  - Contrato de compra e venda;",                    "COB.TXS  - Cobrança de taxas;",                           
"COBRANÇ  - Ação de cobrança;",                             "COBSEG  - Cobrança de seguro;",                           
"COMINAT  - Ação cominatória;",                             "CONCPRE  - Concordata preventiva;",                       
"CONCUB  - União estável ou concubinato;",                  "CONCUR  - Concurso público;",                             
"CONCURS  - Concurso Público;",                             "CONDOM  - Direito condominial;",                          
"CONSE  - Suprimento judicial de consentimento;",           "CONSIG  - Consignação em pagamento;",                     
"CONTR_A  - Cotnrato administrativo;",                      "CONVERS  - Conversão de separação em divórcio;",          
"CURATEL  - Direito de curatela;",                          "DECLARA  - Ação declaratória;",                           
"DEMOLIT  - Ação demolitória;",                             "DESAPR  - Ação de desapropriação;",                       
"DESPEJO  - Despejo por falta de pagamento;",               "DIR.OBR  - Direito das obrigações;",                      
"DISCRIM  - Bens públicos;",                                "DISSOLU  - Dissolução de união estável;",                 
"DIV.DEM  - Divisão e demarcação de terras;",               "DIVÓRCI  - Divórcio;",                                    
"DIVÓRCIO  - Ação de divórcio;",                            "DÚVIDA  - Dúvida registral;",                             
"EMBEXEC  - Embargo à execução;",                           "ESP INTERN  - Internação de idoso;",                      
"ESP MEDIC  - Fornecimento de medicamentos;",               "ESP SAÚDE  - Direito à saúde;",                           
"EXE.ALM  - Execução de alimentos;",                        "EXE.FISC  - Execução fiscal;",                            
"EXECAMB  - Execução tributária;",                          "EXECSEN  - Cumprimento ou execução de sentença;",         
"EXECUÇÃ  - Ação de execução;",                             "EXO.PEN  - Exoneração de alimentos;",                     
"FALÊNCI  - Direito falimentar;",                           "FALÊNCIA  - Ação de falência;",                           
"FAMILIA  - Direito de família;",                           "GUARDA  - Ação de guarda;",                               
"HAB.CRD  - Pedido de habilitação de crédito;",             "HABEAS  - Habeas data;",                                  
"IMISSÃO  - Ação de imissão na posse;",                     "IMPUG  - Pedido de impugnação de crédito;",               
"INDENIZ  - Ação de reparação de danos (indenizatória);",   "INSOLVÊ  - Declaração de Insolvência Civil;",             
"INTERD  - Ação de interdição;",                            "INTERN  - Internação compulsória;",                       
"INTPROIB  - Interdito proibitório;",                       "INVENTÁ  - Inventário ou alvará judicial;",               
"INVENTÁR  - Inventário;",                                  "INVESTIG  - Investigação de paternidade;",                
"IPVA  - IPVA;",                                            "LICIT  - Licitação pública;",                             
"LIQUID  - Liquidação judicial;",                           "LOTEAM  - Loteamento de terras;",                         
"MATERIN  - Investigação de maternidade;",                  "MEDIC  - Mdicamentos;",                                   
"MODCLÁ  - Modificação de multa cominatória;",              "MODCLÁU  - Modificação de cláusula de visitas;",          
"MODIF.G  - Modificação de guarda;",                        "MODIF.GD  - Modificação de guarda;",                      
"MONITÓR  - Ação monitória;",                               "NEG.PAT  - Ação negatória de paternidade;",               
"NUN.OBR  - Nunciação de obra nova;",                       "OBRIG_F  - Ação de obrigação de fazer;",                  
"OBRIG_FA  - Obrigação de Fazer;",                          "OUTRAS  - outras;",                                       
"PARTILHA  - Partilha de bens;,",                            "PATENTE  - Direito de Propriedade Industrial;",           
"PÁTRIO  - Destituição de poder familiar;",                 "PAULIANA  - Ação pauliana;",                              
"PED.FAL  - Pedido de falência;",                           "PED.REST  - Pedido de restituição de bens ou valores;",   
"PENSÃO  - Pensão por morte;",                              "PET.HER  - Petição de herança;",                          
"POSSE  - Direito de posse;",                               "PR.CONT  - Ação de prestação de contas;",                 
"PRESTSE  - Contrato de prestação de serviço;",             "PROTET  - Medida protetiva;",                             
"PROV  - Pedido de providências em registro público;",      "REC.FAT  - Reconhecimento de união de fato;",             
"RECU.JU  - Recuperação judicial;",                         "RECU.JUD  - Recuperação judicial;",                       
"REG.PUB  - Registro públicos;",                            "REGIMO  - Registro de imóveis;",                          
"REGISTR  - Registro públicos;",                            "REGUL.V  - Regulamentação de Visitas;",                   
"REIVIND  - Ação reivindicatória;",                         "RET.AREA  - Ação de retificação de área;",                
"RET.CIV  - Retificação de registro civil;",                "REVBEN  - Revisão de benefício;",                         
"REVISIO  - Revisional de alimentos;",                      "REVISION  - Ação revisional de alimentos;",               
"REVOCAT  - Ação revocatória;",                             "SAÚDE  - Direito à saúde;",                               
"SCORRES  - Sem correspondente;",                           "SEP.CON  - Ação de separação;",                           
"SEP.COR  - Separação de corpos;",                          "SEP.JUD  - Separação judicial;",                          
"SEP.LIT  - Separação litigiosa;",                          "SEQ.BEN  - Sequestro de Bens;",                           
"SERVID  - Servidor Público;",                              "SOC.ASS  - Direito societário ou associativo;",           
"SUPIDAD  - Suprimento de idade;",                          "SUSPROT  - Suspensão de protesto;",                       
"SVISITA  - Regime de visitas;",                            "TESTAM  - Registro de testamento;",                       
"TESTAME  - Cumprimento de testamento;",                    "TRIBMUN  - Tributos municipais;",                         
"TRIBUT  - Direito Tributário;",                            "TUTELA  - Tutela cível;",                                 
"UNIAO  - Reconhecimento de união estável;",                "USUCAP  - Ação de usucapião."
)


rm(distribuicao_padrao, distribuicao_dlc, multiplos_recursos)

save(distribuicao_df, file="Documentos/Distribuicao_df.RData")


##################################### FIM ######################################