
## O propósito deste "script" consiste em adicionar novos dados à base de dados
# já tratada

# Basta adicionar novas listas em formato pdf na pasta:
# PDFs/Distribuicao - Novos Dados

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
#"pattern = 'pdf$' para que apenas arquivos do tipo PDF seham listados
# "recurse = T" para que a função procure PDFs contidos em subpastas
# "full.names = T" para que a lista armazene o caminho até cada arquivo.
# Caso contrário, as demais funções não serão capazes de encontrar os arquivos
# em suas respectivs subpastas

lista_distribuicao_nd <- list.files("PDFs/Distribuicao - Novos Dados", pattern = "pdf$",
                                 recursive = T, full.names = T)


## Cria um arquivo PDF único - Facilita a identificação de erros
#A extração sequecial dos arquivos com lapply() retorna erros

pdf_combine(input = iconv(lista_distribuicao_nd, to = "latin1//TRANSLIT", from = "UTF-8"),
            output = "PDFs/Distribuicao_New_Data.pdf")
rm(lista_distribuicao_nd)


## Cria uma lista de data frames a partir de cada página do pdf
distribuicao_dl_nd <- pdf_data("PDFs/Distribuicao_New_Data.pdf") |>
                    lapply(select, -c(width, height, space)) |>
                    lapply(arrange, "y") |>
                    lapply(spread, x, text) |>
                    lapply(select, -c(y)) |>
                    lapply(unite, "Linha", sep = " ", na.rm = T)
                      

## Cada sublista será nomeada conforme o número de página que assumirá em
# "Distribuicao_Consolidada.pdf"

load(here::here("Documentos", "Distribuicao_df.RData"))
names(Distribuicao_dl_nd) <- seq_along(distribuicao_dl_nd) + max(distribuicao_df$Pagina)

## Desejamos manter apenas as páginas que contenham pelo menos um número de processo

distribuicao_dl_nd <- purrr::keep(distribuicao_dl_nd, ~any(str_detect(.x$Linha, "^\\d{7}.+")))

##Assegurando que cada processo esteja vinculado ao respectivo cargo
#(Continuações de página)
continuacao <- purrr::keep(distribuicao_dl_nd, ~all(!str_detect(.x$Linha,
                                                             "Distribu.+a\\(o\\) Dr\\(a\\)\\..*")))
n_iteracoes = length(distribuicao_dl_nd)-length(continuacao)+1
n = 1
while(n < n_iteracoes){
  if(all(!str_detect(distribuicao_dl_nd[[n]]$Linha,"Distribu.+a\\(o\\) Dr\\(a\\)\\..*"))){
    distribuicao_dl_nd[[n]]$Linha <-  str_remove(distribuicao_dl_nd[[n]]$Linha,"Data da Dis.+")
    distribuicao_dl_nd[[n-1]] <- bind_rows(distribuicao_dl_nd[[n-1]], distribuicao_dl_nd[[n]])
    distribuicao_dl_nd[[n]] <- NULL
    n <-  n - 1
  }
  n <- n + 1
}

rm(continuacao, n, n_iteracoes)

##Removendo Distribuições de Ciência

ciencia <- purrr::keep(distribuicao_dl_nd, ~any(str_detect(.x$Linha,
                                                         ".*Processos - CIÊNCIA")))
n_iteracoes = length(distribuicao_dl_nd)-length(ciencia)+1
n = 1
while(n < n_iteracoes){
  if(any(str_detect(distribuicao_dl_nd[[n]]$Linha, ".*Processos - CIÊNCIA"))){
    distribuicao_dl_nd[[n]] <- NULL
    n <-  n - 1
  }
  n <- n + 1
}

rm(ciencia, n, n_iteracoes)

##Identificando listas nº distribuidas a um cargo específico

for(n in 1:length(distribuicao_dl_nd)){
  if(all(!str_detect(distribuicao_dl_nd[[n]]$Linha,"^\\d{2}º Procurador.+"))){
    
    distribuicao_dl_nd[[n]]$Linha[6] <- "99º Procurador de Justiça da Proc de Justiça Cível"
  } 
}


rm(n, outros)
save(distribuicao_dl_nd, file="Documentos/distribuicao_dl_nd.RData")

load(here::here("Documentos", "distribuicao_dl_nd.RData"))

################################################################################
#                    EXTRAÇÃO DE VARIÁVEIS RELEVANTES                          #
################################################################################

distribuicao_df_nd <- distribuicao_dl_nd |>
  map_dfr(~ .x, .id = "Pagina") |> 
  mutate(Pagina = as.numeric(Pagina)) |>
  group_by(Pagina) |>
  summarise(Cargo = str_replace(Linha[str_detect(Linha,
                    "(^\\d{2})º Procurador.+")],
                    "(^\\d{2})º Procurador.+", "\\1") |>
                     as.factor(),
            
            Processo = str_replace(Linha[str_detect(Linha,
                     "(^\\d{7}.+?\\d{4}.+?\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                     "(^\\d{7}.+?\\d{4}.+?\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1"),
            
            Propositura = str_replace(Linha[str_detect(Linha,
                      "^\\d{7}.+?(\\d{4}).+?\\d{4}.+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                      "^\\d{7}.+?(\\d{4}).+?\\d{4}.+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1") |>
                      as.numeric(),
            
            Codigo = str_replace(Linha[str_detect(Linha,
                      "^\\d{7}.+?\\d{4}.+?(\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                      "^\\d{7}.+?\\d{4}.+?(\\d{4}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1"),

            Tribunal = str_replace(Linha[str_detect(Linha,
                      "^\\d{7}.+?\\d{4}\\.(\\d{1}\\.\\d{2}).+\\d{4}.+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                      "^\\d{7}.+?\\d{4}\\.(\\d{1}\\.\\d{2}).+?[:upper:]{3,}+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1"), 

            Digital = str_replace(Linha[str_detect(Linha,
                      "^\\d{7}.+?\\d{4}.+?\\d{4}.+?([:upper:]{3,}+).+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+")],
                      "^\\d{7}.+?\\d{4}.+?\\d{4}.+?([:upper:]{3,}+)+.+?[:upper:]+.+?[:upper:]+.+?[:upper:]+.+?\\s+\\d+", "\\1") |>
                      as.factor(),
            
            Tipo = str_replace(Linha[str_detect(Linha,
                      "^\\d{7}.+?\\d{4}.+?\\d{4}.+?[:upper:]{3,}.[:digit:]+.*?([:upper:]+[^Não|^Sim]).+\\s+\\d+")],
                      "^\\d{7}.+?\\d{4}.+?\\d{4}.+?[:upper:]{3,}.[:digit:]+.*?([:upper:]+[^Não|^Sim]).+\\s+\\d+", "\\1") |>
                      as.factor(), 
            
            Natureza = str_replace(Linha[str_detect(Linha,
                      "^\\d{7}.+?\\d{4}.+?\\d{4}.+?[:upper:]{3,}+.[:digit:]+.*?[:upper:]+\\S+.+?[:upper:]+.+?([:upper:]{2,}\\D*).*\\s+\\d+$")],
                      "^\\d{7}.+?\\d{4}.+?\\d{4}.+?[:upper:]{3,}+.[:digit:]+.*?[:upper:]+\\S+.+?[:upper:]+.+?([:upper:]{2,}\\D*).*\\s+\\d+$", "\\1")|>
                      as.factor(),  
            
            Data = first(str_replace(Linha[str_detect(Linha,
                      "^.*\\d{2}/\\d{2}/\\d{2}$")],
                      "^.*(\\d{2}/\\d{2}/\\d{2})$", "\\1")) |>
                      as.Date("%d/%m/%y"),

            .groups = "drop")|>
  select(Cargo, Processo, Propositura, Codigo, Tribunal, Digital, Tipo, Natureza, Data, Pagina)|>
  arrange(Data, Pagina, Cargo)

rm(distribuicao_dl_nd)

# Unindo à base de dados original

load(here::here("Documentos", "Distribuicao_df.RData"))
distribuicao_df <- rbind(distribuicao_df, distribuicao_df_nd)

# Removendo duplicidade de dados

duplicados <- distribuicao_df[duplicated(distribuicao_df[, -9]),]
distribuicao_df <- anti_join(distribuicao_df, duplicados)
rm(duplicados)

save(distribuicao_df, file="Documentos/Distribuicao_df.RData")

