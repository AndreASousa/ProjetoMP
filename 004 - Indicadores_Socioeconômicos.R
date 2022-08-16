################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "rvest", #Faz o Webscraping
             "httr", 
             "jsonlite",
             "dplyr", 
             "stringr") #Manipulação de expressões regulares

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
#                        Extração de dados do IBGE                             #
################################################################################

#As informações quanto ao API do IBGE podem ser acessadas em:
#https://servicodados.ibge.gov.br/api/docs/localidades#api-Municipios

#1 - Extraindo o conjunto de municípios do estado de Sâo Paulo
url = "https://servicodados.ibge.gov.br/api/v1/localidades/estados/35/municipios"

municipios_sp <- GET(url) |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()|>  #Converte objetos jason para "r"
  select(id, nome)

save(municipios_sp, file="Documentos/Municipios_SP.RData")

#2 - Extraindo o conjunto de distritos do município de Sâo Paulo
url = "https://servicodados.ibge.gov.br/api/v1/localidades/municipios/3550308/distritos"
sao_paulo <- GET(url) |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()  #Converte objetos jason para "r"  

save(sao_paulo, file="Documentos/Municipio_SP.RData")
