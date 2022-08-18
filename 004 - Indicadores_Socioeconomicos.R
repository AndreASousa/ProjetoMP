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

# As informações quanto ao API do IBGE podem ser acessadas em:
# https://servicodados.ibge.gov.br/api/docs/agregados?versao=3#api-bq

#1 - População residente estimada
url = "https://servicodados.ibge.gov.br/api/v3/agregados/6579/periodos/2001|2002|2003|2004|2005|2006|2007|2008|2009|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021/variaveis/9324?localidades=N6[N3[35]]"

populacao <- GET(url) |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()  #Converte objetos jason para "r"

populacao <- populacao[[4]][[1]][[2]][[1]]
populacao <- cbind(populacao$localidade$id, populacao$serie)
populacao <- rename(populacao, "id" = "populacao$localidade$id")

save(populacao, file="Documentos/populacao.RData")

#2 - PIB
url = "https://servicodados.ibge.gov.br/api/v3/agregados/5938/periodos/2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019/variaveis/37|513|517|6575|525?localidades=N6[N3[35]]"

pib <- GET(url) |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()  #Converte objetos jason para "r"  
