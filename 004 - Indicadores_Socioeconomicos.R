
## As informações sócioeconômicas extraidas a seguir poderão ser conectadas
  # ao restante do banco de dados pelo código de identificação do IBGE

################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "rvest", #Faz o Webscraping
             "httr", 
             "jsonlite",
             "tidyverse")

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
url  <-  "https://servicodados.ibge.gov.br/api/v3/agregados/6579/periodos/2001|2002|2003|2004|2005|2006|2007|2008|2009|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021/variaveis/9324?localidades=N6[N3[35]]"

populacao <- GET(url) |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()  #Converte objetos jason para "r"

extrator <- function(objeto = NULL){
  
  extraido <- objeto[[4]][[1]][[2]][[1]]
  extraido <- cbind(extraido$localidade$id, extraido$serie)
  
  if(!is.null(extraido$localidade$id)){
    extraido <- rename(extraido, "id" = "extraido$localidade$id")
    }
  
  return(extraido)
}

populacao <- extrator(populacao)

save(populacao, file="Documentos/populacao.RData")

#2 - PIB a preços correntes
url  <-  "https://servicodados.ibge.gov.br/api/v3/agregados/5938/periodos/2010|2011|2012|2013|2014|2015|2016|2017|2018|2019/variaveis/37?localidades=N6[N3[35]]"

pib <- GET(url) |>
  getElement("content") |>
  rawToChar() |>
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>
  fromJSON()|>
  extrator()

save(pib, file="Documentos/pib.RData")

#3 - Total de pessoas ocupadas
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1685/periodos/2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020/variaveis/707?localidades=N6[N3[35]]"

ocupados <-  GET(url) |>
  getElement("content") |> 
  rawToChar() |> 
  iconv(to = "latin1//TRANSLIT", from = "UTF-8")  |> 
  fromJSON() |>
  extrator()

save(ocupados, file="Documentos/ocupados.RData")

#4 - Salário médio mensal
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1685/periodos/2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020/variaveis/707?localidades=N6[N3[35]]"

salario <-  GET(url) |>
  getElement("content") |> 
  rawToChar() |> 
  iconv(to = "latin1//TRANSLIT", from = "UTF-8")  |> 
  fromJSON() |>
  extrator()



save(salario, file="Documentos/salario.RData")

#5 - IDH Municipios 2010
url  <-  "https://servicodados.ibge.gov.br/api/v1/pesquisas/37/periodos/2010/indicadores/30255/resultados/35xxxx?scope=sub&pt"

idh <- GET(url) |>
  getElement("content") |> 
  rawToChar() |> 
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>
  fromJSON() 

idh <- idh[[2]][[1]]
idh <- cbind(idh[1], idh$res$'2010')
idh$idh <- idh$`idh$res$"2010"`
idh$`idh$res$"2010"` <- NULL

save(idh, file="Documentos/idh.RData")

#6 - Faixas Estárias
  # Utilizaremos as estimativas do Ministério da Saúde e disponíveis em:
  # http://tabnet.datasus.gov.br/cgi/tabcgi.exe?popsvs/cnv/popbr.def
  # Não foi possível a extrassão automatizada dos dados
  # Extraimos as informações referentes aos anos de 2010 e 2021 em arquivos "csv"

etario <- list.files("Documentos", pattern = "csv$",
                                 recursive = T, full.names = T)

# Verificando a ordem em que cada lista foi baixada
etario

# Convertendo cada arquivo em um data frame aninhado
etario <- lapply(etario, read.csv, sep = ";")

# Tendo em vista que cada df aninhado segue a ordem 2010:2021,
# Podemos uni-los, realizar as modificações e spearalos a cada conjunto de 
# 645 observações (o número de municípios de São Paulo)

etario <- bind_rows(etario)

nrow(etario) / 645 

# Os códigos de Município não correspondem exatamente aos do IBGE
# Falta o último dígito
load(here::here("Documentos", "codigos_sp.RData"))

etario$codigo <- str_sub(etario$Município, end = 6)

municipios_sp$codigo <- str_sub(municipios_sp$id, end = 6)

length(unique(municipios_sp$codigo))

etario <- left_join(etario, municipios_sp[ , c("id", "codigo")], by = "codigo") |>
          select(id, !c(codigo, Município))

count(filter(etario, is.na(id)))

save(etario, file="Documentos/etario.RData")

#7 - Menores de 25 anos

  # Queremos estudar como o envelhecimento da população tem afetado
  # A natureza dos processos ajuizados

  # A hipótese é que o número de ações de alimento tem caído à medida que 
  # O número de menores de 25 anos cai (idade limite em que o direito é assegurado aos filhos)

  # Por outro lado, talvez as ações de saúde aumentem conforme o número de idosos aumenta

# Somando o número de pessoas menores de 25 em cada município
menor_25 <- etario |>
  mutate(menor25 = rowSums(etario[, 2:6])) |>
  select(id, menor25)

# Criando uma coluna que indica o ano em que cada estimativa foi feita
menor_25$ano <- as.character(rep(2010:2021, each = 645))

# Agrupando as observações de cada ano em uma coluna própria
menor_25 <- tidyr::pivot_wider(menor_25, names_from = ano, values_from = menor25)

# Verificando a consisTência dos dados
filter_all(menor_25, any_vars(is.na(.)))

save(menor_25, file="Documentos/menor_25.RData")

#8 - Maiores de 60 anos

# Somando o número de pessoas maiores de 60 em cada município
maior_60 <- etario |>
  mutate(maior60 = rowSums(etario[, 14:18])) |>
  select(id,maior60)

# Criando uma coluna que indica o ano em que cada estimativa foi feita
maior_60$ano <- as.character(rep(2010:2021, each = 645))

# Agrupando as observações de cada ano em uma coluna própria
maior_60 <- tidyr::pivot_wider(maior_60, names_from = ano, values_from = maior60)

# Verificando a consisTência dos dados
filter_all(maior_60, any_vars(is.na(.)))

save(maior_60, file="Documentos/maior_60.RData")

