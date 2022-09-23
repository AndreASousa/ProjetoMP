
## O Script se destina à criaçao de uma tabela que agregue informações
  # socioeconômicas de cada Comarca paulista, a cada ano.

################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "rvest", #Faz o Webscraping
             "httr", 
             "jsonlite",
             "tidyverse",
             "readxl")

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
#                        Extração de Dados Socioeconômicos                     #
################################################################################

load(here::here("Documentos", "municipios_sp.RData"))

# As informações quanto ao API do IBGE podem ser acessadas em:
# https://servicodados.ibge.gov.br/api/docs/agregados?versao=3#api-bq

#1 - População residente (Não compreende 2007 e 2010)
url  <-  "https://servicodados.ibge.gov.br/api/v3/agregados/6579/periodos/2001|2002|2003|2004|2005|2006|2008|2009|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021/variaveis/9324?localidades=N6[N3[35]]"

populacao <- GET(url) |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()  #Converte objetos jason para "r"


# Criamos uma função para facilitar a extração da lista aninhada resultante da
  # raspagem de dados
extrator <- function(objeto = NULL){
  
  extraido <- objeto[[4]][[1]][[2]][[1]]
  extraido <- cbind(extraido$localidade$id, extraido$serie)
  
  #Renomeamos a coluna qye contém o código IBGE como "id"
  if("extraido$localidade$id" %in% colnames(extraido)){
    extraido <- rename(extraido, "id" = "extraido$localidade$id")
    }
  
  return(extraido)
}

populacao <- extrator(populacao)

#2 - População Residente - Censo 2010
url  <-  "https://servicodados.ibge.gov.br/api/v3/agregados/202/periodos/2010/variaveis/93?localidades=N6[N3[35]]&classificacao=2[0]|1[0]"

populacao_2010 <- GET(url) |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()|>  #Converte objetos jason para "r"
  extrator()

populacao <- left_join(populacao, populacao_2010, by = "id")
rm(populacao_2010)

#3 - Contagem Populacional 2007
url  <-  "https://servicodados.ibge.gov.br/api/v3/agregados/793/periodos/2007/variaveis/93?localidades=N6[N3[35]]"

populacao_2007 <- GET(url) |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()|>  #Converte objetos jason para "r"
  extrator()

populacao <- left_join(populacao, populacao_2007, by = "id")
rm(populacao_2007)

# Verificando a consisTência dos dados
filter_all(populacao, any_vars(is.na(.)))

# Vamos Converter os dados para a estrutura de uma série temporal
# As observações de cada ano serão dispostas em uma mesma linha, ao invés de colunas 

organiza <- function(objeto = NULL){
  
  # Capturando o nome do objeto submetido à função
  nome_objeto <-deparse(substitute(objeto))
  
  objeto <- objeto |>
    pivot_longer(cols = !id, names_to = "ano",
                 # A coluna criada recebe o nome do objeto
                 values_to = nome_objeto) |>
    arrange(ano, id)
  
  # Converte para valores numéricos
  var_classe <- c("ano", nome_objeto)
  objeto[var_classe] <- lapply(objeto[var_classe], as.numeric)
  
  return(objeto)
}

populacao <- organiza(populacao)

save(populacao, file="Documentos/populacao.RData")


#4 - PIB Municipal a preços correntes (Série de 2002 a 2019)
url  <-  "https://servicodados.ibge.gov.br/api/v3/agregados/5938/periodos/2002|2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019/variaveis/37?localidades=N6[N3[35]]"

pib <- GET(url) |>
  getElement("content") |>
  rawToChar() |>
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>
  fromJSON()|>
  extrator()

pib <- organiza(pib)

save(pib, file="Documentos/pib.RData")

#5 - Total de pessoas ocupadas - Cadastro Central de Empresas
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1685/periodos/2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020/variaveis/707?localidades=N6[N3[35]]"

ocupados <-  GET(url) |>
  getElement("content") |> 
  rawToChar() |> 
  iconv(to = "latin1//TRANSLIT", from = "UTF-8")  |> 
  fromJSON() |>
  extrator()

ocupados <- organiza(ocupados)

save(ocupados, file="Documentos/ocupados.RData")

#6 - Salário médio mensal - Cadastro Central de Empresas
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1685/periodos/2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019|2020/variaveis/1606?localidades=N6[N3[35]]"

salario <-  GET(url) |>
  getElement("content") |> 
  rawToChar() |> 
  iconv(to = "latin1//TRANSLIT", from = "UTF-8")  |> 
  fromJSON() |>
  extrator()

salario <- organiza(salario)

save(salario, file="Documentos/salario.RData")

#7 - Número de divórcios concedidos em 1ª instância a casais com filhos menores de idade
  # Pesquisa Estatísticas do Registro Civil
url  <-  "https://servicodados.ibge.gov.br/api/v3/agregados/5936/periodos/2014|2015|2016|2017|2018|2019|2020/variaveis/235?localidades=N6[N3[35]]"

divorcios <- GET(url) |>
  getElement("content") |>
  rawToChar() |>
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>
  fromJSON()|>
  extrator()

divorcios <- organiza(divorcios)
divorcios[is.na(divorcios)] <- 0

save(divorcios, file="Documentos/divorcio.RData")

#8 - Número de filhos menores de idade de casais envolvidos em divórcios
  # condedidos em 1ª Instância
url  <-  "https://servicodados.ibge.gov.br/api/v3/agregados/5936/periodos/2014|2015|2016|2017|2018|2019|2020/variaveis/6565?localidades=N6[N3[35]]&classificacao=737[0]"

n_filhos <- GET(url) |>
  getElement("content") |>
  rawToChar() |>
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>
  fromJSON()|>
  extrator()

n_filhos <- organiza(n_filhos)
n_filhos[is.na(n_filhos)] <- 0

save(n_filhos, file="Documentos/n_filhos.RData")

#9- IDH Municipios 2010
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
idh$idh <- as.numeric(idh$idh)

# Os códigos de Município não correspondem exatamente aos do IBGE
# Falta o último dígito
municipios_sp$codigo <- str_sub(municipios_sp$id, end = 6)

# A exclusão do último dígito não gera duplicidade de dados
length(unique(municipios_sp$codigo))

idh <- idh |>
  left_join(municipios_sp[ , c("id", "codigo")], by = c("localidade" = "codigo")) |>
  dplyr::select(id, idh)

# O procedimento conseguiu vincluar todas as informações ao identificador do IBGE
count(filter(idh, is.na(id)))

save(idh, file="Documentos/idh.RData")

#10 - Faixas Estárias

  # Utilizaremos as estimativas do Ministério da Saúde e disponíveis em:
  # http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/popsvsbr.def
  # Não foi possível a extrassão automatizada dos dados
  # Extraimos as informações referentes aos anos de 2010 e 2021 em arquivos "csv"

etario <- list.files("Documentos/Estimativa_Etaria", pattern = "csv$",
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

# Vamos vincular cada observação ao respectivo ano
etario$ano <- as.character(rep(2006:2021, each = 645))

# Os códigos de Município não correspondem exatamente aos do IBGE
# Falta o último dígito

etario$codigo <- str_sub(etario$Município, end = 6)

etario <- left_join(etario, municipios_sp[ , c("id", "codigo")], by = "codigo") |>
          dplyr::select(id, !c(codigo, Município))

# O procedimento conseguiu vincluar todas as informações ao identificador do IBGE
count(filter(etario, is.na(id)))

# Existe divergência entre população residente total estimada pelo IBGE e
 # pelo Ministério da Saúde. Optamos pelos dados do IBGE.
etario <- etario|>
  arrange(ano, id) |>
  dplyr::select(id, ano, everything(), -Total)

# Simplificando os nomes das variáveis
# É necessário que cada uma se inicie com um caractere não numérico, para
# evitar conflito com algumas funções. Ex. ols_vif_tol()

colnames(etario) <- c("id", "ano", "f0a4", "f5a9", "f10a14", "f15a19", "f20a24",
                      "f25a29", "f30a34", "f35a39", "f40a44", "f45a49", "f50a54",
                      "f55a59", "f60a64", "f65a69", "f70a74", "f75a79", "f80mais")

#Converte para valores numéricos
var_classe <- colnames(etario[ , -1])
etario[var_classe] <- lapply(etario[var_classe], as.numeric)

save(etario, file="Documentos/etario.RData")


#11 - PIB municipal - 2020 e 2021

# Utilizaremos as estimativas da Fundação Sistema Estadual de Análise de Dados de São Paulo (SEADE)
# Aplicaremos o incremento percentual anual no PIB do Estado de São Paulo para estimar os valores
# a seus municípios constituíntes.

url <- "https://repositorio.seade.gov.br/dataset/90ae5a90-7638-409c-be21-c3049daa4a55/resource/abf25069-ceeb-4115-8945-e0c4d0508588/download/05-2022_tabelas_site.xlsx"
  
GET(url = url, write_disk(path = "Documentos/pib_seade.xlsx", overwrite = TRUE))

# Desejamos extrair as onformações constantes da planilha nº 9
excel_sheets(here("Documentos", "pib_seade.xlsx"))

pib_seade <- as.data.frame(read_excel(here("Documentos", "pib_seade.xlsx"),
                           sheet = "Mensal 09"))

# Selecionando as colunas correspondentes ao mês, ano e PIB
pib_seade <- pib_seade[ , c(1, 2, 7)] |>
  rename("mes" = 1, "ano" = 2, "pib" = 3) |>
  # Selecionando a diferença percentual do PIB dezembro a dezembro
  filter(ano %in% c("2020", "2021"), mes == "Dezembro")

# Convertendo valores de strings para numéricos
pib_seade$pib <- as.numeric(pib_seade$pib)

pib_2020 <- pib |>
  filter(ano == 2019)|>
  mutate(pib = round(pib * (1 + pib_seade$pib[1]/100), 0)) |>
  mutate(ano = 2020)

pib_2021 <- pib_2020|>
  mutate(pib = round(pib * (1 + pib_seade$pib[2]/100), 0)) |>
  mutate(ano = 2021)

pib <- rbind(pib, pib_2020, pib_2021)

rm(pib_seade, pib_2020, pib_2021)

save(pib, file="Documentos/pib.RData")


################################################################################
#                            Aglutinando Dados                                 #
################################################################################

#1 - Agrupando os indicadores em uma mesma tabela
indicadores <- populacao |>
  filter(ano %in% 2014:2020) |>
  left_join(municipios_sp[ , c("id", "comarca")], by = "id") |>
  left_join(pib, by = c("id",  "ano")) |>
  left_join(salario, by = c("id",  "ano")) |>
  left_join(ocupados, by = c("id",  "ano")) |>
  left_join(idh, by = "id") |>
  left_join(divorcios, by = c("id",  "ano")) |>
  left_join(n_filhos, by = c("id",  "ano")) |>
  left_join(etario, by = c("id",  "ano")) |>
  dplyr::select(id, comarca, everything())

indicadores$divorcios[is.na(indicadores$divorcios)] <- 0
indicadores$n_filhos[is.na(indicadores$n_filhos)] <- 0

# Verificando a consistência dos dados
filter_all(indicadores, any_vars(is.na(.)))

#2 -  Calculando os indicadores para cada ano / comarca
indicadores <- indicadores |>
  group_by(ano, comarca)|> 
  summarise(
    # Definindo os valores de idh e salário médio das comarcas
    # pela média ponderada com a populacao de cada cidade em cada ano
    salario = weighted.mean(salario, populacao),
    idh = weighted.mean(idh, populacao),
    # Aplicando a função sum() sobre variáveis que representam
    # Números absolutos
    across(-c(id, salario, idh), sum)) |>
  ungroup () %>% droplevels(.)



#3 - Convertendo o PIB nominal em PIB per capta
indicadores <- indicadores |>
  mutate(pib = round(pib / populacao, 2)) |>
  rename(pib_pc = pib)

save(indicadores, file="Documentos/indicadores.RData")

summary(indicadores)

