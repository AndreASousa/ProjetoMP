
## O Script se destina à criação de duas bases de dados:
  # 1. A relação dos códigos de unidades de origem e respectivas comarcas e municípios
  # 2. A relação de municípios dos estado de São Paulo e a comarca a que pertencem

## todos os números de processo devem observar a mesma estrutura:
  # “NNNNNNN-DD.AAAA.J.TR.OOOO”, se traduzindo em:
  # 7 números sequenciais do processo (NNNNNNN);
  # 2 dígitos verificadores (DD);
  # o ano de ajuizamento (AAAA);
  # o órgão em que foi ajuizado (J);
  # o Tribunal ou Circunscrição Militar (TR); e
  # A unidade de origem dentro da estrutura administrativa dos Tribunais (OOOO).

## A unidade de origem (OOOO) permite aferir em que localidade do estado de São Paulo
  # o feito foi ajuizado.

## Um mesmo município pode ser representado por mais de uma Unidade de Origem
  # ex. A Comarca da Capital é representada diversos códigos a depender da região,
  # distrito ou mesmo assuno.

## Por outro lado, uma mesma unidade de origem pode representar mais de um município
  # ex. o município de Águas da Prata pertence à comarca de São João da Boa Vista

## Como pretendemos analisar os processos à luz de indicadores socioeconômicos
  # Precisamos vincular cada unidade de origem a um ou mais municípios


################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "readxl", #Leitura de Planilhas do Excel
             "rvest", #Faz o Webscraping
             "httr",
             "dplyr",
             "tidyverse",
             "jsonlite",
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
#                   VINCULANDO CADA MUNICÍPIO A UMA COMARCA                    #
################################################################################

## A a lista com unidades de origem (OOOO) pode ser obtida na página do CNJ
  # no seguinte endereço: "https://www.cnj.jus.br/wp-content/uploads/2011/02/foros-1.xls"
  # Contudo, a relação é datada de 2011 e se encontra desatualizada.

  # Complementaremos as informações por meio do cruzamento com 
  # a relação de comarcas do Tribunal de Justiça:
  # "https://www.tjsp.jus.br/QuemSomos/QuemSomos/RegioesAdministrativasJudiciarias"

  # Como uma mesma comarca pode possuit mais de uma unidade judiciária,
  # Cruzaremos a nova relação com a lista de processos distribuidos a fim
  # de identifucar os códigos de origem faltantes


#1 - Extraindo o conjunto de municípios do estado de Sâo Paulo

municipios_sp <- GET("https://servicodados.ibge.gov.br/api/v1/localidades/estados/35/municipios") |>
  getElement("content") |> #Selecionando "$content"
  rawToChar() |> #Transforma o conteúdo em um texto
  iconv(to = "latin1//TRANSLIT", from = "UTF-8") |>#Modifica a codificação para "UTF-8"
  fromJSON()|>  #Converte objetos jason para "r"
  select(id, nome)

#2 - Extraindo o conjunto de comarcas Paulista

comarcas <- rvest::read_html("https://www.tjsp.jus.br/QuemSomos/QuemSomos/RegioesAdministrativasJudiciarias") |>
  rvest::html_elements(".list-group-item") |> # "." para selecionar "classes"
  rvest::html_text()

comarcas <- stringr::str_replace(comarcas[str_detect(comarcas, ".+\\s-.+")],
                                 "(.+)\\s-.+", "\\1")  |>
  as.data.frame() 

colnames(comarcas) <- "comarca"


#3 - Identificando as sedes de Comarca

sedes_de_comarca <- inner_join(municipios_sp, comarcas, by = c("nome" ="comarca"))

# Note que as "sedes_de_comarcas" possui um número de observações inferior ao
# Total de comarcas do TJSP
# Isso ocorre por diferença de grafia

grafia <- anti_join(comarcas, sedes_de_comarca, by = c("comarca" = "nome"))
grafia <- grafia[, 1]

# Vila Mimosa não é uma comarca, mas um Fórum Regional da Comarca de Campinas

comarcas <- filter(comarcas, comarca != "Vila Mimosa") |>
  arrange(comarca)

# Vamos adotar a grafia do IBGE
nova_grafia <- as.data.frame(
              c("Carapicuíba", "Cerqueira César", "Estrela d'Oeste", 
                 "Palmeira d'Oeste", "Rio Grande da Serra", "Santa Bárbara d'Oeste",
                 "Santana de Parnaíba", "São Luiz do Paraitinga",
                 "Santa Rosa de Viterbo"))
              
colnames(nova_grafia) <- "comarca"

comarcas <- filter(comarcas, !comarca %in% grafia)
comarcas <- rbind(comarcas, nova_grafia)

sedes_de_comarca <- inner_join(municipios_sp, comarcas, by = c("nome" ="comarca"))
sedes_de_comarca$comarca <- sedes_de_comarca$nome

rm(nova_grafia, grafia)

#4 - Vinculando os demais Municípios paulistas à respectiva comarca

municipios_faltantes <- anti_join(municipios_sp, comarcas, by = c("nome" ="comarca"))

# É possível identificar a comarca a que cada município é vinculado
# pelo endereço: https://www.tjsp.jus.br/ListaTelefonica

# Infelizmente, o seguinte método apenas funciona para municípios que não sejam
# sede de comarda, obrigando o passo acima.

# A página segue o formato de formulário e remete a outros URLs
# Utilizamos as ferramentas de desenvolvedor do Chrome para acompanhar o
# tráfego de rede ("F12") e o corpo da mensagem desejado ("Payload) 

busca_comarca <- function(municipio){
  
  corpo <- list(texto = municipio)
  
  busca_codigo <- httr::POST(url = "https://www.tjsp.jus.br/AutoComplete/ListarMunicipios",
                             body = corpo,
                             encode = "form",
                             httr::accept("text/html; charset=latin1;")) |>
    httr::content("parse")
  
  corpo <- list(parmsEntrada = busca_codigo[[1]][["Codigo"]],
                codigoTipoBusca = "1")
  
  busca_foro <- httr::POST(url = "https://www.tjsp.jus.br/ListaTelefonica/RetornarResultadoBusca",
                           body = corpo,
                           encode = "form",
                           httr::accept("text/html; charset=latin1;")) |>
    httr::content("parse") |>
    rvest::html_elements("h4") |>
    rvest:: html_text()
  
  comarca <- stringr::str_replace(busca_foro[str_detect(busca_foro, ".+à comarca\\s+(.+)")],
                                  ".+à comarca\\s+(.+)", "\\1")
  
  Sys.sleep(0.2)
  
  return(comarca)
  
}

# Intentamos o uso da função: 
  # unicipios_faltantes <- sapply(municipios_faltantes$nome, busca_comarca)
  # Contudo, a diferença de grafia entre os registros do IBGE e TJSP
  # Resulta no erro "subscript out of bounds"

  # A solução foi o empreo de map() em conjunto com possibly()
  # possibly() captura o erro e o converte em NA

municipios_faltantes$comarca <- map(municipios_faltantes$nome, possibly(busca_comarca, NA))

#Mais uma vez, existe diferença de grafia entre o IBGE e o TJSP
filter(municipios_faltantes, is.na(comarca))

revisado <- c("Biritiba Mirim", "Florínea")

municipios_faltantes$comarca[municipios_faltantes$nome %in% revisado] <- c("Mogi das Cruzes", "Assis")

municipios_faltantes$comarca <- as.character(municipios_faltantes$comarca)

# "Lindoia" e "Uru" escapam da pesquisa
revisado <- c("Lindóia", "Uru")
municipios_faltantes$comarca[municipios_faltantes$nome %in% revisado] <- c("Águas de Lindóia", "Pirajuí")

#  A Comarca de Estrela d'Oeste é grafada como "Estrela dOeste pelo sistema do TJSP

municipios_faltantes$comarca <- gsub("Estrela dOeste",  "Estrela d'Oeste", municipios_faltantes$comarca )

municipios_sp <- rbind(sedes_de_comarca, municipios_faltantes)|>
  arrange(nome)

save(municipios_faltantes, file="Documentos/municipios_faltantes.RData")
save(municipios_sp, file="Documentos/municipios_sp.RData")

rm(revisado, municipios_faltantes, sedes_de_comarca, busca_comarca)


################################################################################
#                   VINCULAÇNDO CADA CÓDIGO A UMA COMARCA                     #
################################################################################



#5 -  Vinculando os Códigos de Unidade Judiciária a cada Comarca

httr::GET(url = "https://www.cnj.jus.br/wp-content/uploads/2011/02/foros-1.xls",
          write_disk(path = "Documentos/foros_sp.xls", overwrite = TRUE))
                    
foros <- as.data.frame(read_excel(here::here("Documentos", "foros_sp.xls"))) |>
        rename(Codigo = Código, Descr = Descrição)


##Adicionando "Leading Zeroes"
  #Cada código de unidade judiciária possui 4 números (OOOO), sendo necessário
  #adicionar zeros no iício
foros <- foros |>
         mutate(Codigo = str_pad(Codigo, 4, pad = "0"),
         #Criando uma nova coluna
         Local = Descr)

foros <- foros |>
      mutate(Local = gsub(".+Regional.+-\\s+.+", "São Paulo", Local)) |>
      mutate(Local = gsub(".+Regional de.+", "Campinas", Local)) |>
      #Apenas São Paulo e Campinas possuem Foros Regionais
      #Vila Mimosa é o único foro regional pertencente a Campinas 
      mutate(Local = gsub(".+Central.+", "São Paulo", Local)) |>
      mutate(Local = gsub("Foro das Execuções Fiscais.+", "São Paulo", Local)) |>
      mutate(Local = gsub("Foro Especial da Infância e Juventude", "São Paulo", Local)) |>
      mutate(Local = gsub("Setor de Cartas Precatórias Cíveis - Cap", "São Paulo", Local)) |>
      mutate(Local = gsub("Foro de\\s+(.+)", "\\1", Local))|>
      mutate(Local = case_when(Comarca == "São Paulo" & grepl("Distrital", foros$Local) ~ "São Paulo",
                            TRUE  ~ Local)) |> #Se a Comarca for São Paulo e For um Local Distrital -> "São Paulo"
      mutate(Local = gsub(".+Distrital de\\s+(.+)", "\\1", Local))




teste <- dplyr::right_join(uni_jud, comarcas, by = c("Local" ="Comarca"))

#Birigui - "Uso de Tremas"
#Carapicuiba - "Acentuação diferente"
#Cerqueira Cesar - "Acentuação diferente"
#Eldorado - Registrado como "Eldorado Paulista" para distinguir de cidade homônima
#Embu das Artes - Registrado como "Local de Embu"
#Estrela D’Oeste -  Distinçao ente ' e ´
#Ipaussu - Registrado como Ipauçu o nome atuautliza "ss"
#Palmeira D’Oeste -  Distinçao ente ' e ´
#Rio Grande de Serra - Uso de "de Serra" em lugar de "da Serra"
#Santa Bárbara D’Oeste - Distinçao ente ' e ´
#Santa Rosa do Viterbo - Uso de "de Viderbo" em lugar de "de Viterbo"
#Vila Mimosa - Trata-se de Local Regional de Campinas

revisado <- data.frame(embu_d_a <- c("0176", "Local de Embu das Artes", "Embu das Artes", "Embu das Artes"),
                  ipaussu <- c("0252", "Local de Ipaussu", "Ipaussu", "Ipaussu"),
                  santana_d_p <- c("0529", "Local de Santana de Parnaíba", "Santana de Parnaíba", "Santana de Parnaíba"),
                  row.names = c("Codigo", "Descr", "Comarca", "Local"))|>
                  t()
row.names(revisado) <- NULL

foros <- foros |>
         filter(!(Codigo %in% c("0176", "0252","0529"))) |>
         rbind(revisado) |>
         arrange(Codigo)

rm(revisado, comarcas, embu_d_a, ipaussu, santana_d_p)
n_distinct(foros$Local)


save(foros, file="Documentos/foros.RData")








