
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

sede_de_comarca <- inner_join(municipios_sp, comarcas, by = c("nome" ="comarca"))

# Note que "sede_de_comarca" possui um número de observações inferior ao
  # Total de comarcas do TJSP
  # Isso ocorre por diferença de grafia

grafia <- anti_join(comarcas, sede_de_comarca, by = c("comarca" = "nome"))
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

sede_de_comarca <- inner_join(municipios_sp, comarcas, by = c("nome" ="comarca"))
sede_de_comarca$comarca <- sede_de_comarca$nome

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

# Mais uma vez, existe diferença de grafia entre o IBGE e o TJSP
filter(municipios_faltantes, is.na(comarca))

revisado <- c("Biritiba Mirim", "Florínea")

municipios_faltantes$comarca[municipios_faltantes$nome %in% revisado] <- c("Mogi das Cruzes", "Assis")

municipios_faltantes$comarca <- as.character(municipios_faltantes$comarca)

# "Lindoia" e "Uru" escapam da pesquisa
revisado <- c("Lindóia", "Uru")
municipios_faltantes$comarca[municipios_faltantes$nome %in% revisado] <- c("Águas de Lindóia", "Pirajuí")

#  A Comarca de Estrela d'Oeste é grafada como "Estrela dOeste pelo sistema do TJSP

municipios_faltantes$comarca <- gsub("Estrela dOeste",  "Estrela d'Oeste", municipios_faltantes$comarca )

municipios_sp <- rbind(sede_de_comarca, municipios_faltantes)|>
  arrange(nome)

save(municipios_sp, file="Documentos/municipios_sp.RData")

rm(revisado, municipios_faltantes, sede_de_comarca, busca_comarca)


################################################################################
#                   VINCULAÇNDO CADA CÓDIGO A UMA COMARCA                     #
################################################################################



#1 -  Extraindo a tabela que contém os códigos de unidade de origem
  # O Documento é desatualizado e exige modificações

httr::GET(url = "https://www.cnj.jus.br/wp-content/uploads/2011/02/foros-1.xls",
          write_disk(path = "Documentos/codigos_sp.xls", overwrite = TRUE))
                    
codigos <- as.data.frame(read_excel(here::here("Documentos", "codigos_sp.xls"))) |>
        rename(codigo = Código, descr = Descrição, comarca_antiga = Comarca)


#2 - Adicionando "Leading Zeroes"
  # Cada código de unidade judiciária possui 4 números (OOOO), sendo necessário
  # adicionar zeros no iício

codigos <- codigos |>
         mutate(codigo = str_pad(codigo, 4, pad = "0"),
        # Criando uma nova coluna
        comarca = descr)


#3 - Atualizando a lista de Comarcas

# Note que a tabela apresenta 43 comarcas a menos do que o atual número de
  # Comarcas segundo o próprio TJSP
length(comarcas$comarca) - length(unique(codigos$comarca_antiga))

# Vamos Povoar a coluna "comarca" rescém criada com a identificação da cidade,
  # tendo em vista que a disposição das comarcas pode ter mudado,
  # mas o "Foro distrital de Arujá" continua em Arujá

# Embora os códigos "0000" e "0990" constem como pertencentes à Comarca de São Paulo,
  # eles representam os processos e procedimentos de Competência Originária do Tribunal
  # Tratam-se de feitos que têm início diretamente no Tribunal,
  # e não em uma Vara de 1ª Instância

# A Exemplo, todos os agravos de instrumento do estado são distribuidos diretamente
  # às Câmaras e Sub-Presidências do Tribunal de Justiça sob o código "0000".

# Assim, a manutenção como "Comarca de São Paulo" pode ensejar conclusões erradas
  # Quando da análise dos dados, fazendo supor que todo agravo de instrumento,
  # todo recurso eespecial, todo recurso extraordinário foram ajuizados por
  # residentes na Capital

codigos <- codigos |>
      mutate(comarca = gsub(".+Regional.+-\\s+.+", "São Paulo", comarca)) |>
      mutate(comarca = gsub(".+Regional de.+", "Campinas", comarca)) |>
      #Apenas São Paulo e Campinas possuem Foros Regionais
      #Vila Mimosa é o único foro regional pertencente a Campinas 
      mutate(comarca = gsub(".+Central.+", "São Paulo", comarca)) |>
      mutate(comarca = gsub("Foro das Execuções Fiscais.+", "São Paulo", comarca)) |>
      mutate(comarca = gsub("Foro Especial da Infância e Juventude", "São Paulo", comarca)) |>
      mutate(comarca = gsub("Setor de Cartas Precatórias Cíveis - Cap", "São Paulo", comarca)) |>
      mutate(comarca = gsub("Foro de\\s+(.+)", "\\1", comarca))|>
      mutate(comarca = case_when(comarca_antiga == "São Paulo" & grepl("Distrital", codigos$comarca) ~ "São Paulo",
                            TRUE  ~ comarca)) |> #Se a Comarca for São Paulo e For um Local Distrital -> "São Paulo"
      mutate(comarca = gsub(".+Distrital de\\s+(.+)", "\\1", comarca)) |>
      mutate(comarca = gsub("Foro Unificado", "Competência Originária", comarca)) |>
      mutate(comarca = gsub("Tribunal de .+", "Competência Originária", comarca)) |>
      mutate(comarca = gsub("Foro\\s+(.+)", "\\1", comarca))


#4 - Identificando todas as comarcas que aparecem na lista municípios_sp,
  # mas que estão ausentes da tabela "codigos" 

# O objeto "municípios" já contém as 320 comarcas do Estado de São Paulo.
  #Contudo, nem todas as comarcas constam da lista do CNJ que extraímos.

anti_join(municipios_sp, codigos, by = "comarca") |>
    distinct(comarca, .keep_all = TRUE) |>
    arrange(comarca)

# Birigui - "Uso de Tremas"
# Eldorado - Registrado como "Eldorado Paulista" para distinguir de cidade homônima
# Embu das Artes - É comarca, Código "0176"
# Estrela D’Oeste -  Distinçao ente ' e ´
# Ipaussu - Registrado com "ç" ao invés de "ss"
# Palmeira D’Oeste -  Distinçao ente ' e ´ e "d" e "D"
# Santa Bárbara D’Oeste - Distinçao ente ' e ´
# Santana de Parnaíba - É comarca, Código "0529"
# São Luiz do Paraitinga - "Luis" ao invés de "Luiz"

revisado <- data.frame(embu_d_a <- c("0176", "Foro de Embu das Artes", "Embu das Artes", "Embu das Artes"),
                       santana_d_p <- c("0529", "Foro de Santana de Parnaíba", "Santana de Parnaíba", "Santana de Parnaíba"),
                  row.names = c("codigo", "descr", "comarca_antiga", "comarca"))|>
                  t()
row.names(revisado) <- NULL

codigos <- codigos |>
           rbind(revisado) |>
           arrange(comarca)     

codigos$comarca <- gsub("Birigüi",  "Birigui", codigos$comarca)
codigos$comarca <- gsub("Eldorado Paulista",  "Eldorado", codigos$comarca)
codigos$comarca <- gsub("Estrela D'Oeste",  "Estrela d'Oeste", codigos$comarca)
codigos$comarca <- gsub("Ipauçu",  "Ipaussu", codigos$comarca)
codigos$comarca <- gsub("Palmeira D'Oeste",  "Palmeira d'Oeste", codigos$comarca)
codigos$comarca <- gsub("Santa Bárbara D'Oeste",  "Santa Bárbara d'Oeste", codigos$comarca)
codigos$comarca <- gsub("São Luis do Paraitinga",  "São Luiz do Paraitinga", codigos$comarca)

rm(revisado, comarcas, embu_d_a, santana_d_p)


#5 - Identificando as localidades em "códigos" que não são sede de comarca

sede_de_comarca <- semi_join(codigos, municipios_sp, by = "comarca") |>
  select(codigo, descr, comarca)

nao_comarca <- anti_join(codigos, municipios_sp, by = "comarca") |>
  select(codigo, descr, comarca)

#6 - Temos uma lista de cidades que não são sede de comarca
  #Vamos identificar as comarcas pelo cruzamento com a tabela "municipios_sp"

nao_comarca <- left_join(nao_comarca, municipios_sp[ , 2:3], by = c("comarca" = "nome"))
              
filter(nao_comarca, is.na(comarca.y))

# Brás Cubas é foro Distrital pertencente à Comarca de Mogi das Cruzes
# Competência Originária é a designação que atribuimos aos feitos iniciados
  #diretamente em segunda instância
# Embu foi renomeada como Embu das Artes pela Lei Estadual nº 14.537
# Florínia pertence à Comarca de Assis
# Brás Cubas é foro Distrital da Comarca do Guarujá

original <- c("Brás Cubas", "Embu", "Florínia", "Vicente de Carvalho")
novo <- c("Mogi das Cruzes", "Embu das Artes", "Assis", "Guarujá")

nao_comarca$comarca.y[nao_comarca$comarca %in% original] <- novo
nao_comarca$comarca.y[nao_comarca$comarca == "Competência Originária"] <- "Competência Originária"

nao_comarca <- nao_comarca |>
               mutate(comarca = comarca.y) |>
               select(!comarca.y)

codigos <- rbind(sede_de_comarca, nao_comarca)

# Atingimos o número atual de comarcas Paulistas com a adição de um identificador
  # para os feitos de competência originária do Tribunal
n_distinct(codigos$comarca)

codigos |>
  semi_join(municipios_sp, by = "comarca") |>
  distinct(comarca, .keep_all = TRUE) |>
  nrow()

rm(nao_comarca, sede_de_comarca, novo, original)

################################################################################
#                     ADICIONANDO CÓDIGOS FALTANTES                            #
################################################################################

#1 - Identificando códigos constantes das listas de distribuição, mas
  # desprovidos de correspondente em "codigos"
load(here::here("Documentos", "Distribuicao_df.RData"))

codigos_faltantes <- distribuicao_df |>
  left_join(codigos, by = "codigo") |>
  select(processo, codigo, tribunal, comarca) |>
  filter(is.na(comarca), tribunal == "8.26") |>
  distinct(codigo, .keep_all = TRUE) |>
  arrange(codigo)


#2 - Efetuando Raspagem de Dados no sistema do TJSP
  # O procedimento a seguir consiste em uma extensa adaptação à função
  # baixar_cposg(), Desenvolvida por José de Jesus Filho e disponível em:
  # https://github.com/jjesusfilho/tjsp


busca_foro <- function(processo = NULL){
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  url <- "https://esaj.tjsp.jus.br/cposg/search.do?"
  unificado <- processo |> stringr::str_extract(".{15}")
  codigo <- processo |> stringr::str_extract("\\d{4}$")
  
  query1 <- list(conversationId = "", paginaConsulta = "1", 
               localPesquisa.cdLocal = "-1", cbPesquisa = "NUMPROC", 
               tipoNuProcesso = "UNIFICADO", numeroDigitoAnoUnificado = unificado, 
               foroNumeroUnificado = codigo, dePesquisaNuUnificado = processo, 
               dePesquisa = "", uuidCaptcha = "", pbEnviar = "Pesquisar")
  
  conteudo <- httr::RETRY("GET", url = url, query = query1, 
                         quiet = TRUE, httr::timeout(2)) |>
              httr::content("parsed") |>
              rvest::html_elements(".line-clamp__2")|>
              rvest::html_nodes(":contains('Comarca')") |>
              rvest:: html_text()|>
              str_split(" / ") |>
              as_vector()

  conteudo[1] <- gsub("Comarca de (.+)", "\\1", conteudo[1])
  conteudo[3] <- codigo
  conteudo[4] <- processo
  conteudo <- rev(conteudo)

  return(conteudo)
}

codigos_faltantes <- map(codigos_faltantes$processo, busca_foro)

#3 - Convertendo as listas aninhadas em um só df
codigos_faltantes <- data.frame(Reduce(rbind, codigos_faltantes))
names(codigos_faltantes) <- c("processo", "codigo", "descr", "comarca")
row.names(codigos_faltantes) <- NULL

#4 - Enfrentando casos não solucionados pela função
filter(codigos_faltantes, is.na(comarca))

adicionados <- data.frame(
  a = c("0027", "Foro de Iacanga", "Ibitinga"),
  b = c("0074", "Foro de Maracaí", "Maracaí"),
  b = c("0232", "Foro de Cesário Lange", "Cesário Lange"),
  b = c("0243", "UAAJ Natividade da Serra", "Paraibuna"),
  e = c("0514", "Foro Distrital de Itupeva", "Jundiaí"),
  f = c("0550", "Foro Plantão de Rio Claro", "Rio Claro"),
  g = c("0551", "Foro Plantão de Limeira", "Limeira"),
  h = c("0592", "Foro Pantão de Tupã", "Tupã"),
  i = c("0594", "Foro de Bauru", "Bauru"),
  j = c("0600", "Foro Plantão de Lins", "Lins"),
  k = c("0603", "Foro Plantão de Araçatuba", "Araçatuba"),
  l = c("0618", "Foro Plantão de Taubaté", "Taubaté"),
  m = c("0622", "Foro Plantão de Itapeva", "Itapeva"),
  n = c("0631", "Foro Plantão de Amparo", "Amparo"),
  o = c("0968", "Turma de Uniformização - JE", "Competência Originária"),
  p = c("9005", "Colégio Recursal de Santana", "São Paulo"),
  q = c("9007", "Colégio Recursal de Campinas", "Campinas"),
row.names = c("codigo", "descr", "comarca"))|>
t()

row.names(adicionados) <- NULL

codigos <- rbind(codigos,
           filter(codigos_faltantes[2:4], !is.na(comarca)),
           adicionados)|>
           arrange(codigo)

#5 - Avaliando o Resultado

# O Número de comarcas distintas em "códigos" ultrapassa em 2 a quantidade
  # atual de comarcas no estado.
n_distinct(codigos$comarca)

anti_join(codigos, municipios_sp, by = "comarca") |>
  select(codigo, descr, comarca)

# Competência Originária é a designação atribuida aos procedimentos iniciados
  # diretamente no tribunal.
  # Mogi Guaçu sem hífen.

codigos$comarca <- gsub("Mogi-Guaçu",  "Mogi Guaçu", codigos$comarca)

n_distinct(codigos$comarca)

codigos |>
  semi_join(municipios_sp, by = "comarca") |>
  distinct(comarca, .keep_all = TRUE) |>
  nrow()

# Todos os códigos não identificados dizem respeito a processos ajuizados em 
  # outros Tribunais, como pode ser aferido pelo código de Tribunal diferente de
  # "8.26"

View(distribuicao_df |>
  left_join(codigos, by = "codigo") |>
  select(processo, codigo, tribunal, comarca) |>
  filter(is.na(comarca)) |>
  distinct(codigo, .keep_all = TRUE) |>
  arrange(codigo))

rm(codigos_faltantes, adicionados)

save(codigos, file="Documentos/codigos_sp.RData")
