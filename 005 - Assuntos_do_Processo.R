
## ATENÇÃO!!!!! 
  # dado o volume de processos, cada procedimento de webscraping exige horas.
  
# O resultado final da extração está disponível a seguir:
  # load(here::here("Documentos", "obrig_f.RData"))
  # load(here::here("Documentos", "dir_obrig.RData"))
  # load(here::here("Documentos", "ato_adm.RData"))

################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse",
             "lubridate", # utilizado para separar dados por mês e ano
             "rvest", #Faz o Webscraping
             "httr", 
             "jsonlite")

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
#                                  FUNÇÕES                                     #
################################################################################

#1 - Função para extrair o assunto de cada processo da página do TJSP
busca_assunto <- function(processo = NULL){
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
    rvest::html_elements("#assuntoProcesso") |>
    rvest::html_text()
  
  conteudo[2] <- sub("-.*", "", conteudo)
  conteudo[2] <- str_trim(conteudo[2])
  conteudo[1] <- sub(".+?-", "", conteudo[1])
  conteudo[1] <- str_trim(conteudo[1])
  conteudo[3] <- processo
  
  conteudo <- rev(conteudo)
  
  return(conteudo)
}

#2 - Converte listas aninhadas em um só data frame
converte_df <- function(lista = NULL){
  lista <- data.frame(Reduce(rbind, lista))
  names(lista) <- c("processo", "macro_assunto", "assunto_especifico")
  row.names(lista) <- NULL
  
  # Covnverte a classe das variáveis
  fator <- c("macro_assunto", "assunto_especifico")
  lista[fator] <- lapply(lista[fator], as.factor)
  
  return(lista)
}

################################################################################
#                           Assuntos do Processo                               #
################################################################################

load(here("Documentos", "Distribuicao_df.RData"))
load(here("Documentos", "codigos_sp.RData"))
load(here("Documentos", "indicadores.RData"))

#1 - Identificar as categorias das variáveis "Natureza"

summary(distribuicao_df$natureza)
table(distribuicao_df$natureza)

# 60.199 causas são identificadas como sendo "OBRIG_F", "DIR.OBR", "ATO ADM"
nrow(distribuicao_df |>
  filter(natureza %in% c("OBRIG_F", "DIR.OBR", "ATO ADM")))

#2 -  Estimando o conteúdo das ações de obrigação de fazer

  # ATENÇÃO!!!!! o procedimento de webscraping exige horas
  # O resultado final da extração está disponível a seguir:
  # load(here::here("Documentos", "obrig_f.RData"))

obrig_f <- distribuicao_df |>
  filter(tribunal == "8.26",
         natureza == "OBRIG_F") |>
  distinct(processo, .keep_all = FALSE)

# Extraindo o assunto de cada processo a partir da página do TJSP
obrig_f <- map(obrig_f$processo, busca_assunto)

obrig_f <- converte_df(obrig_f)

save(obrig_f, file="Documentos/obrig_f.RData")

# Avaliando a consistência dos dados
colSums(is.na(obrig_f))

# Aferindo a proporção de processos sem assunto (NA) - 21,4%
sum(is.na(obrig_f$macro_assunto)) / nrow(obrig_f)

# Excluindo as linhas com "missing values"
obrig_f <- obrig_f |>
  drop_na()

# Identificando os assuntos recorrentes
summary(obrig_f, maxsum = 10)

unique(obrig_f$macro_assunto)
unique(obrig_f$assunto_especifico)

#4 - Identificando as causas que envolvem o direito à saúde

saude <- obrig_f |>
  # Convertendo para letras maíusculas
  mutate_all(.funs = str_to_upper) |>
  # Cocnatenando as colunas em uma nova coluna chamada "natureza"
  unite(natureza, macro_assunto:assunto_especifico, remove = F)|>
  # Filtra as linhas que contenham alguma das seguintes palavras:
  filter(str_detect(natureza,
                    paste(
                      c("SAÚDE", "MÉDICO", "MEDICA", "HOSPITAL", "HOME CARE", "INSUMO"),
                      collapse = "|"))) |>
  # Selecionando apenas as colunas "processo" e "natureza"
  select(processo, natureza) |>
  # A natureza dos processo será renomada como "SAÚDE"
  mutate(natureza = "SAÚDE")

# Identificamos 14.155 processos únicos relacionados ao direito à Saúde  
nrow(saude)

save(saude, file="Documentos/saude.RData")


#5-  Estimando o conteúdo das ações classificadas como "direito das obrigações"
# ATENÇÃO!!!!! o procedimento de webscraping exige horas
# O resultado final da extração está disponível a seguir:

# load(here::here("Documentos", "dir_obrig.RData"))

dir_obrig <- distribuicao_df |>
  filter(tribunal == "8.26",
         natureza == "DIR.OBR") |>
  distinct(processo, .keep_all = FALSE)

dir_obrig <- map(dir_obrig$processo, busca_assunto) |>
  converte_df()

save(dir_obrig, file="Documentos/dir_obrig.RData")

# Aferindo a proporção de processos sem assunto (NA) - 9,3%
sum(is.na(dir_obrig$macro_assunto)) / nrow(dir_obrig)

# Excluindo as linhas com "missing values"
dir_obrig <- drop_na(dir_obrig)

# Identificando os assuntos recorrentes
summary(dir_obrig, maxsum = 10)

unique(dir_obrig$macro_assunto)
unique(dir_obrig$assunto_especifico)

#6 - Identificando as causas que envolvem o direito à saúde

saude2 <- dir_obrig |>
  # Convertendo para letras maíusculas
  mutate_all(.funs = str_to_upper) |>
  # Cocnatenando as colunas em uma nova coluna chamada "natureza"
  unite(natureza, macro_assunto:assunto_especifico, remove = F)|>
  # Filtra as linhas que contenham alguma das seguintes palavras:
  filter(str_detect(natureza,
                    paste(
                      c("SAÚDE", "MÉDICO", "MEDICA", "HOSPITAL", "HOME CARE", "INSUMO"),
                      collapse = "|"))) |>
  # Selecionando apenas as colunas "processo" e "natureza"
  select(processo, natureza) |>
  # A natureza dos processo será renomada como "SAÚDE"
  mutate(natureza = "SAÚDE")

# Identificamos 5.093 processos únicos relacionados ao direito à Saúde  
nrow(saude2)

saude <- rbind(saude, saude2)

save(saude, file="Documentos/saude.RData")

#7 - Estimando o conteúdo das ações que questionam atos administrativos

# ATENÇÃO!!!!! o procedimento de webscraping exige horas
# O resultado final da extração está disponível a seguir:

# load(here::here("Documentos", "ato_adm.RData"))

ato_adm <- distribuicao_df |>
  filter(tribunal == "8.26",
         natureza == "ATO ADM") |>
  distinct(processo, .keep_all = FALSE)

ato_adm <- map(ato_adm$processo, busca_assunto) |>
  converte_df()

save(ato_adm, file="Documentos/ato_adm.RData")

# Aferindo a proporção de processos sem assunto (NA) - 4,6%
sum(is.na(ato_adm$macro_assunto)) / nrow(ato_adm)

# Excluindo as linhas com "missing values"
ato_adm <- drop_na(ato_adm)

# Identificando os assuntos recorrentes
summary(ato_adm, maxsum = 10)

unique(ato_adm$macro_assunto)
unique(ato_adm$assunto_especifico)

#8 - Identificando as causas que envolvem o direito à saúde

saude2 <- ato_adm |>
  # Convertendo para letras maíusculas
  mutate_all(.funs = str_to_upper) |>
  # Cocnatenando as colunas em uma nova coluna chamada "natureza"
  unite(natureza, macro_assunto:assunto_especifico, remove = F)|>
  # Filtra as linhas que contenham alguma das seguintes palavras:
  filter(str_detect(natureza,
                    paste(
                      c("SAÚDE", "MÉDICO", "MEDICA", "HOSPITAL", "HOME CARE", "INSUMO"),
                      collapse = "|"))) |>
  # Selecionando apenas as colunas "processo" e "natureza"
  select(processo, natureza) |>
  # A natureza dos processo será renomada como "SAÚDE"
  mutate(natureza = "SAÚDE")

# Identificamos 449 processos únicos relacionados ao direito à Saúde  
nrow(saude2)

saude <- rbind(saude, saude2)

rm(saude2)

save(saude, file="Documentos/saude.RData")

#9 - Causas que versam sobre direito à saúde

# Mais causas foram classificadas como Direito das Obrigações, Obrigação de Fazer e 
# Direito Administrativo (19.697) do que pelas designações acima (11.957)
distribuicao_df |>
  distinct(processo, .keep_all = TRUE) |>
  filter(natureza %in% c("MEDIC", "SAÚDE", "ESP MEDIC", "ESP SAÚDE")) |>
  nrow()

# Identificando o total de processos distribuidos que envolvem o direito
# à Saude

df1 <- distribuicao_df |>
  filter(natureza %in% c("MEDIC", "SAÚDE", "ESP MEDIC", "ESP SAÚDE"))

df2 <- distribuicao_df |>
  filter(tribunal == "8.26",
         natureza %in% c("ATO ADM", "DIR.OBR", "OBRIG_F")) |>
  select(!natureza)  |>
  inner_join(saude, by = "processo")

saude <- rbind(df1, df2)

rm(df1, df2)

save(saude, file="Documentos/saude.RData")




################################################################################
#                    Estudando as Ações de Alimento                            #
################################################################################
load(here("Documentos", "alimento.RData"))

# Vinculando cada processo com origem em 1ª instância a sua 
# respecticva comarca
alimento <- alimento |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         propositura %in% as.character(2017:2020),
         comarca != "Competência Originária",
         # Atenção! Corrigir esses espaços!
         tipo %in% c("AJ ","AP ", "RECURSO ","REEXAME ")) |>
  distinct(processo, .keep_all = TRUE) |>
  group_by(propositura, comarca) |>
  summarise(num_proc = n()) |>
  ungroup () %>% droplevels(.)  

comarca <- unique(codigos$comarca)
comarca <- comarca[comarca != "Competência Originária"]

zero <- data.frame(
  propositura = rep(2017:2020, times = 320),
  comarca = rep(comarca, each = 4))

zero <- left_join(zero, alimento, by = c("propositura", "comarca"))

zero$num_proc[is.na(zero$num_proc)] <- 0

alimento <- zero

rm(comarca, zero)

# Vinculando os indicadores socioeconômicos aos processos ajuizados
alimento <- alimento |>
  left_join(indicadores, by = c("propositura" = "ano", "comarca"))

# Estatísticas descritivas univariadas e tabela de frequências
summary(alimento)

# Matriz de Correlações
rho_alimento <- cor(alimento[, c(-1, -2)])
rho_alimento[1, ]

correlation::correlation(alimento)
cor.test(alimento$num_proc, alimento$salario)

# Será que a correlação muda ao excluirmos São Paulo?
teste <- filter(alimento, comarca != "São Paulo")
rho_teste <- cor(teste[, c(-1, -2)])
rho_teste[1, ]

correlation::correlation(teste[,c(-1, -2)])
cor.test(teste$num_proc, teste$salario)

# Modelagem com todas as variáveis
alimento_rl <- lm(num_proc ~ populacao, alimento)

# Parâmetros do modelo
summary(alimento_rl)

# Somando o número de pessoas com até 19 em cada município

alimento <- alimento |>
  mutate(menor20 = rowSums(alimento[, 11:14]))

alimento_rl <- lm(num_proc ~ menor20, alimento)
summary(alimento_rl)

# Somando o número de pessoas menores de 25 em cada município
alimento <- alimento |>
  mutate(menor25 = rowSums(alimento[, 11:15]))

alimento <- alimento |>
  mutate(f25a39 = rowSums(alimento[, 16:18]))

alimento_rl <- lm(num_proc ~ menor25, alimento)
summary(alimento_rl)

alimento_rl <- lm(num_proc ~ ., alimento[ , c(1, 5:21)])
summary(alimento_rl)

alimento_step <- step(alimento_rl, k = 3.841459)

summary(alimento_step)
anova(alimento_step)
sf.test(alimento_step$residuals)

################################################################################
#                      Estudando as Ações de Saúde                             #
################################################################################
load(here("Documentos", "saude.RData"))

# Vinculando cada processo com origem em 1ª instância a sua 
# respecticva comarca
saude <- saude |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         propositura %in% as.character(2006:2020),
         comarca != "Competência Originária",
         #Atenção! Note o espaçamento! Modificar Depois
         tipo %in% c("AJ ","AP ", "AJ ", "RECURSO ","REEXAME ")) |>
  distinct(processo, .keep_all = TRUE) |>
  group_by(propositura, comarca) |>
  summarise(num_proc = n()) |>
  ungroup () %>% droplevels(.) 

# Comarcas sem causas ajuizadas:
comarca_zero <- unique(indicadores$comarca[!indicadores$comarca %in% saude$comarca])

# Agrupando o número de processos ajuizados por ano e comarca:
saude <- saude |>
  group_by(propositura, comarca) |>
  # Conta o número de processos ajuizados em cada comarca a cada ano
  summarise(num_proc = n()) |>
  ungroup () %>% droplevels(.)  

# Adicionando o valor de 0 para a contagem de Icanga e Itupeva
zero <- data.frame(
  propositura = rep(2006:2020, length(comarca_zero)),
  comarca = comarca_zero,
  num_proc = 0)

saude <- rbind(saude, zero)

# Vinculando os indicadores socioeconômicos aos processos ajuizados
saude <- saude |>
  left_join(indicadores, by = c("propositura" = "ano", "comarca")) |>
  dplyr::select(num_proc, everything(), -c(propositura, comarca))

# Estatísticas descritivas univariadas e tabela de frequências
summary(saude)

# Matriz de Correlações
rho_saude <- cor(saude)
rho_saude[1, ]

# Modelagem com todas as variáveis
saude_rl <- lm(num_proc ~ populacao, saude)

# Parâmetros do modelo
summary(saude_rl)

# Somando o número de pessoas menores de 20 em cada município
saude <- saude |>
  mutate(menor20 = rowSums(saude[, 5:8]))

saude_rl <- lm(num_proc ~ menor20, saude)
summary(saude_rl)

# Somando o número de pessoas menores de 25 em cada município
saude <- saude |>
  mutate(menor25 = rowSums(saude[, 5:9]))

saude_rl <- lm(num_proc ~ menor25, saude)
summary(saude_rl)

saude_rl <- lm(num_proc ~ ., saude[ , c(1, 5:21)])
summary(saude_rl)

saude_step <- step(saude_rl, k = 3.841459)

summary(saude_step)

anova(saude_step)
sf.test(saude_step$residuals)

chart.Correlation(alimento)


