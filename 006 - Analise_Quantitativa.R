################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse",
             "lubridate", # utilizado para separar dados por mês e ano
             "openxlsx" # manipul arquivos ".xlsx"
             )

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
#                            Análise Quantitativa                              #
################################################################################

load(here("Documentos", "Distribuicao_df.RData"))
load(here("Documentos", "codigos_sp.RData"))
load(here("Documentos", "indicadores.RData"))

# Estatísticas univariadas
summary(distribuicao_df)

table(distribuicao_df$codigo)


# Vamos agrupar o dataset com base no mês e ano
distr_mes <- distribuicao_df |> 
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.) |>
  filter (mes < "2022-08-01") #Vamos excluir o mês em aberto

ggplot(data = distr_mes) + 
  geom_line(aes(x = mes , y = num_proc)) +
  geom_smooth(aes(x = mes , y = num_proc), color = "black", method = "lm") +
  labs(x = "Mês",
       y = "Número de Processos Distribuidos",
       title = "Distribuições Mês a Mês"
       ) +
  theme(axis.line = element_line(color="black", size = 1.5))

openxlsx::write.xlsx(distr_mes, file = "E:/Andre/DataScience/Projeto MP/Documentos/distr_mes.xlsx")


# Vamos Vizualizar o dataset com base no ano 
distr_ano <- distribuicao_df |> 
  group_by(ano = floor_date(data, unit = "year")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.) |>
  filter (ano < "2022-01-01" & ano > "2017-12-31")

#Um incremento de 17% entre janeiro de 2018 e dezembro de 2021
(distr_ano$num_proc[4]/distr_ano$num_proc[1])*100
  
  ggplot(distr_ano) + 
  geom_line(aes(x = ano , y = num_proc)) +
  geom_smooth(aes(x = ano , y = num_proc), method = "lm") +
  labs(x = "Ano",
       y = "Número de Processos",
       title = "Distribuições a Cada Ano")
  

# Vamos Aferir quantos cargos ativos e quntos processos por cargo ativo

distr_cargo_ano <- distribuicao_df |>
  filter(cargo != 99) |> #Excluindo os casos distribuidos à Secretaria  
  group_by(ano = floor_date(data, unit = "year")) |>
  summarise(num_proc = n(),
            c_ativos = n_distinct(cargo),
            proc_cargo = num_proc / c_ativos)|>
  ungroup () %>% droplevels(.) |>
  filter (ano < "2022-01-01" & ano > "2017-12-31")

openxlsx::write.xlsx(distr_cargo_ano, file = "E:/Andre/DataScience/Projeto MP/Documentos/distr_cargo.xlsx", overwrite = TRUE)
  
(distr_cargo_ano$proc_cargo[4]/distr_cargo_ano$proc_cargo[1])*100  
  
distr_cargo <- distribuicao_df |> 
  filter(cargo != 99) |> #Excluindo os casos distribuidos à Secretaria
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(num_proc = n(),
            c_ativos = n_distinct(cargo),
            proc_cargo = num_proc / c_ativos)|>
  ungroup () %>% droplevels(.) |>
  filter (mes < "2022-08-01") #Vamos excluir o mês em aberto

openxlsx::write.xlsx(distr_cargo, file = "E:/Andre/DataScience/Projeto MP/Documentos/distr_cargo.xlsx")

ggplot(data = distr_cargo) + 
  geom_line(aes(x = mes , y = proc_cargo)) + 
  geom_smooth(aes(x = mes , y = proc_cargo), method = "lm") +
  labs(x = "Mês",
       y = "Processos por Cargo",
       title = "Distribuições Por Cargo")


# Vamos Aferir cargos ativos a cada semana
distr_cargo_semana <- distribuicao_df |> 
  filter(cargo != 99) |> #Excluindo os casos distribuidos à Secretaria
  group_by(semana = floor_date(data, unit = "week")) |>
  summarise(num_proc = n(),
            c_ativos = n_distinct(cargo),
            proc_cargo = num_proc / c_ativos)|>
  ungroup () %>% droplevels(.)
  

ggplot(data = distr_cargo_semana) + 
  geom_line(aes(x = semana , y = proc_cargo)) + 
  geom_smooth(aes(x = semana , y = proc_cargo), method = "lm") +
  labs(x = "Mês",
       y = "Processos por Cargo",
       title = "Distribuições Por Cargo")

 max(distr_cargo_semana$proc_cargo) 
  

# Evolução no número de processos digitais

distribuicao_df |> 
 group_by(ano = floor_date(data, unit = "year")) |>
 summarise(pct.digital = mean(digital == "SIM"))

# Proporção entre comarcas

prop_comarcas <- distribuicao_df |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         comarca != "Competência Originária",
         tipo %in% c("AP ", "MS ", "PETIÇÃO ", "AJ-")) |>
  distinct(processo, .keep_all = TRUE) |>
  group_by(comarca) |>
  summarise(cont = n()) |>
  mutate(prop_ajuiz = cont / sum(cont) * 100) |>
  arrange(desc(prop_ajuiz)) |>
  ungroup () %>% droplevels(.)

prop_populacao <- indicadores[indicadores$ano == "2020", c("comarca", "populacao")] |>
  mutate(prop_popl = populacao / sum(populacao) * 100)

prop_comarcas <- prop_comarcas |>
  left_join(prop_populacao, by = "comarca") |>
  mutate(judicializacao = prop_ajuiz / prop_popl)

# Morosidade da Justiça
morosidade <- distribuicao_df |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         comarca != "Competência Originária",
         tipo %in% c("AP ", "MS ", "PETIÇÃO ", "AJ-")) |>
  distinct(processo, .keep_all = TRUE) |>
  group_by(ano = floor_date(data, unit = "year")) |>
  summarise(tempo_gasto = mean(propositura))|>
  ungroup () %>% droplevels(.)

morosidade$teste <- as.Date(date_decimal(morosidade$tempo_gasto),"%d-%m-%Y", tz = "UTC")

morosidade$dif_dia <- morosidade$ano - morosidade$teste
morosidade$dif_ano <-time_length(morosidade$ano - morosidade$teste, "month")

# Quanto tempo o processo de cada comarca demora para chegar à 2ª Inst
morosidade_comarca <- distribuicao_df |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         comarca != "Competência Originária",
         tipo %in% c("AP ", "MS ", "PETIÇÃO ", "AJ-")) |>
  distinct(processo, .keep_all = TRUE) |>
  group_by(comarca) |>
  summarise(morosidade = mean(propositura))|>
  ungroup () %>% droplevels(.)

prop_comarcas <- prop_comarcas |>
  left_join(morosidade_comarca, by = "comarca")

filter(codigos, comarca == "Taubaté")

teste <- filter(distribuicao_df, codigo %in% c("0618", "0625"))
mean(teste$propositura)

# Média de vezes que um processo retorna ao MP
retorno <- table(distribuicao_df['processo'])
mean(retorno)

# Média de vezes que um processo de 1ª Instância retorna ao MP
retorno <- distribuicao_df |>
  filter(tipo %in% c("AP ", "MS ", "PETIÇÃO ", "AJ-")) |>
  count(processo)

mean(retorno$n)
rm(retorno)

################################################################################
#                           Assuntos do Processo                               #
################################################################################

#1 - Identificar as categorias das variáveis "Natureza"

table(distribuicao_df$natureza)
summary(distribuicao_df$natureza)
# É possível notar uma certa inconsistência no cadastramento das "Naturezas"
# A exemplo, ações de alimento aparecem tanto como "ALIMENT" quanto "ALIMENTO"

#2 - Causas Que Versam sobre o Direito a alimentos
alimen <- c("ALIMENT", "REVISION", "ALIMENTO", "ALIMPRO", "ALPAREN", "EXE.ALM")

# 24,5% de todas as causas versam sobre o direito a alimentos (72.643)
nrow(filter(distribuicao_df, natureza %in% alimen))
nrow(filter(distribuicao_df, natureza %in% alimen)) / nrow(distribuicao_df)

alimento <- distribuicao_df |>
  filter(natureza %in% alimen)

rm(alimen)

save(alimento, file="Documentos/alimento.RData")

# Visualizando a evolução do número de processos
distr_alim <- alimento |> 
  group_by(ano = floor_date(data, unit = "year")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.) |>
  filter (ano < "2022-01-01" & ano > "2017-12-31")

ggplot(distr_alim) + 
  geom_line(aes(x = ano , y = num_proc)) +
  geom_smooth(aes(x = ano , y = num_proc), method = "lm") +
  labs(x = "Ano",
       y = "Processos de Alimentos",
       title = "Processos Envolvendo o Direito a Alimentos")

#Um incremento de 12,4% entre janeiro de 2018 e dezembro de 2021
(distr_alim$num_proc[4]/distr_alim$num_proc[1])*100

rm(distr_alim)

#3 -  Estimando o conteúdo das ações de obrigação de fazer

  # ATENÇÃO!!!!! o procedimento de webscraping exige horas
  # O resultado final da extração está disponível a seguir:

  # load(here::here("Documentos", "obrig_f.RData"))

obrig_f <- distribuicao_df |>
  filter(tribunal == "8.26",
         natureza == "OBRIG_F") |>
  distinct(processo, .keep_all = FALSE)


# Função para extrair o assunto de cada processo da página do TJSP
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

# Extraindo o assunto de cada processo a partir da página do TJSP
# Atenção!!! O procedimento exige horas!
obrig_f <- map(obrig_f$processo, busca_assunto)

# Convertendo as listas aninhadas em um só df
converte_df <- function(lista = NULL){
  lista <- data.frame(Reduce(rbind, lista))
  names(lista) <- c("processo", "macro_assunto", "assunto_especifico")
  row.names(lista) <- NULL
  
  # Covnverte a classe das variáveis
  fator <- c("macro_assunto", "assunto_especifico")
  lista[fator] <- lapply(lista[fator], as.factor)
  
  return(lista)
  }

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

# Ao menos 12,8% de todos os processos dizem respeito ao direito à Saúde (37.734)
nrow(saude) / nrow(distribuicao_df)

save(saude, file="Documentos/saude.RData")

# Visualizando a evolução do número de processos
distr_saude <- saude |> 
  group_by(ano = floor_date(data, unit = "year")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.) |>
  filter (ano < "2022-01-01" & ano > "2017-12-31")

ggplot(distr_saude) + 
  geom_line(aes(x = ano , y = num_proc)) +
  geom_smooth(aes(x = ano , y = num_proc), method = "lm") +
  labs(x = "Ano",
       y = "Processos de Alimentos",
       title = "Processos Envolvendo o Direito a Alimentos")

# Um incremento de 35% entre janeiro de 2018 e dezembro de 2021
(distr_saude$num_proc[4]/distr_saude$num_proc[1])*100

rm(distr_saude)
