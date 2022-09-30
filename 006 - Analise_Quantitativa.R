################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse",
             "lubridate", # utilizado para separar dados por mês e ano
             "openxlsx", # manipula arquivos ".xlsx"
             
             "fitdistrplus", # auxilia a identificar a distribuição que melhor se adequa
              # aos dados 
             
             "gamlss",# testa a aderência a diferentes distribuiçoes e retorna
                # aquela que minimiza o AIC
             
             "olsrr", # Diagnóstico dos Resíduos
             "MASS" # cálculo dos parâmetros da distribuição
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
#                                  FUNÇÕES                                     #
################################################################################

#1 - Agrupa o número de processos conforme janelas de 12 meses
  # independente do mês de início e fim (não precisa seguir o ano calendário)

  # Com isso, conseguimos mais um ano de dados que seriam excluidos de outro modo
  # A opção por períodos de janeiro a dezembro limitaria a análise ao período
  # compreendido entre 2018 a 2012, ignorando 2017 e 2022.

agrupa_12 <- function(dado = NULL){
  dado <- dado |>
  group_by(indice = c(0, rep(1:(nrow(dado)-1)%/%12))) |>
  summarise(num_proc = sum(num_proc))|>
  ungroup () %>% droplevels(.)
  
  return(dado)
}

#Converte um nº dias para anos, meses e dias
ano_dia <- function(x) {
  ano <- x %/% 365
  x <- x - (ano * 365)
  mes <- x %/% 30.42
  dia <- round(x - (mes * 30.42))
  paste(ano, "anos e", mes, "meses e", dia, "dias")
}

#Converte meses para meses e dias
ano_mes <- function(x) {
  ano <- x %/% 12
  mes <- (x %% 12)
  paste(ano, "anos e", mes, "meses")
}

#Converte frações de mês para meses e dias
mes_dia <- function(x) {
  mes <- floor(x)
  dia <- round((x - mes) * 30.42);
  paste(mes, "meses e", dia, "dias")
}

################################################################################
#                            Análise Quantitativa                              #
################################################################################

load(here("Documentos", "Distribuicao_df.RData"))
load(here("Documentos", "codigos_sp.RData"))
load(here("Documentos", "indicadores.RData"))

# Estatísticas univariadas
summary(distribuicao_df)

#1 - Vamos agrupar o dataset com base no mês e ano
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

ggplot(distr_ano) + 
  geom_line(aes(x = ano , y = num_proc)) +
  geom_smooth(aes(x = ano , y = num_proc), method = "lm") +
  labs(x = "Ano",
       y = "Número de Processos",
       title = "Distribuições a Cada Ano")

# Um incremento de 17% entre janeiro de 2018 e dezembro de 2021
(distr_ano$num_proc[4]/distr_ano$num_proc[1])*100

# Um incremento de 23% entre agosto de 2017 a julho de 2022
m12 <- agrupa_12(distr_mes)  

(m12$num_proc[5]/m12$num_proc[1] - 1)*100

rm(distr_ano, distr_mes)
  
#2 - Vamos aferir quantos cargos ativos e quntos processos por cargo ativo

distr_cargo_ano <- distribuicao_df |>
  filter(cargo != 99) |> #Excluindo os casos distribuidos à Secretaria  
  group_by(ano = floor_date(data, unit = "year")) |>
  summarise(num_proc = n(),
            c_ativos = n_distinct(cargo),
            proc_cargo = num_proc / c_ativos)|>
  ungroup () %>% droplevels(.) |>
  filter (ano < "2022-01-01" & ano > "2017-12-31")

openxlsx::write.xlsx(distr_cargo_ano, file = "E:/Andre/DataScience/Projeto MP/Documentos/distr_cargo.xlsx", overwrite = TRUE)
  
distr_cargo_mes <- distribuicao_df |> 
  filter(cargo != 99) |> #Excluindo os casos distribuidos à Secretaria
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(num_proc = n(),
            c_ativos = n_distinct(cargo),
            proc_cargo = num_proc / c_ativos)|>
  ungroup () %>% droplevels(.) |>
  filter (mes < "2022-08-01") #Vamos excluir o mês em aberto

openxlsx::write.xlsx(distr_cargo_mes, file = "E:/Andre/DataScience/Projeto MP/Documentos/distr_cargo.xlsx")

ggplot(data = distr_cargo_mes) + 
  geom_line(aes(x = mes , y = proc_cargo)) + 
  geom_smooth(aes(x = mes , y = proc_cargo), method = "lm") +
  labs(x = "Mês",
       y = "Processos por Cargo",
       title = "Distribuições Por Cargo")

# Um incremento de 23% entre agosto de 2017 a julho de 2022
m12 <- agrupa_12(distr_cargo_mes)  

(m12$num_proc[5]/m12$num_proc[1] - 1)*100

rm(distr_cargo_ano, distr_cargo_mes)

#3 - Cargos ativos a cada semana
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
  
#4 - Evolução no número de processos digitais
digitais <- distribuicao_df |> 
  group_by(ano = floor_date(data, unit = "month")) |>
  summarise(pct.digital = mean(digital == "NÃO")) |>
  ungroup () %>% droplevels(.)

digitais <- digitais |>
  group_by(indice = c(0, rep(1:(nrow(digitais)-1)%/%12))) |>
  summarise(pct.digital = mean(pct.digital))|>
  ungroup () %>% droplevels(.)

rm(digitais)

#5 - Média de Manifestações por causa ajuizada
retorno <- distribuicao_df |>
  filter(tribunal == "8.26",
         tipo %in% c("AJ","AP", "RECURSO","REEXAME")) |>
  distinct(processo, .keep_all = TRUE)

# Cada controvérsia gera em média duas 2,1 manifestações na Procuradoria
distribuicao_df |>
  filter(tribunal == "8.26")|>
  nrow() / nrow(retorno)

rm(retorno)

#6 - Morosidade da Justiça - Quanto tempo demora para chegar à procuradoria

# Cria uma tabela apenas com processos originados mo TJSP, 
  # e que tenham ingressado na procuradoria como recurso contra decisão final
  # proferida por juízo de 1ª Instância (sentença)

recurso_sentenca <- distribuicao_df |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  # Seleciona apenas os processos com origem no TJSP
  filter(tribunal == "8.26",
         # Exclui osprocessos instaurados diretamente em 2ª Instância
         comarca != "Competência Originária",
         # Seleciona apenas os recursos contra decisão
         tipo %in% c("AJ","AP", "RECURSO","REEXAME"),
         #Excluindo os processos com data de ajuizamento mal cadastrado        
         propositura %in% 1900:2022) |>
  # Evitando duplicidade
  distinct(processo, .keep_all = TRUE)

save(recurso_sentenca, file="Documentos/Recurso_contra_sentenca.RData")

morosidade <- recurso_sentenca |>
  # Convertendo o ano de propositura para o formato de data
  # Atribuindo o dia que representa a metade do ano forense
  # (exclui os dias do recesso forense, entre 20 dez. e 06 jan.)
  # 30 de jun. de cada ano
  mutate(propositura = as.Date(paste(propositura, 6, 30, sep = "-")))|>
  # Agrupando os dados por ano de entrada na Procuradoria
  group_by(ano = floor_date(data, unit = "year")) |>
  summarise(propositura = mean(propositura),
            ingresso = mean(data),
            morosidade = round(time_length(ingresso - propositura, "month")))|>
  ungroup () %>% droplevels(.)

rm(morosidade)

#7 - Quanto tempo o recurso contra sentença de cada comarca demora para chegar à 2ª Inst
morosidade_comarca <- recurso_sentenca |> 
  mutate(propositura = as.Date(paste(propositura, 6, 30, sep = "-")))|>
  group_by(comarca) |>
  summarise(num_proc = n(),
            propositura = mean(propositura),
            ingresso = mean(data),
            morosidade = round(time_length(ingresso - propositura, "day")))|>
  ungroup () %>% droplevels(.)

# Convertendo para formato de ano e mês
morosidade_comarca <- morosidade_comarca |>
  mutate(extenso = ano_dia(morosidade)) |>
  arrange(comarca)

# Capturando a distribuicaod os dados
dist <- morosidade_comarca$morosidade

# Visualizando a distribuição dos dados
hist(dist)

# Teste de Normalidade Shapiro-Wilk
# p_valor < 0.5 - distribuição diferente da curva normal
shapiro.test(dist)

# Váriância 68X superior à média
  # Não se adequa à distribuição de poisson
mean(dist)
var(dist)

# Tentando identificar da distribuição que melhor se adequa aos dados
  # Dentre as possibildiades, mais se aproxima da lognormal 
fitdistrplus::descdist(dist)

# utilizando a função gamlss::fitDist() para testar automaticamente
  # diversas distribuições Retorna a que apresenta o menor AIC
  # type = "realline" will test all distributions defined on the whole real line
  # type = "realplus" will only try distributions defined on the real positive line.
  # K = 2 indica que pretendemos selecionar de acordo com o menor AIC
fit <- fitDist(y = dist, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

summary(fit)

gamlss(formula = dist ~ 1, family = "LNO", trace = FALSE)

# Testando a aderência dos resíduos à normalidade
  # Shapiro-Wilk - pvalue > 0.05 -> hipótese H0 = distribuição normal
  # Kolmogorov-Smirnov - pvalue> 0.05 -> hipótese H0 = aderêcia  à distr estudada
ols_test_normality(fit$residuals)

fit  <-  fitdistr(dist, "lognormal")$estimate

# pvalue > 0.05 -> Hipótese H0, aderência à distribuição lognormal
ks.test(x = dist, y = "plnorm", fit[1], fit[2])

openxlsx::write.xlsx(morosidade_comarca, file = "E:/Andre/DataScience/Projeto MP/Documentos/morosidade.xlsx")

rm(dist, fit)

#8 - Comparando a proporção de ajuizamentos em cada comarca com sua população
prop_comarcas <- recurso_sentenca |>
  group_by(comarca) |>
  summarise(num_proc = n()) |>
  mutate(prop_ajuiz = num_proc / sum(num_proc) * 100) |>
  arrange(comarca) |>
  ungroup () %>% droplevels(.)

# A média de data de ajuizamento dos processos sentenciados é 2018
idade_media <- recurso_sentenca |> 
  mutate(propositura = as.Date(paste(propositura, 6, 30, sep = "-")))
mean(idade_media$propositura)

# Vamos comparar, portanto, com a estimativa populacional de 2018
prop_populacao <- indicadores[indicadores$ano == "2018", c("comarca", "populacao")] |>
  mutate(populacao = populacao / sum(populacao) * 100)

prop_comarcas <- prop_comarcas |>
  left_join(prop_populacao, by = "comarca") |>
  mutate(judicializacao = prop_ajuiz / populacao) |>
  arrange(comarca)

# Será que a disparidade na morosidade em algumas comarcas pode ser
# explicada pelo volume excessivo de processos?

filter(prop_comarcas, comarca %in% c("Taubaté", "Santa Rosa de Viterbo", "Votuporanga"))

#Atenção! É necessário se assegurar que as duas colunas sigam a mesma ordem
cor.test(morosidade_comarca$morosidade, prop_comarcas$judicializacao)

rm(morosidade_comarca, prop_populacao, prop_comarcas)

#8 - Processos de duas Comarcas não alcançaram a Procuradoria
ajuizamento <- distribuicao_df |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  dplyr::select(propositura, comarca) |>
  filter(comarca != "Competência Originária") |>
  left_join(indicadores, by = c("propositura" = "ano", "comarca")) |>
  dplyr::select(num_proc, everything())

# Duas comarcas não possuem processo registrado
length(unique(ajuizamento$comarca))

# São as Jurisdições de Icanga e Itupeva
unique(indicadores$comarca[!indicadores$comarca %in% ajuizamento$comarca])

# São comarcas com população estimada de 12.002 e 64.330 habitantes em 2021
indicadores|>
  dplyr::filter(ano == 2020, 
                comarca %in% c("Iacanga", "Itupeva")) |>
  dplyr::select(comarca, populacao)
  
  summarise(populacao = sum(populacao)) |>
  arrange(populacao) |>
  dplyr::filter(comarca %in% c("Iacanga", "Itupeva"))
  
rm(ajuizamento)

# Itupeva foi foro distrital da Comarca de Jundiaí até 2016
# https://www.itupeva.sp.gov.br/prefeitura/jurisdicao

# O Foro de Iacanga foi inaugurado em 2015
# https://www.tjsp.jus.br/Noticias/Noticia?codigoNoticia=26252

################################################################################
#                   Analisando Assuntos Específicos                            #
################################################################################

#1 - Identificar as categorias das variáveis "Natureza"

summary(distribuicao_df$natureza)
table(distribuicao_df$natureza)
# É possível notar uma certa inconsistência no cadastramento das "Naturezas"
# A exemplo, ações de alimento aparecem tanto como "ALIMENT" quanto "ALIMENTO"

#2 - Causas Que Versam sobre o Direito a alimentos
alimen <- c("ALIMENT", "REVISION", "ALIMENTO", "ALIMPRO", "EXE.ALM")

# 24,5% de todas as causas versam sobre o direito a alimentos (72.526)
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

# Um incremento de 18,4% entre agosto de 2017 a julho de 2022
distr_alim <- alimento |> 
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.)

m12 <- agrupa12(distr_alim)

(m12$num_proc[5]/m12$num_proc[1] - 1)*100

rm(distr_alim)


#3 - Causas que versam sobre direito à saúde

load(here("Documentos", "saude.RData"))

# Ao menos 12,8% de todos os processos dizem respeito ao direito à Saúde (37.734)
nrow(saude)
nrow(saude) / nrow(distribuicao_df)

# Visualizando a evolução do número de processos
distr_saude <- saude |> 
  group_by(ano = floor_date(data, unit = "month")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.) |>
  filter (ano < "2022-08-01")

openxlsx::write.xlsx(distr_saude, file = "E:/Andre/DataScience/Projeto MP/Documentos/distr_saude.xlsx")

ggplot(distr_saude) + 
  geom_line(aes(x = ano , y = num_proc)) +
  geom_smooth(aes(x = ano , y = num_proc), method = "lm") +
  labs(x = "Ano",
       y = "Processos de Alimentos",
       title = "Processos Envolvendo o Direito a Alimentos")

# Um incremento de 85,5% entre agosto de 2017 a julho de 2022
distr_saude <- saude |> 
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.)

m12 <- agrupa_12(distr_saude)

sum(m12$num_proc)

(m12$num_proc[5]/m12$num_proc[1] - 1)*100

rm(distr_saude, m12)

##################################### FIM ######################################