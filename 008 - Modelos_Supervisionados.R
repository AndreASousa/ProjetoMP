################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse",
             "nortest", # Teste de distribuição de variáveis
             "olsrr" # Diagnóstico de multicolinearidade e heterocelasticidade
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
#                        Combinando Bases de Dados                             #
################################################################################

load(here("Documentos", "Distribuicao_df.RData"))
load(here("Documentos", "codigos_sp.RData"))
load(here("Documentos", "indicadores.RData"))

ajuizamento <- distribuicao_df |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         propositura %in% as.character(2006:2020),
         comarca != "Competência Originária",
         tipo %in% c("AJ","AP", "AJ", "HDATA", "MI", "RECURSO","REEXAME")) |>
  distinct(processo, .keep_all = TRUE) |>
  select(propositura, comarca) |>
  arrange(propositura, comarca)

# Processos de duas Comarcas não alcançaram a Procuradoria
length(unique(ajuizamento$comarca))

# São as Jurisdições de Icanga e Itupeva
comarca_zero <- unique(indicadores$comarca[!indicadores$comarca %in% ajuizamento$comarca])

# São comarcas com população estimada de 12.002 e 64.330 habitantes em 2021
filter(populacao, ano == 2021) |>
  left_join(municipios_sp, by = "id") |>
  group_by(comarca) |>
  summarise(populacao = sum(populacao)) |>
  arrange(populacao) |>
  filter(comarca %in% c("Iacanga", "Itupeva"))

# Itupeva foi foro distrital da Comarca de Jundiaí até 2016
# https://www.itupeva.sp.gov.br/prefeitura/jurisdicao

# O Foro de IAcanga foi inaugurado em 2015
# https://www.tjsp.jus.br/Noticias/Noticia?codigoNoticia=26252

# Contando o número de processos ajuizados em cada comarca a cada ano
ajuizamento <- ajuizamento |>
  group_by(propositura, comarca) |>
  # Conta o número de processos ajuizados em cada comarca a cada ano
  summarise(num_proc = n()) |>
  ungroup () %>% droplevels(.)

# Adicionando o valor de 0 para a contagem de Icanga e Itupeva
zero <- data.frame(
  propositura = rep(2006:2020, length(comarca_zero)),
  comarca = comarca_zero,
  num_proc = 0)

ajuizamento <- rbind(ajuizamento, zero)

ajuizamento <- ajuizamento|>
  left_join(indicadores, by = c("propositura" = "ano", "comarca")) |>
  select(num_proc, everything(), -c(propositura, comarca))

# Estatísticas descritivas univariadas e tabela de frequências
summary(ajuizamento)

# Matriz de Correlações
rho_ajuizamento <- cor(saude)
rho_ajuizamento[1, ]

################################################################################
#                   Estimação por Regressão Linear Múltipla                    #
################################################################################

# Modelagem com todas as variáveis
ajuizamento_rl <- lm(num_proc ~ . , ajuizamento)

# Parâmetros do modelo
summary(ajuizamento_rl)

#procedimento "stepwise"
ajuizamento_step <- step(ajuizamento_rl, k = 3.841459)

summary(ajuizamento_step)

# Em estudo
anova(ajuizamento_step)
# "F value" = é o F calculado. Queremos saber se sua posição no eixo das abssissas
# está à frente ou atrás do Fcrítico (que separa 95% de confiança para o curso)
# Queremos excluir a hipótese nula (F calculado < F crítico ou p-value > 0,05)

# A Hipótese H1 (F calculado > F crítico ou P-value < 0,05 - a área sobre a curva é menor que 5%)
# Queremos saber se o F calculado é estatísticamente diferente de 0 a 95% de confiança.

# "Pr" = probability value - é a área embaixo da curva à direita do F calculado

# Teste de Shapiro-Francia (Aderência dos resíduos à normalidade)
sf.test(ajuizamento_step$residuals)

#Plotando os resíduos do modelo com a cuva normal teórica
ajuizamento |>
  mutate(residuos = ajuizamento_step$residuals) |>
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ajuizamento_step$residuals),
                            sd = sd(ajuizamento_step$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

# Kernel density estimation (KDE) - forma não-paramétrica para estimar a
# função densidade de probabilidade de uma variável aleatória
ajuizamento |>
  ggplot() +
  geom_density(aes(x = ajuizamento_step$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()

##################################################################################
#                        DIAGNÓSTICO DE HETEROCEDASTICIDADE                      #
##################################################################################

# Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(ajuizamento_step)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

#"Prob > Chi2"  é o p-value

# Adicionando fitted values e resíduos do modelo 'step wise' na tabela "ajuizamentos"
ajuizamento$fitted_step <- ajuizamento_step$fitted.values
ajuizamento$residuos_step <- ajuizamento_step$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_planosaude'
ajuizamento |>
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

class(ajuizamento_step)

#Teste de Multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(ajuizamento_step)

#A Tolerância varia de 0 a 1 e queremos que seja próxima de 1

#VIF (Variance Inflation Factor) = 1/tolerância
#VIF varia de 1 a + infinito

################################################################################
#                    Estudando as Ações de Alimento                            #
################################################################################
load(here("Documentos", "alimento.RData"))

# Vinculando cada processo com origem em 1ª instância a sua 
  # respecticva comarca
alimento <- alimento |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         propositura %in% as.character(2006:2020),
         comarca != "Competência Originária",
         tipo %in% c("AJ","AP", "AJ", "HDATA", "MI", "RECURSO","REEXAME")) |>
  distinct(processo, .keep_all = TRUE) |>
  select(propositura, comarca) |>
  arrange(propositura, comarca)

# Comarcas sem causas ajuizadas: Icanga e Itupeva
comarca_zero <- unique(indicadores$comarca[!indicadores$comarca %in% alimento$comarca])

# Agrupando o número de processos ajuizados por ano e comarca:
alimento <- alimento |>
  group_by(propositura, comarca) |>
  # Conta o número de processos ajuizados em cada comarca a cada ano
  summarise(num_proc = n()) |>
  ungroup () %>% droplevels(.)  

# Adicionando o valor de 0 para a contagem de Icanga e Itupeva
zero <- data.frame(
  propositura = rep(2006:2020, length(comarca_zero)),
  comarca = comarca_zero,
  num_proc = 0)

alimento <- rbind(alimento, zero)

# Vinculando os indicadores socioeconômicos aos processos ajuizados
alimento <- alimento |>
  left_join(indicadores, by = c("propositura" = "ano", "comarca")) |>
  select(num_proc, everything(), -c(propositura, comarca))

# Estatísticas descritivas univariadas e tabela de frequências
summary(alimento)

# Matriz de Correlações
rho_alimento <- cor(alimento)
rho_alimento[1, ]

# Modelagem com todas as variáveis
alimento_rl <- lm(num_proc ~ populacao, alimento)

# Parâmetros do modelo
summary(alimento_rl)

# Somando o número de pessoas menores de 20 em cada município
alimento <- alimento |>
  mutate(menor20 = rowSums(alimento[, 5:8]))

alimento_rl <- lm(num_proc ~ menor20, alimento)
summary(alimento_rl)

# Somando o número de pessoas menores de 25 em cada município
alimento <- alimento |>
  mutate(menor25 = rowSums(alimento[, 5:9]))

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
         tipo %in% c("AJ","AP", "AJ", "HDATA", "MI", "RECURSO","REEXAME")) |>
  distinct(processo, .keep_all = TRUE) |>
  select(propositura, comarca) |>
  arrange(propositura, comarca)

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
  select(num_proc, everything(), -c(propositura, comarca))

# Estatísticas descritivas univariadas e tabela de frequências
summary(saude)

# Matriz de Correlações
rho_saude <- cor(saude)
rho_saude[1, ]
psych::cortest.bartlett(R = rho_saude)

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

