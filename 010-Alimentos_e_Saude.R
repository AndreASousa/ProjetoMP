################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse",
             "lubridate", # utilizado para separar dados por mês e ano
             "MASS", # Para rodar modelos do tipo binomial negativo
             "fastDummies", # Transforma var categóricas em dummies
             "correlation", # matriz de correlações e p-valores
             "lmtest" # Teste da Razão de Verosimilhança
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

# Armazena indicadores de qualidade dos modelos estimados
qualidade <- function(glm = NULL, df = qual_modelo){
  
  #Captura o nome do modelo
  modelo <- deparse(substitute(glm))
  
  #Calcula a Máxima Verossimilhança
  #Exige-se uma função diferente para os modelos do pacote VGAM:
  if(typeof(glm) == "S4"){
    logl <- VGAM::lrtest(glm)
    graus <- logl@Body[1,1]
    veross <- logl@Body[1,2]
  } else{
    logl <- lmtest::lrtest(glm)
    graus <- logl[1,1]
    veross <- logl[1,2]
  }
  akaike <- AIC(glm)
  if(!is.null(deviance(glm))){
    rse <- sqrt(deviance(glm)/df.residual(glm))
  } else {
    rse <- NA
  }
  
  linha <- data.frame("Modelo" = modelo, "df" = graus, "ll" = veross, "AIC" = akaike, "RSE" = rse)
  
  df <- df |>
    rbind(linha)
  
  return(df)
}

qual_modelo <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(qual_modelo) <- c("Modelo", "df", "ll", "AIC", "RSE")

################################################################################
#                         Criando as Tabelas                                   #
################################################################################

# Tabela que vincula as comarcas aos códigos de unidade de origem (OOOO)
  # (Criado em: 003 - Municipios_e_Comarcas.R)
load(here::here("Documentos", "codigos_sp.RData"))

# Tabela com os indicadores socioeconômicos das comarcas paulistas
  # (Criado em: 004 - Indicadores_Socioeconomicos.R)
load(here("Documentos", "indicadores.RData"))

# Tabela com as ações que versam sobre o direito a alimentos.
  # (Criado em: 005 - Assuntos_do_Processo.R)
load(here("Documentos", "alimento.RData"))

# Tabela com as ações que versam sobre o direito à Saúde
  # (Criado em: 005 - Assuntos_do_Processo.R)
load(here("Documentos", "saude.RData"))

# Tabela com as Comarcas divididas em Clusters
  # (Criado em: 009 - Analise_de_Agrupamento.R)
  # Já está definidco como variável categórica (factor)
load(here("Documentos", "cluster.RData"))


#1 - Vincula cada processo à comarca de origem --------------------------------- 
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

saude <- saude |>
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


#2 - Atribuindo o valor de 0 ajuizamentos para observações ausentes na base ----
comarca <- unique(codigos$comarca)
comarca <- comarca[comarca != "Competência Originária"]

zero <- data.frame(
  propositura = rep(2017:2020, times = 320),
  comarca = rep(comarca, each = 4))

zero_alimento <- left_join(zero, alimento, by = c("propositura", "comarca"))
zero_saude <- left_join(zero, saude, by = c("propositura", "comarca"))

zero_alimento$num_proc[is.na(zero_alimento$num_proc)] <- 0
zero_saude$num_proc[is.na(zero_saude$num_proc)] <- 0

alimento <- zero_alimento
saude <- zero_saude

rm(comarca, zero, zero_alimento, zero_saude)

#3 - Vincula os indicadores socioeconômicos ------------------------------------
alimento <- alimento |>
  left_join(indicadores, by = c("propositura" = "ano", "comarca"))

saude <- saude |>
  left_join(indicadores, by = c("propositura" = "ano", "comarca"))

rm(indicadores, codigos)

################################################################################
#                    Estudando as Ações de Alimentos                           #
################################################################################

# Estatísticas descritivas univariadas e tabela de frequências
summary(alimento$num_proc)

# Matriz de Correlações

rho_alimento <- cor(alimento[, c(-1, -2)])
rho_alimento[1, ]

variaveis <- c("num_proc", "divorcios", "n_filhos", "f0a4", "f5a9", "f10a14",
               "f15a19", "f20a24", "f25a29", "f30a34", "f35a39", "f40a44",
               "f45a49", "f50a54", "f55a59", "f60a64", "f65a69", "f70a74",
               "f75a79", "f80mais")

correlation(alimento[ , variaveis])[1:19, ]

cor.test(alimento$num_proc, alimento$f0a4)

# Será que a correlação muda ao excluirmos São Paulo?
teste <- filter(alimento, comarca != "São Paulo")
rho_teste <- cor(teste[, c(-1, -2)])
rho_teste[1, ]

correlation(teste[ , variaveis])[1:19, ]
cor.test(teste$num_proc, teste$salario)

#1 - Cria novas faixas etárias e adiciona os clusters --------------------------

alimento <- alimento |>
  mutate(menor20 = rowSums(alimento[ , c("f0a4", "f5a9", "f10a14","f15a19")]))|>
  mutate(menor25 = rowSums(alimento[, c("f0a4", "f5a9", "f10a14","f15a19", "f20a24")])) |>
  mutate(f25a39 = rowSums(alimento[, c("f25a29", "f30a34", "f35a39")])) |>
  # Queremos a relação com o tamanho da população
  mutate(across(c("menor20", "menor25", "f25a39", "divorcios", "n_filhos"), .fns = ~./ populacao)) |>
  left_join(cluster, by = "comarca")|>
  dplyr::select(comarca, num_proc, menor20, menor25, f25a39, divorcios, n_filhos, cluster_k) |>
  rename(cluster = cluster_k)
  
# Procedimento n-1 dummies
alimento <- dummy_columns(.data = alimento,
                           select_columns = "cluster",
                           remove_selected_columns = T,
                           remove_most_frequent_dummy = T)  
  

#2 - Regressões Simples Binomial Negativo --------------------------------------
bneg_menor20 <- glm.nb(num_proc ~ menor20,
                      data = alimento)

qual_modelo <- qualidade(bneg_menor20)

bneg_menor25 <- glm.nb(num_proc ~ menor25,
                       data = alimento)

qual_modelo <- qualidade(bneg_menor25)

bneg_25a39 <- glm.nb(num_proc ~ f25a39,
                       data = alimento)

qual_modelo <- qualidade(bneg_25a39)

bneg_divorcio <- glm.nb(num_proc ~ divorcios,
                        data = alimento)

qual_modelo <- qualidade(bneg_divorcio)

# Número total de filhos dos casais divorciados / população
bneg_filhos <- glm.nb(num_proc ~ n_filhos,
                        data = alimento)

qual_modelo <- qualidade(bneg_filhos)
(qual_modelo <- arrange(qual_modelo, desc(ll), desc(AIC), desc(RSE)))
  
# Teste de razão de verossimilhança
lrtest(bneg_25a39, bneg_menor25)


#3 - Regressões Simples Binomial Negativo - Sem Sâo Paulo ----------------------
alimento_exsp <- filter(alimento, comarca != "São Paulo")

bneg_menor20_exsp <- glm.nb(num_proc ~ menor20,
                       data = alimento_exsp)

qual_modelo <- qualidade(bneg_menor20_exsp)

bneg_menor25_exsp <- glm.nb(num_proc ~ menor25,
                       data = alimento_exsp)

qual_modelo <- qualidade(bneg_menor25_exsp)

bneg_25a39_exsp <- glm.nb(num_proc ~ f25a39,
                     data = alimento_exsp)

qual_modelo <- qualidade(bneg_25a39_exsp)

bneg_divorcio_exsp <- glm.nb(num_proc ~ divorcios,
                        data = alimento_exsp)

qual_modelo <- qualidade(bneg_divorcio_exsp)

bneg_filhos_exsp <- glm.nb(num_proc ~ n_filhos,
                      data = alimento_exsp)

qual_modelo <- qualidade(bneg_filhos_exsp)
(qual_modelo <- arrange(qual_modelo, desc(ll), desc(AIC), desc(RSE)))

# Teste de razão de verossimilhança
lrtest(bneg_25a39_exsp, bneg_menor25_exsp)


#4 - Regressões Binomial Negativo Com  Clusters --------------------------------
bneg_menor20_dummie <- glm.nb(num_proc ~ menor20 + cluster_2 + cluster_3 + cluster_4 + cluster_5,
                       data = alimento)

qual_modelo <- qualidade(bneg_menor20_dummie)

bneg_menor25_dummie <- glm.nb(num_proc ~ menor25 + cluster_2 + cluster_3 + cluster_4 + cluster_5,
                       data = alimento)

qual_modelo <- qualidade(bneg_menor25_dummie)

bneg_25a39_dummie <- glm.nb(num_proc ~ f25a39 + cluster_2 + cluster_3 + cluster_4 + cluster_5,
                     data = alimento)

qual_modelo <- qualidade(bneg_25a39_dummie)

bneg_divorcio_dummie <- glm.nb(num_proc ~ divorcios + cluster_2 + cluster_3 + cluster_4 + cluster_5,
                        data = alimento)

qual_modelo <- qualidade(bneg_divorcio_dummie)

bneg_filhos_dummie <- glm.nb(num_proc ~ n_filhos + cluster_2 + cluster_3 + cluster_4 + cluster_5,
                      data = alimento)

qual_modelo <- qualidade(bneg_filhos_dummie)

(qual_modelo <- arrange(qual_modelo, desc(ll), desc(AIC), desc(RSE)))

# Teste de razão de verossimilhança
lrtest(bneg_25a39_dummie, bneg_menor25_dummie)


#3 - Regressões Bneg Multipla --------------------------------------------------

rm_bneg <- glm.nb(num_proc ~ .,
                  data = alimento[ , 2:length(alimento)])

# Procedimento "stepwise" - Excluiu o salário médio
rm_bneg <- step(rm_bneg, k = 3.841459)

summary(rm_bneg)

qual_modelo <- qualidade(rm_bneg)


################################################################################
#                      Estudando as Ações de Saúde                             #
################################################################################

# Estatísticas descritivas univariadas e tabela de frequências
summary(saude$num_proc)

# Matriz de Correlações
rho_saude <- cor(saude)
rho_saude[1, ]

# Cria novas faixas etárias e adiciona os clusters

saude <- saude |>
  mutate(menor20 = rowSums(alimento[ , c("f0a4", "f5a9", "f10a14","f15a19")]))|>
  mutate(menor25 = rowSums(alimento[, c("f0a4", "f5a9", "f10a14","f15a19", "f20a24")])) |>
  mutate(f25a39 = rowSums(alimento[, c("f25a29", "f30a34", "f35a39")])) |>
  # Queremos a relação com o tamanho da população
  mutate(across(c("menor20", "menor25", "f25a39", "divorcios", "n_filhos"), .fns = ~./ populacao)) |>
  left_join(cluster, by = "comarca")|>
  dplyr::select(comarca, num_proc, menor20, menor25, f25a39, divorcios, n_filhos, cluster_k) |>
  rename(cluster = cluster_k)

# n-1 Dummies
saude <- dummy_columns(.data = saude,
                          select_columns = "cluster",
                          remove_selected_columns = T,
                          remove_most_frequent_dummy = T)  

anova(saude_step)
sf.test(saude_step$residuals)

chart.Correlation(alimento)

##################################### FIM ######################################