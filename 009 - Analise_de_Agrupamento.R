# Desejamos agrupar as comarcas de acordo com suas características
  # socioeconômicas.

# Os clusters serão formados apenas a partir das variáveis explicativas
  # que pretendemos utlizar para compreender o número de ajuizamentos.

# A Variável dependente (número de processos por comarca a cada ano) 
  # não será utilizada.

# Isso poderia gerar viés na estimação dos coeficientes angulares.
  # Parte da variância da variável "Y" também seria colocada no
  # lado direito da equação.


################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse", # pacote para manipulacao de dados
             "factoextra", # algoritmo de cluster e visualizacao
             "fpc", # algoritmo de cluster e visualizacao
             "gridExtra", #para a funcao grid arrange
             "MASS", # Para rodar modelos do tipo binomial negativo
             "pscl", # Modelos "Zero-Inflated" e Teste de Vuong
             "VGAM", # Modelo de Regressão para distribuição de Pareto
             "car", # PowerTransform (transformar boxcox em modelos não lineares)
             "EnvStats" # boxcoxTransform()
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

################################################################################
#                             Análise de Agrupamento                           #
################################################################################

# Tabela com o número de processos sentenciados por ano de ajuizamento e Comarca.
  # (Criado em: 008 - Modelos_GLM.R)
load(here::here("Documentos", "ajuizamento.RData"))


#1 - Criando a Tabela ----------------------------------------------------------
  # Seleciona os indicadores correspondentes a 2019
  # Caso contrário, teriamos 4 observações para cada município
comarcas <- ajuizamento |>
  filter(propositura == 2019) |>
  dplyr::select(comarca, populacao, salario, ocupados, pib_pc, idh) |>
  as.data.frame()
  
# Transformando os nomes de comarcas em nomes de linhas
rownames(comarcas) <- comarcas$comarca
comarcas$comarca <- NULL

# Padronizando as variáveis
comarcas <- scale(comarcas) |>
  as.data.frame()


#2 - Elaborando um cluste hierárquico ---------------------------------------------

# Como o volume de dados não é muito grande,
  # Optamos pelo cluster hierárquico

# Calculando a matriz de distancias euclidianas
distancia <- dist(comarcas, method = "euclidean")

#Calculando os Clusters
  # metodos disponiveis: "average", "single", "complete" e "ward.D"
cluster.hierarquico <- hclust(distancia, method = "single")

# Verificando o Elbow 
fviz_nbclust(comarcas, FUN = hcut, method = "wss")

# Criando 4 grupos de Comarcas
cluster <- cutree(cluster.hierarquico, k = 4) |>
  data.frame()
names(cluster) <- "cluster"

table(cluster)

# Criando uma tabela com os clusters e comarcas
cluster <- tibble::rownames_to_column(cluster, "comarca")

# Vejamos como ficou cada grupo:
cluster |>
  filter(cluster == 4)


################################################################################
#                    Agrupamento com Pesos Diferentes                          #
################################################################################

# As variáveis explicativas não influem de forma equânime na predição do 
  # número de processos. 

# A população da comarca é mais relevante que o PIB per capta, por exemplo.
  # A análise de agrupamentos com variáveis padronizadas assume a igualdade de
  # importância e resulta em separações que não fazem tanto sentido do 
  # ponto de vista preditivo

# Vamos atribuir pesos diferentes para cada variável. Para estimá-los,
  # faremos uma regressão múltipla sobre valores padronizados e extrairemos
  # os coeficientes angulares.

# Optamos pelo medor modelo de regressão rodado em 008 - Modelos_GLM.R,
  # Medido pelo ll e AIC

#1 - Definindo Pesos
auxiliar <- ajuizamento |>
  dplyr::select(num_proc, populacao, salario, idh, pib_pc, ocupados)
  
# A variável dependente precisa continuar discreta
auxiliar[ , 2:6] <- scale(auxiliar[ , 2:6])
  
rm_bneg <- glm.nb(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados,
                  data = auxiliar)

#procedimento "stepwise"
rm_bneg <- step(rm_bneg, k = 3.841459)

# Parâmetros do modelo
summary(rm_bneg)

# Ocupados não é estatisticamente relevante
rm_bneg <- glm.nb(formula = num_proc ~  populacao + salario + pib_pc + idh,
                  data = auxiliar)

# Parâmetros do modelo
summary(rm_bneg)

# Capturando o coeficientes angulares
pesos <- rm_bneg$coefficients[2:5] |>
  abs() # Números absoutos

comarcas <- comarcas |>
  dplyr::select(!ocupados)

# Os nomes das variáveis estão na mesma ordem
names(pesos) == names(comarcas)

# Podemos multiplicar os pesos da seguinte forma
comarcas <- comarcas * pesos[col(comarcas)]


#2 - Cluster Não Hierárquico - K-Means

k2 <- kmeans(comarcas, centers = 2) # 2 centróides
k3 <- kmeans(comarcas, centers = 3) # 3 centroides
k4 <- kmeans(comarcas, centers = 4) # ...
k5 <- kmeans(comarcas, centers = 5)
k6 <- kmeans(comarcas, centers = 6)

# Criar graficos
g2 <- fviz_cluster(k2, geom = "point", data = comarcas) + ggtitle("k = 2")
g3 <- fviz_cluster(k3, geom = "point", data = comarcas) + ggtitle("k = 3")
g4 <- fviz_cluster(k4, geom = "point", data = comarcas) + ggtitle("k = 4")
g5 <- fviz_cluster(k5, geom = "point", data = comarcas) + ggtitle("k = 5")
g6 <- fviz_cluster(k6, geom = "point", data = comarcas) + ggtitle("k = 6")

# Imprime os gráficos na mesma tela
grid.arrange(g2, g3, g4, g5, g6, nrow = 2)

# Verificando o Elbow 
fviz_nbclust(comarcas, FUN = kmeans, method = "wss")

data.frame(k4$cluster)

# Criando 4 grupos de Comarcas
cluster <- k4$cluster |>
  data.frame()
names(cluster) <- "cluster"

table(cluster)

# Nomes de Comarca como coluna
cluster <- tibble::rownames_to_column(cluster, "comarca")

# Transformando em variável categórica
cluster$cluster <- as.factor(cluster$cluster)

# Vejamos como ficou cada grupo:
cluster |>
  filter(cluster == 1)

rm(auxiliar, cluster.hierarquico, comarcas, g2, g3, g4, g5,g6,
   k2, k3, k4, k5, k6, rm_bneg)

save(cluster, file="Documentos/cluster.RData")

################################################################################
#                    Estimando um Modelo com Clusters                          #
################################################################################

# Tabela com os indicadores de qualidade dos modelos estimados.
  # (Criado em: 008 - Modelos_GLM.R)
load(here::here("Documentos", "qualidade_modelo.RData"))

# Criando a base
variaveis <- ajuizamento |>
  left_join(cluster, by = "comarca") |>
  dplyr::select(cluster, num_proc, populacao, salario, idh, pib_pc)

rm(ajuizamento, cluster)

# Procedimento n-1 dummies
variaveis <- dummy_columns(.data = variaveis,
                                      select_columns = "cluster",
                                      remove_selected_columns = T,
                                      remove_most_frequent_dummy = T)

#1 - Número de Processos ~ População e Clusters --------------------------------
rs_dummie <- lm(num_proc ~ populacao + cluster_1 + cluster_3 + cluster_4,
                 data = variaveis)

# Procedimento "stepwise"
rs_dummie <- step(rs_dummie, k = 3.841459)

# Parâmetros do modelo
summary(rs_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_dummie)


#2 - Número de Processos ~ Todas as Variáveis ----------------------------------
rlm_dummie <- lm(num_proc ~ ., variaveis)

# Procedimento "stepwise"
rlm_dummie <- step(rlm_dummie, k = 3.841459)

summary(rlm_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rlm_dummie)


#3 - Modelo de Poisson - Número de Processos ~ População e Clusters-------------
rs_poisson_dummie <- glm(formula = num_proc ~ populacao + cluster_1 + cluster_3 + cluster_4,
                         data = variaveis,
                         family = "poisson")

# Procedimento "stepwise"
rs_poisson_dummie <- step(rs_poisson_dummie, k = 3.841459)

# Parâmetros do modelo
summary(rs_poisson_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_poisson_dummie)


#4 - Modelo de Poisson - Número de Processos ~ Todas as Variáveis --------------
rm_poisson_dummie <- glm(formula = num_proc ~ .,
                         data = variaveis,
                         family = "poisson")

# Procedimento "stepwise"
rm_poisson_dummie <- step(rm_poisson_dummie, k = 3.841459)

summary(rm_poisson_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_poisson_dummie)


#5 - Modelo Binomial Negativo - Número de Processos ~ População e Clusters------
rs_bneg_dummie <- glm.nb(formula = num_proc ~ populacao + cluster_1 + cluster_3 + cluster_4,
                         data = variaveis)

# Procedimento "stepwise"
rs_bneg_dummie <- step(rs_bneg_dummie, k = 3.841459)

# Parâmetros do modelo
summary(rs_bneg_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_bneg_dummie)


#6 - Modelo Binomial Negativo - Número de Processos ~ Todas as Variáveis -------
rm_bneg_dummie <- glm.nb(formula = num_proc ~ .,
                         data = variaveis)

# Procedimento "stepwise"
rm_bneg_dummie <- step(rm_bneg_dummie, k = 3.841459)

# Parâmetros do modelo
summary(rm_bneg_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_bneg_dummie)


#7 - Modelo Zero-Inflated Binomial Negativo - Número de Processos ~ População e Clusters
rs_zero_bneg_dummie <- zeroinfl(formula = num_proc ~ populacao + cluster_1 + cluster_3 + cluster_4
                             | populacao, #pipe
                             data = variaveis,
                             dist = "negbin")
summary(rs_zero_bneg_dummie)

# Teste de Vuong
vuong(m1 = rs_bneg_dummie, m2 = rs_zero_bneg_dummie)

# Procedimento "stepwise"
rs_zero_bneg_dummie <- step(rs_zero_bneg_dummie, k = 3.841459)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_zero_bneg_dummie)


#8 - Modelo ZINB - Número de Processos ~ Todas as Variáveis --------------------
rm_zero_bneg_dummie <- zeroinfl(formula = num_proc ~  .
                      | populacao, #pipe
                      data = variaveis,
                      dist = "negbin")
summary(rm_zero_bneg_dummie)

# Teste de Vuong
vuong(m1 = rm_bneg_dummie, m2 = rm_zero_bneg_dummie)

# Procedimento "stepwise"
rm_zero_bneg_dummie <- step(rm_zero_bneg_dummie, k = 3.841459)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_zero_bneg_dummie)


#9 - Modelo Pareto - Número de Processos ~ População e Clusters-----------------

rs_pareto_dummie <- vglm(num_proc ~ populacao + cluster_1 + cluster_3 + cluster_4,
                  family = VGAM::gpd(threshold = -0.0001),
                  data = variaveis)


summary(rs_pareto_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_pareto_dummie)


#10  - Modelo Pareto - Número de Processos ~ Todas as Variáveis ----------------

rm_pareto_dummie <- vglm(num_proc ~ .,
                       family = VGAM::gpd(threshold = -0.0001),
                       data = variaveis)

summary(rm_pareto_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_pareto_dummie)


# 11 - Modelo Binomial Negativo - População Transformada -----------------------

lambda <- powerTransform(variaveis$populacao)$lambda

pop_lambda <- variaveis |>
  mutate(populacao =  boxcoxTransform(variaveis$populacao, lambda = lambda))

rs_bneg_box_dummie <- glm.nb(num_proc ~ populacao + cluster_1 + cluster_3 + cluster_4,
                      data = pop_lambda)

# Procedimento "stepwise"
rs_bneg_box_dummie <- step(rs_bneg_box_dummie, k = 3.841459)

summary(rs_bneg_box_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_bneg_box_dummie)


# 12 - Modelo Binomial Negativo - População Transformada e todas variáveis -----
rm_bneg_box_dummie <- glm.nb(num_proc ~ .,
                      data = pop_lambda)

# Procedimento "stepwise"
rm_bneg_box_dummie <- step(rm_bneg_box_dummie, k = 3.841459)

summary(rm_bneg_box_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_bneg_box_dummie)


#13 - Modelo ZINB - População Transformada--------------------------------------
rs_zero_bneg_box_dummie <- zeroinfl(formula = num_proc ~ populacao + cluster_1 + cluster_3 + cluster_4
                             | populacao, #pipe
                             data = pop_lambda,
                             dist = "negbin")

# Teste de Vuong
vuong(m1 = rs_bneg_box_dummie, m2 = rs_zero_bneg_box_dummie)

# Procedimento "stepwise"
rs_zero_bneg_box_dummie <- step(rs_zero_bneg_box_dummie, k = 3.841459)

summary(rs_zero_bneg_box_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_zero_bneg_box_dummie)


#14 - Modelo ZINB - População Transformada e todas variáveis -------------------
rm_zero_bneg_box_dummie <- zeroinfl(formula = num_proc ~  .
                             | populacao, #pipe
                             data = pop_lambda,
                             dist = "negbin")

# Teste de Vuong
vuong(m1 = rm_bneg_box_dummie, m2 = rm_zero_bneg_box_dummie)

#procedimento "stepwise"
rm_zero_bneg_box_dummie <- step(rm_zero_bneg_box_dummie, k = 3.841459)

summary(rm_zero_bneg_box_dummie)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_zero_bneg_box_dummie)



save(qual_modelo, file="Documentos/qualidade_modelo2.RData")

##################################### FIM ######################################
