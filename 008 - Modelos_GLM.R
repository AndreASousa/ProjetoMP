# load(here("Documentos", "ajuizamento.RData"))
# load(here("Documentos", "ajuizamento_exsp.RData"))
# load(here("Documentos", "ajuizamento_bc.RData"))
# load(here("Documentos", "ajuizamento_bc_exsp.RData"))
# load(here("Documentos", "ajuizamento_fat.RData"))
# load(here("Documentos", "ajuizamento_fat_exsp.RData"))


################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse", # pacote para manipulacao de dados
             "plotly", 
             "nortest", # Teste de distribuição de variáveis
             "olsrr", # Diagnóstico de multicolinearidade e heterocelasticidade
             
             "fitdistrplus", # auxilia a identificar a distribuição que melhor se adequa
             # aos dados 
            
             "gamlss",# testa a aderência a diferentes distribuiçoes e retorna
             # aquela que minimiza o AIC
             
             "EnvStats", # teste de aderência à distribuição
             "correlation", # Combina os coeficientes de correlação e testes
             "lmtest", # likelihood ratio test para comparação de LL's entre modelos
             "correlation", # matriz de correlações e p-valores
             "reshape2",
             "psych", # Análise fatorial
             "kableExtra", # Vizualização da tabela
             "factoextra", # extração e vizualização os eigenvalues
             "ggrepel",
             "car", # PowerTransform (transformar boxcox em modelos não lineares)
             "MASS", # Para rodar modelos do tipo binomial negativo
             "pscl", # Modelos "Zero-Inflated" e Teste de Vuong
             "VGAM", # Modelo de Regressão para distribuição de Pareto
             "openxlsx" # manipula arquivos ".xlsx"
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
#                  Criando a Tabela com dados de Contagem                      #
################################################################################

# Tabela com códigos de unidades judiciárias e nome das comarcas
  # (Criado em: 003 - Municipios_e_Comarcas.R)
load(here("Documentos", "codigos_sp.RData"))

# Tabela com os indicadores socioeconômicos das comarcas paulistas
  # (Criado em: 004 - Indicadores_Socioeconomicos.R)
load(here("Documentos", "indicadores.RData"))

# Tabela com os processos que ingressaram na procuradoria em sede de
  # recurso contra sentença (Criado em: 006 - Analise_Quantitativa.R)
load(here("Documentos", "Recurso_contra_sentenca.RData"))

#1 - Criando a Base de dados de processos sentenciados entre 2017 e 2020--------

# Optamos por esse perído para evitar o viés causado pelos processos que já
  # ingressaram antes do início temporal da base de dados, ou os que foram 
  # ajuizados em 2021 e 2022 e que ainda tramitam em primeira instância;
# Caso, contrário, poderiams chegar a conclusões equivocadas, como supor que
  # o númeor de feitos sentenciados era muito menor antes de 2017 ou que caiu
  # abruptamente em 2021 e 2022 simplesmente por não estarem capturados na
  # base de dados.

ajuizamento <- recurso_sentenca |>
  filter(propositura %in% as.character(2017:2020))|>
  dplyr::select(propositura, comarca) |>
  # Contando o número de processos ajuizados em cada comarca a cada ano
  group_by(propositura, comarca) |>
  # Conta o número de processos ajuizados em cada comarca a cada ano
  summarise(num_proc = n()) |>
  ungroup () %>% droplevels(.)  

# Adicionando o valor de 0 para o caso de nenhum processo ter ingressado na
# Procuradoria originado de uma comarca em um determinado ano

# Atenção!!! A transformação por box-cox exige valores positivos

comarca <- unique(codigos$comarca)
comarca <- comarca[comarca != "Competência Originária"]

zero <- data.frame(
  propositura = rep(2017:2020, times = 320),
  comarca = rep(comarca, each = 4))

zero <- left_join(zero, ajuizamento, by = c("propositura", "comarca"))

zero$num_proc[is.na(zero$num_proc)] <- 0

ajuizamento <- zero

# 11 observações receberam a adição de "0"
nrow(filter(ajuizamento, num_proc == 0))

#2 - Adicionando os indicadores socioeconômicos de cada comarca ----------------
ajuizamento <- ajuizamento|>
  left_join(indicadores, by = c("propositura" = "ano", "comarca")) |>
  dplyr::select(propositura, comarca, num_proc, everything())

rm(codigos, comarca, indicadores, recurso_sentenca, zero)

#3 - Tabela que exclui a Comarca da capital ------------------------------------
ajuizamento_exsp <- filter(ajuizamento, comarca != "São Paulo")

# Estatísticas descritivas univariadas e tabela de frequências
summary(ajuizamento)
summary(ajuizamento_exsp)

# São Paulo - Média de 5.591, 25% do total
ajuizamento |> 
  group_by(comarca) |>
  summarise(total = sum(num_proc),
            media = mean(num_proc))|>
  ungroup () %>% droplevels(.) |>
  mutate(percentual = total / sum(total)) |>
  arrange(desc(percentual))

# Estatísticas descritivas univariadas e tabela de frequências
summary(ajuizamento)
summary(ajuizamento_exsp)

# São Paulo - Média de 5.591, 25% do total
ajuizamento |> 
  group_by(comarca) |>
  summarise(total = sum(num_proc),
            media = mean(num_proc))|>
  ungroup () %>% droplevels(.) |>
  mutate(percentual = total / sum(total)) |>
  arrange(desc(percentual))

#4 - Transformando a variável dependente pelo lambda de Box-Cox ----------------
ajuizamento_bc <- ajuizamento |>
  filter(num_proc != 0)

lambda_bc <- powerTransform(ajuizamento_bc$num_proc)$lambda

ajuizamento_bc <- ajuizamento_bc |>
  mutate(bc =  boxcoxTransform(ajuizamento_bc$num_proc, lambda = lambda_bc)) |>
  dplyr::select(propositura, comarca, num_proc, bc, everything())

ajuizamento_bc_exsp <- ajuizamento_exsp |>
  filter(num_proc != 0)

lambda_bc_exsp <- powerTransform(ajuizamento_bc_exsp$num_proc)$lambda

ajuizamento_bc_exsp <- ajuizamento_bc_exsp |>
  mutate(bc =  boxcoxTransform(ajuizamento_bc_exsp$num_proc, lambda = lambda_bc_exsp)) |>
  dplyr::select(propositura, comarca, num_proc, bc, everything())

save(ajuizamento, file="Documentos/ajuizamento.RData")
save(ajuizamento_exsp, file="Documentos/ajuizamento_exsp.RData")
save(ajuizamento_bc, file="Documentos/ajuizamento_bc.RData")
save(ajuizamento_bc_exsp, file="Documentos/ajuizamento_bc_exsp.RData")

################################################################################
#                        Distribuição dos Dados                                #
################################################################################

# Visualizando a distribuição

plot(density(ajuizamento$num_proc), main = "Kernel Density das Causas Sentenciadas")

# Histograma da variável dependente
ggplotly(
  ajuizamento |>
    ggplot(aes(x = num_proc,
               fill = ..count..)) +
    geom_histogram(bins = round(2 * nrow(ajuizamento) ^ (1 / 3)),
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF",
                        high = "#FDE725FF") +
    labs(x = "Número de Processos",
         y = "Frequência") +
    theme_bw()
)

# Teste de Normalidade Shapiro-Wilk
  # p_valor < 0.5 - distribuição diferente da curva normal
shapiro.test(ajuizamento$num_proc)

# Estamos tratando de dados de contagem
  #Será que adere a Poisson ou Binomial Negativa?
  # Váriância mais de mil vezes superior à média
  # Não se adequa à distribuição de Poisson
var(ajuizamento$num_proc) / mean(ajuizamento$num_proc)
var(ajuizamento_exsp$num_proc) / mean(ajuizamento_exsp$num_proc)

# Tentando identificar da distribuição que melhor se adequa aos dados
# Pelo gráfico de Cullen Frey
# Dentre as possibildiades, mais se aproxima da Distribuição Beta
  fitdistrplus::descdist(ajuizamento$num_proc)

# utilizando a função gamlss::fitDist() para testar automaticamente
# diversas distribuições Retorna a que apresenta o menor AIC
# type = "realline" will test all distributions defined on the whole real line
# type = "realplus" will only try distributions defined on the real positive line.
# K = 2 indica que pretendemos selecionar de acordo com o menor AIC
fit <- fitDist(y = ajuizamento$num_proc, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

# Distruvyulçai de Pareto Tipo II
# AIC = 12597
# Pareto 2o = Ditribuição original de Pareto Tipo 2
# Sima = is the inverse of the sigma in code PARETO2()
  # and coresponse to the usual parameter alpha of the Patreto distribution
# sigma = scale
# mu = location
summary(fit)

# Estimando parametros pela máxima verossimilhança
gamlssML(formula = ajuizamento$num_proc, family = "PARETO2o")
# Distribuição Binomial Negativa
gamlssML(formula = ajuizamento$num_proc, family = "NBI")

# Estimando parâmetros para distribuição generalizada de pareto
eva::gpdFit(ajuizamento$num_proc, threshold = 4.32)

# Simulando uma distribuição de pareto com os parâmetros definidos
pareto <- rPARETO2o(n = 1280, mu = 4.32, sigma = 0.8345)
plot(density(pareto), col = "red")

# Simulando uma distribuição binomial negativo
negbin <- rNBI(n = 1280, mu = 4.241, sigma = 0.4685)
plot(density(negbin), col = "red")

mu <- 4.241
sigma <- 0.4685

# http://www.gamlss.com/wp-content/uploads/2013/01/gamlss-manual.pdf
# µ = média = (1-p)*r/p

# Parâmetro de Forma ou concentração (θ)
  # Número de fracassos
  # Var(Y) = µ + σµ2 = μ+μ2/θ = (1-p)*r/p^2
  # σµ2 = μ2/θ => σ = 1/θ => θ = 1/σ
theta <- 1/sigma

# Probabilidade de sucesso (p):
(p <- sigma / mu)
#Numero de Sucessos (r):
(r <- (p * mu) / (1 - p))

rm(pareto, negbin, mu, sigma, theta, p, r)

# POR FAZER!!!
# Teste Kolmogorov-Smirnov
# Admite apenas valores positivos e não repqtidos
# pvalue < 0.05 -> Hipótese H1, não adere à distribuição de pareto
# pvalue > 0.05 -> Hipótese H0, aderência à distribuição de pareto
teste <- filter(ajuizamento, num_proc > 0)|>
  dplyr::select(num_proc)  |>
  unique()
  
ks.test(x = teste, y = "ppareto", 4.32, 0.8345)
ks.test(x = teste, y = "ppareto", 4.32, 0.8345)
?ks.test

EnvStats::gofTest(teste, distribution = "pareto", test = "ks")

# http://soche.cl/chjs/volumes/09/01/Suarez-Espinosa_etal(2018).pdf

# Matriz de Correlações
rho_ajuizamento <- cor(ajuizamento[,3:8])
rho_ajuizamento
rho_ajuizamento[1, ]

# Teste de Correlação de Pearson
cor.test(ajuizamento$num_proc, ajuizamento$salario)

# Matriz de correlações e p-valores
correlation::correlation(ajuizamento[,3:8])

correlation::correlation(ajuizamento_exsp[,3:8])

# Será que a correlação muda ao selecionarmos apenas um ano?
teste <- filter(ajuizamento, propositura == "2018")
rho_teste <- cor(teste[,3:8])
rho_teste[1, ]

# Será que a correlação muda ao excluirmos São Paulo?
teste <- filter(ajuizamento, comarca != "São Paulo")
rho_teste <- cor(teste[, c(-1, -2)])
rho_teste[1, ]

correlation::correlation(teste[,c(-1, -2)])
cor.test(teste$num_proc, teste$salario)

rm(fit, teste, pareto, rho_ajuizamento)

# São Paulo distorce a análise
ggplot(data = ajuizamento) +
  geom_point(mapping = aes(x = num_proc, y = populacao)) 

#Sem São Paulo
ggplotly(ajuizamento |>
           filter(comarca != "São Paulo")|>
           ggplot() +
            geom_point(mapping = aes(x = num_proc, y = populacao), color = "#39568CFF", size = 2.5) +
            geom_smooth(mapping = aes(x = num_proc, y = populacao),
                  method = "lm", se = F, size = 2) +
      xlab("Distância") +
      ylab("Tempo") +
      scale_color_manual("Legenda:",
                         values = "grey50") +
      theme_classic()
  )

################################################################################
#                            Análise Fatorial                                  #
################################################################################

#1 - Scores Fatoriais para a base - inclusive São Paulo -----------------------

# Padronização das observações
  # O algoritmo psych::prcomp(), EXIGE que a a matriz de dados 
  # esteja padronizada pelo procedimento zscores:
dados_std <- ajuizamento[ , 4:length(ajuizamento)] |>
  scale() |> #Padroniza pelo Z-score
  data.frame() #Deixa em formato de df

# Teste de efericidade de Bartlett
  # Queremos que a matriz de correlaçãoes seja estatisticamente != 0
cortest.bartlett(R = dados_std)

# Rodando a PCA
afpc <- prcomp(dados_std)
summary(afpc)

# Sumarizando pontos importantes:
  # Calcula o eigenvalue e mostra a variância compartilhada e cumulativa
data.frame(eigenvalue = afpc$sdev ^ 2,
           var_compartilhada = summary(afpc)$importance[2,],
           var_cumulativa = summary(afpc)$importance[3,]) -> relatorio

relatorio |> 
  kable() |>
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)

# O objeto afpc possui os seguintes componentes:
afpc$sdev # (EIGENVALUES)^(1/2) -> desvio-pardãro dos componentes principais
afpc$rotation # EIGENVECTORS
afpc$center # Média padronizada das variáveis
afpc$scale # desvios-padrão de cada variável utilizadas para a padronização.

#Visualizando os pesos que cada variável tem em cada componente principal 
#obtido pela PCA (EIGENVECTORS)
data.frame(afpc$rotation) %>%
  mutate(var = names(dados_std)) %>%
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

# Extraindo as Cargas Fatoriais
k <- sum((afpc$sdev ^ 2) > 1) #Aplicando o critério de Keiser

if(k > 1){
  cargas_fatoriais <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k])
} else{
  cargas_fatoriais <- afpc$rotation[, 1] * (afpc$sdev[1])
}

# Relatório das CARGAS FATORIAIS e das COMUNALIDADES
  # Se a comunalidade da variável "salario" para dois fatores é 0.63,
  # Significa que apenas dois fatores capturaram 63% da variância dessa variável
data.frame(cargas_fatoriais) |>
  rename(F1 = X1,
         F2 = X2) |>
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() |>
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)

# Plotagem das Cargas Fatoriais
  #CUIDADO COM O ESPELHAMENTO!
data.frame(cargas_fatoriais) |>
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpc)$importance[2,1] * 100,
                                    digits = 2),
                              "%)")),
       y = paste("F2", paste0("(",
                              round(summary(afpc)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()

#'Note que F1 explica 83,81% do comportamento dos dados e possui alta correlação
  # com população, faixas etárias, ocupados, divórcios e número de filhos
# Por sua vez, F2 explica 9,59% do comportamento dos dados e possui correlação
# próxima a ZERO com essas mesmas variáveis

# ATENÇÃO, o R faz um ESPELHAMENTO horizontal e vertical para separar os fatores
# Espelhamento significa mudar o sinal em um plano cartesiano ("-x", "-y" etc)
# Talvez com o propósito de enfatizar a ideia de ortogonalidade entre os fatores

# Scores Fatoriais
scores_fatoriais <- t(afpc$rotation)/afpc$sdev
colnames(scores_fatoriais) <- colnames(dados_std)

# Visualizando os Scores Fatoriais
scores_fatoriais |>
  t() |>
  data.frame() |>
  rename(PC1 = 1,
         PC2 = 2) |>
  dplyr::select(PC1, PC2) |>
  kable() |>
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)

# Devemos efetuar a multiplicação por -1, 
# visto que os scores fatoriais das observações mais fortes são, por padrão, 
# apresentados acompanhados do sinal de menos.

F1 <- t(apply(dados_std, 1, function(x) x * scores_fatoriais[1,]))
F2 <- t(apply(dados_std, 1, function(x) x * scores_fatoriais[2,]))

F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * -1)

F2 <- data.frame(F2) %>%
  mutate(fator2 = rowSums(.) * -1)

ajuizamento_fat <- ajuizamento|>
  mutate(f1 = F1$fator1, f2 = F2$fator2) |>
  dplyr::select(propositura, comarca, num_proc, f1, f2)


#2 - Scores Fatoriais para a base - excluindo São Paulo ----------------------

# Padronização das observações
# O algoritmo psych::prcomp(), EXIGE que a a matriz de dados 
# esteja padronizada pelo procedimento zscores:
dados_std <- ajuizamento_exsp[ , 4:length(ajuizamento)] |>
  scale() |> #Padroniza pelo Z-score
  data.frame() #Deixa em formato de df

# Teste de efericidade de Bartlett
# Queremos que a matriz de correlaçãoes seja estatisticamente != 0
cortest.bartlett(R = dados_std)

# Rodando a PCA
afpc <- prcomp(dados_std)
summary(afpc)

# Sumarizando pontos importantes:
# Calcula o eigenvalue e mostra a variância compartilhada e cumulativa
data.frame(eigenvalue = afpc$sdev ^ 2,
           var_compartilhada = summary(afpc)$importance[2,],
           var_cumulativa = summary(afpc)$importance[3,]) -> relatorio

relatorio |> 
  kable() |>
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)

# O objeto afpc possui os seguintes componentes:
afpc$sdev # (EIGENVALUES)^(1/2) -> desvio-pardãro dos componentes principais
afpc$rotation # EIGENVECTORS
afpc$center # Média padronizada das variáveis
afpc$scale # desvios-padrão de cada variável utilizadas para a padronização.

#Visualizando os pesos que cada variável tem em cada componente principal 
#obtido pela PCA (EIGENVECTORS)
data.frame(afpc$rotation) %>%
  mutate(var = names(dados_std)) %>%
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

# Extraindo as Cargas Fatoriais
k <- sum((afpc$sdev ^ 2) > 1) #Aplicando o critério de Keiser

if(k > 1){
  cargas_fatoriais <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k])
} else{
  cargas_fatoriais <- afpc$rotation[, 1] * (afpc$sdev[1])
}

# Relatório das CARGAS FATORIAIS e das COMUNALIDADES
# Se a comunalidade da variável "salario" para dois fatores é 0.63,
# Significa que apenas dois fatores capturaram 63% da variância dessa variável
data.frame(cargas_fatoriais) |>
  rename(F1 = X1,
         F2 = X2) |>
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() |>
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)

# Plotagem das Cargas Fatoriais
#CUIDADO COM O ESPELHAMENTO!
data.frame(cargas_fatoriais) |>
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpc)$importance[2,1] * 100,
                                    digits = 2),
                              "%)")),
       y = paste("F2", paste0("(",
                              round(summary(afpc)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()

# Note que F1 explica 86,14% do comportamento dos dados e possui forte correlação
# com população, faixas etárias, ocupados, divórcios e número de filhos
# Por sua vez, F2 explica 8,45% do comportamento dos dados e possui correlação
# próxima a ZERO com essas mesmas variáveis

# Scores Fatoriais
scores_fatoriais <- t(afpc$rotation)/afpc$sdev
colnames(scores_fatoriais) <- colnames(dados_std)

# Visualizando os Scores Fatoriais
scores_fatoriais |>
  t() |>
  data.frame() |>
  rename(PC1 = 1,
         PC2 = 2) |>
  dplyr::select(PC1, PC2) |>
  kable() |>
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 16)

# Devemos efetuar a multiplicação por -1, 
# visto que os scores fatoriais das observações mais fortes são, por padrão, 
# apresentados acompanhados do sinal de menos.

F1 <- t(apply(dados_std, 1, function(x) x * scores_fatoriais[1,]))
F2 <- t(apply(dados_std, 1, function(x) x * scores_fatoriais[2,]))

F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * -1)

F2 <- data.frame(F2) %>%
  mutate(fator2 = rowSums(.) * -1)

ajuizamento_fat_exsp <- ajuizamento_exsp|>
  mutate(f1 = F1$fator1, f2 = F2$fator2) |>
  dplyr::select(propositura, comarca, num_proc, f1, f2)

rm(afpc, cargas_fatoriais, dados_std, F1, F2, relatorio, scores_fatoriais, k)

save(ajuizamento_fat, file="Documentos/ajuizamento_fat.RData")
save(ajuizamento_fat_exsp, file="Documentos/ajuizamento_fat_exsp.RData")

################################################################################
#                         Estimação por Regressão                              #
################################################################################

#1 - Regressão Linear - Número de Processos X População ------------------------

rs_populacao <- lm(num_proc ~ populacao, ajuizamento)

# Parâmetros do modelo 
  # Soma dos Erros ao Quadrado (R2) = 0.9779
  # Estatística F de Snedecor p-valor < 2.2e-16
  # Rejeitada a Hipótese nula, Beta estatisticamente significante
  # para esplicar o comportamento de Y
summary(rs_populacao)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_populacao)

# Teste de Shapiro-Francia (Aderência dos resíduos à normalidade)
  # Será que a variável se comporta de forma linear?
  # Vamos verificar se os resíduos se adequam à distribuição normal
  # p-valor < 0.05 (não aderente à normalidade)
sf.test(rs_populacao$residuals)

#Plotando os residuos
ajuizamento |>
  mutate(residuos = rs_populacao$residuals) |>
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequência") + 
  theme_bw()

#2 - Modelo com a variável dependente transformada -----------------------------
  # Perdeu enormemente a capacidade preditiva
  # Tampouco adere à normalidade
rs_populacao_bc <- lm(bc ~ populacao, ajuizamento_bc)
summary(rs_populacao_bc)
sf.test(rs_populacao_bc$residuals)

# Isso acontece porque a variável explicativa tampouco adere à normalidade
shapiro.test(ajuizamento$populacao)
fitDist(y = ajuizamento$populacao, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

plot(density(ajuizamento$num_proc), col = 'red',
     main = "Kernel Density das Causas Sentenciadas")

plot(density(ajuizamento$populacao), main = "Kernel Density da População")

auxiliar <- ajuizamento[ , c("num_proc", "populacao")] |>
  scale() |>
  round(2) |>
  as.data.frame()

plot(density(auxiliar$num_proc), 
     col = "red",
     main = "Kernel Density Causas Sentenciadas e População") +
  lines(density(auxiliar$populacao), col = "black") +
  legend("bottomright", c("Número de Processos", "População"),
         col = c("red","black"),
         lty = c(1,1), lwd = c(1,1))

openxlsx::write.xlsx(auxiliar,
           file = "E:/Andre/DataScience/Projeto MP/Documentos/KDE.xlsx",
           overwrite = TRUE)
rm(auxiliar)

# O que acontece se transformarmos a variável explicativa?
lambda <- powerTransform(ajuizamento_bc$populacao)$lambda

temp <- ajuizamento_bc |>
  mutate(populacao =  boxcoxTransform(ajuizamento_bc$populacao, lambda = lambda))

# O R2 melhora, mas ainda não alcança a regressão linear
rs_populacao_bc <- lm(bc ~ populacao, temp)
summary(rs_populacao_bc)
sf.test(rs_populacao_bc$residuals)
rm(lambda, rs_populacao_bc)


#3 - Regressão Linear - Número de Processos X População ------------------------
  # Sem São Paulo

rs_populacao_exsp <- lm(num_proc ~ populacao, ajuizamento_exsp)

# Parâmetros do modelo - R2 = 0.7733
# Estatística F de Snedecor p-valor < 2.2e-16
summary(rs_populacao_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_populacao_exsp)

# Teste de Shapiro-Francia (Aderência dos resíduos à normalidade)
sf.test(rs_populacao_exsp$residuals)

#Plotando os residuos
ajuizamento_exsp |>
  mutate(residuos = rs_populacao_exsp$residuals) |>
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequência") + 
  theme_bw()

#4 - Modelo com a variável dependente transformada -----------------------------
  # Perdeu enormemente a capacidade preditiva
  # Tampouco adere à normalidade
rs_populacao_bc_exsp <- lm(bc ~ populacao, ajuizamento_bc_exsp)
summary(rs_populacao_bc_exsp)
sf.test(rs_populacao_bc_exsp$residuals)

# Isso acontece porque a variável explicativa tampouco adere à normalidade
shapiro.test(ajuizamento_exsp$populacao)
fitDist(y = ajuizamento_exsp$populacao, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

plot(density(ajuizamento_exsp$num_proc), main = "Kernel Density das Causas Sentenciadas")
plot(density(ajuizamento_exsp$populacao), main = "Kernel Density da População")

# O que acontece se transformarmos a variável explicativa?
lambda <- powerTransform(ajuizamento_bc_exsp$populacao)$lambda

temp <- ajuizamento_bc_exsp |>
  mutate(populacao =  boxcoxTransform(ajuizamento_bc_exsp$populacao, lambda = lambda))

# O R2 melhora, mas ainda não alcança a regressão linear
  # Resíduos não adetem à normalidade
rs_populacao_bc_exsp <- lm(bc ~ populacao, temp)
summary(rs_populacao_bc_exsp)
sf.test(rs_populacao_bc_exsp$residuals)
rm(lambda, rs_populacao_bc_exsp)


#5 - Regressão Linear - Processos X População, IDH, PIB_pc, Ocupados -----------

rlm <- lm(num_proc ~ populacao + salario + idh + pib_pc + ocupados, ajuizamento)

#procedimento "stepwise"
rlm <- step(rlm, k = 3.841459)

# R2 = 0.9794
# Estatística F de Snedecor p-valor < 2.2e-16
summary(rlm)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rlm)

# Teste de Shapiro-Francia (Aderência dos resíduos à normalidade)
sf.test(rlm$residuals)

# Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
  # H0 : ausência de heterocedasticidade.
  # H1: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
  # variáveis explicativas, o que indica omissão de variável relevante!
  #"Prob > Chi2"  é o p-value
ols_test_breusch_pagan(rlm)

auxiliar <- data.frame(fitted = rlm$fitted.values, residual = rlm$residuals)

# Relação resíduos e fitted values do modelo:
auxiliar |>
  ggplot() +
  geom_point(aes(x = fitted, y = residual),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo",
       y = "Resíduos do Modelo") +
  theme_bw()

#Teste de Multicolinearidade (Variance Inflation Factor e Tolerance)
  # A Tolerância varia de 0 a 1 e queremos que seja próxima de 1
  # VIF (Variance Inflation Factor) = 1/tolerância
  # VIF varia de 1 a + infinito
ols_vif_tol(rlm)


#6 - Regressão Linear - Processos X População, IDH, PIB_pc, Ocupados ----------- 
  # Sem São Paulo

# Modelagem com todas as variáveis
rlm_exsp <- lm(num_proc ~ populacao + salario + idh + pib_pc + ocupados,
                       ajuizamento_exsp)

# Procedimento "stepwise" - Todas variáveis mantidas
rlm_exsp <- step(rlm_exsp, k = 3.841459)

# R2 = 0.8185
# Estatística F de Snedecor p-valor < 2.2e-16
summary(rlm_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rlm_exsp)

# Teste de Shapiro-Francia (Aderência dos resíduos à normalidade)
sf.test(rlm_exsp$residuals)

# Diagnóstico de Heterocedasticidade
ols_test_breusch_pagan(rlm_exsp)

# Diagnóstico de Multicoliniariadade
ols_vif_tol(rlm_exsp)

#7 - Regressão Linear - Score Fatorial -----------------------------------------

rlm_fat <- lm(num_proc ~ f1 + f2,
              ajuizamento_fat)

#procedimento "stepwise"
rlm_fat <- step(rlm_fat, k = 3.841459)

# R2 = 0.9793
# Estatística F de Snedecor p-valor < 2.2e-16
summary(rlm_fat)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rlm_fat)

# Teste de Shapiro-Francia (Aderência dos resíduos à normalidade)
sf.test(rlm_fat$residuals)


#8 - Regressão Linear - Score Fatorial - Sem São Paulo -------------------------

# Modelagem com todas as variáveis
rlm_fat_exsp <- lm(num_proc ~ f1 + f2,
                       ajuizamento_fat_exsp)

# procedimento "stepwise" - Score Fatorial 2 Excluído
rlm_fat_exsp <- step(rlm_fat_exsp, k = 3.841459)

# R2 = 0.7971
# Estatística F de Snedecor p-valor < 2.2e-16
summary(rlm_fat_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rlm_fat_exsp)

# Teste de Shapiro-Francia (Aderência dos resíduos à normalidade)
sf.test(rlm_fat_exsp$residuals)


#9 - Modelo de Poisson - Número de Processos X População -----------------------

rs_poisson <- glm(formula = num_proc ~  populacao,
                      data = ajuizamento,
                      family = "poisson")

# Parâmetros do modelo
summary(rs_poisson)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_poisson)


#10 - Modelo de Poisson - Número de Processos X População -----------------------
  # Sem São Paulo

rs_poisson_exsp <- glm(formula = num_proc ~  populacao,
                  data = ajuizamento_exsp,
                  family = "poisson")

# Parâmetros do modelo
summary(rs_poisson_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_poisson_exsp)


#11 - Modelo de Poisson - Processos X População, IDH, PIB_pc, Ocupados ----------

rm_poisson <- glm(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados,
                  data = ajuizamento,
                  family = "poisson")

# procedimento "stepwise" # todos mantidos
rm_poisson <- step(rm_poisson, k = 3.841459)

# Parâmetros do modelo
summary(rm_poisson)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_poisson)


#12 - Modelo de Poisson - Processos X População, IDH, PIB_pc, Ocupados ---------
# Sem São Paulo

rm_poisson_exsp <- glm(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados,
                       data = ajuizamento_exsp,
                       family = "poisson")

# procedimento "stepwise" - todos mantidos
rm_poisson_exsp <- step(rm_poisson_exsp, k = 3.841459)

# Parâmetros do modelo
summary(rm_poisson_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_poisson_exsp)


#13 - Modelo de Poisson - Score Fatorial ---------------------------------------

rm_poisson_fat <- glm(formula = num_proc ~  f1 + f2,
                  data = ajuizamento_fat,
                  family = "poisson")

# procedimento "stepwise" # todos mantidos
rm_poisson_fat <- step(rm_poisson_fat, k = 3.841459)

# Parâmetros do modelo
summary(rm_poisson_fat)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_poisson_fat)


#14 - Modelo de Poisson - Score Fatorial ---------------------------------------
# Sem São Paulo

rm_poisson_fat_exsp <- glm(formula = num_proc ~ f1 + f2,
                       data = ajuizamento_fat_exsp,
                       family = "poisson")

#procedimento "stepwise" - todos mantidos
rm_poisson_fat_exsp <- step(rm_poisson_fat_exsp, k = 3.841459)

# Parâmetros do modelo
summary(rm_poisson_fat_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_poisson_fat_exsp)


#15 - Modelo Binomial Negativo - Número de Processos X População ---------------

rs_bneg <- glm.nb(formula = num_proc ~  populacao,
                  data = ajuizamento)

glm.control()
# Não convergiu - aumentando o nº de iterações
rs_bneg <- glm.nb(formula = num_proc ~  populacao,
                  data = ajuizamento, control = list(maxit = 100, epsilon = 1e-08, trace = F))

# Parâmetros do modelo
summary(rs_bneg)

# Estatística z de Wald - para verificação da significância estatística
  # do parâmetro theta
rs_bneg$theta / rs_bneg$SE.theta  # maior que 1.96
# Estamos dividindo "theta" por seu erro padrão
# Essa distribuição regride à distribuição normal padrão.
# Portanto, se maior do que o valor crítico de 1.96, ele é estatisticamente significante.

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_bneg)


#16 - Modelo Binomial Negativo - Número de Processos X População ---------------
# Sem São Paulo

rs_bneg_exsp <- glm.nb(formula = num_proc ~  populacao,
                       data = ajuizamento_exsp)

# Parâmetros do modelo
summary(rs_bneg_exsp)

# Estatística z de Wald
rs_bneg_exsp$theta / rs_bneg_exsp$SE.theta

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_bneg_exsp)


#17 - Modelo Binomial Negativo - Processos X População, IDH, PIB_pc e ocupados -

rm_bneg <- glm.nb(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados,
                  data = ajuizamento)

# Não convergiu - aumentando o nº de iterações
rm_bneg <- glm.nb(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados,
                  data = ajuizamento, control = list(maxit = 100, epsilon = 1e-08, trace = F))

#procedimento "stepwise" - Excluiu PIB_pc
rm_bneg <- step(rm_bneg, k = 3.841459)

# Parâmetros do modelo
summary(rm_bneg)

# Estatística z de Wald 
rm_bneg$theta / rm_bneg$SE.theta

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_bneg)


#18 - Modelo Binomial Negativo - Processos X População, IDH, PIB_pc e ocupados
# Sem São Paulo

rm_bneg_exsp <- glm.nb(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados,
                       data = ajuizamento_exsp)

#procedimento "stepwise" - Saiu PIB_pc e salario
rm_bneg_exsp <- step(rm_bneg_exsp, k = 3.841459)

# Parâmetros do modelo
summary(rm_bneg_exsp)

# Estatística z de Wald 
rm_bneg_exsp$theta / rm_bneg_exsp$SE.theta

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_bneg_exsp)


#19 - Modelo Binomial Negativo - Score Fatorial --------------------------------

rm_bneg_fat <- glm.nb(formula = num_proc ~  f1 + f2,
                      data = ajuizamento_fat)

#procedimento "stepwise" # todos mantidos
rm_bneg_fat <- step(rm_bneg_fat, k = 3.841459)

# Parâmetros do modelo
summary(rm_bneg_fat)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_bneg_fat)


#20 - Modelo Binomial Negativo - Score Fatorial --------------------------------
# Sem São Paulo

rm_bneg_fat_exsp <- glm.nb(formula = num_proc ~ f1 + f2,
                           data = ajuizamento_fat_exsp)

#procedimento "stepwise" - todos mantidos
rm_bneg_fat_exsp <- step(rm_bneg_fat_exsp, k = 3.841459)

# Parâmetros do modelo
summary(rm_bneg_fat_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_bneg_fat_exsp)

#21 - Modelo Zero-Inflated Poisson (ZIP) --------------------------------------

zero_poisson <- zeroinfl(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados
                              | populacao, #pipe
                              data = ajuizamento,
                              dist = "poisson")

# Tudo o que vier antes do "pipe" (|) é componente de contagem
# O que vem depois do pipe é a variável que será investigada como potencial
# explicativa do comportamento de inflação de zeros
# Estamos investigando se um indicador populacional baixo explica a inflação de "0"

summary(zero_poisson)

# Teste de Vuong
vuong(m1 = rm_poisson, m2 = zero_poisson)


# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(zero_poisson)


#22 - Modelo Zero-Inflated Binomial Negativo (ZINB) ----------------------------
zero_bneg <- zeroinfl(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados
                           | populacao, #pipe
                           data = ajuizamento,
                           dist = "negbin")
summary(zero_bneg)

# Teste de Vuong
vuong(m1 = rm_bneg, m2 = zero_bneg)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(zero_bneg)

#23 - Modelo Zero-Inflated Poisson (ZIP) ---------------------------------------
  # Sem São Paulo

zero_poisson_exsp <- zeroinfl(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados
                     | populacao, #pipe
                     data = ajuizamento_exsp,
                     dist = "poisson")

# Tudo o que vier antes do "pipe" (|) é componente de contagem
  # O que vem depois do pipe é a variável que será investigada como potencial
  # explicativa do comportamento de inflação de zeros
  # Estamos investigando se um indicador populacional baixo explica a inflação de "0"

summary(zero_poisson_exsp)

# Teste de Vuong
vuong(m1 = rm_poisson_exsp, m2 = zero_poisson_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(zero_poisson_exsp)

#24 - Modelo Zero-Inflated Binomial Negativo (ZINB) ----------------------------
  # Sem São Paulo
zero_bneg_exsp <- zeroinfl(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados
                      | populacao, #pipe
                      data = ajuizamento_exsp,
                      dist = "negbin")

summary(zero_bneg_exsp)

# Teste de Vuong
vuong(m1 = rm_bneg_exsp, m2 = zero_bneg_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(zero_bneg_exsp)


#25 - Modelo Pareto Generalizado - Número de Processos X População -------------

rs_pareto <- vglm(num_proc ~ populacao,
                    family = VGAM::gpd(threshold = -0.1),
                    data = ajuizamento)


summary(rs_pareto)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_pareto)


#26 - Modelo Pareto Generalizado - Número de Processos X População -------------
  # Sem São Paulo

rs_pareto_exsp <- vglm(num_proc ~ populacao,
                    family = VGAM::gpd(threshold = -0.1),
                    data = ajuizamento_exsp)

summary(rs_pareto_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_pareto_exsp)


#27 - Modelo Pareto Generalizado - Processos X População, IDH, PIB_pc e ocupados

rm_pareto <- vglm(num_proc ~  populacao + salario + idh + pib_pc + ocupados,
                  family = VGAM::gpd(threshold = -0.1),
                  data = ajuizamento)
summary(rm_pareto)

#procedimento "stepwise"

rm_pareto <- vglm(num_proc ~  populacao + salario + idh + pib_pc,
                  family = VGAM::gpd(threshold = -0.1),
                  data = ajuizamento)

summary(rm_pareto)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_pareto)


#28 - Modelo Pareto Generalizado - Processos X População, IDH, PIB_pc e ocupados
  # Sem São Paulo

rm_pareto_exsp <- vglm(num_proc ~  populacao + salario + idh + pib_pc + ocupados,
                       family = VGAM::gpd(threshold = -0.1),
                       data = ajuizamento_exsp)
summary(rm_pareto_exsp)

# Procedimento Stepwise
rm_pareto_exsp <- vglm(num_proc ~  populacao + salario + idh + pib_pc,
                       family = VGAM::gpd(threshold = -0.1),
                       data = ajuizamento_exsp)
summary(rm_pareto_exsp)

rm_pareto_exsp <- vglm(num_proc ~  populacao + idh + pib_pc,
                       family = VGAM::gpd(threshold = -0.1),
                       data = ajuizamento_exsp)
summary(rm_pareto_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_pareto_exsp)


#29 - Modelo Binomial Negativo - Número de Processos X População Transformada--

lambda <- powerTransform(ajuizamento$populacao)$lambda

pop_lambda <- ajuizamento |>
  mutate(populacao =  boxcoxTransform(ajuizamento$populacao, lambda = lambda))

rs_bneg_box <- glm.nb(num_proc ~ populacao,
               data = pop_lambda)

summary(rs_bneg_box)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_bneg_box)


#30 - Modelo Binomial Negativo - Número de Processos X População Transformada--
  # Sem São Paulo

lambda_exsp <- powerTransform(ajuizamento_exsp$populacao)$lambda

pop_lambda_exsp <- ajuizamento_exsp |>
  mutate(populacao =  boxcoxTransform(ajuizamento_exsp$populacao, lambda = lambda_exsp))

rs_bneg_box_exsp <- glm.nb(num_proc ~ populacao,
                   data = pop_lambda_exsp)

summary(rs_bneg_box_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_bneg_box_exsp)


#31 - Modelo Binomial Negativo - População Transformada------------------------
rm_bneg_box <- glm.nb(num_proc ~ populacao + salario + idh + pib_pc + ocupados,
                      data = pop_lambda)

#procedimento "stepwise" - Excluiu o salário médio
rm_bneg_box <- step(rm_bneg_box, k = 3.841459)

summary(rm_bneg_box)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_bneg_box)

#32 - Modelo Binomial Negativo - População Transformada------------------------
  # Sem São Paulo

rm_bneg_box_exsp <- glm.nb(num_proc ~ populacao + salario + idh + pib_pc + ocupados,
                           data = pop_lambda_exsp)

# procedimento "stepwise"
rm_bneg_box_exsp <- step(rm_bneg_box_exsp, k = 3.841459)

summary(rm_bneg_box_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_bneg_box_exsp)


#33 - Modelo ZINB - Número de Processos X População Transformada----------------
rs_zero_bneg_box <- zeroinfl(formula = num_proc ~  populacao
                             | populacao, #pipe
                             data = pop_lambda,
                             dist = "negbin")

# Teste de Vuong - P-Valor Superior a 0.05
vuong(m1 = rs_bneg_box, m2 = rs_zero_bneg_box)

summary(rs_zero_bneg_box) # Não excluiu salário

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_zero_bneg_box)


#34 - Modelo ZINB - Número de Processos X População Transformada----------------
  # Sem São Paulo
rs_zero_bneg_box_exsp <- zeroinfl(formula = num_proc ~  populacao
                                  | populacao, #pipe
                                  data = pop_lambda_exsp,
                                  dist = "negbin")

# Teste de Vuong - Hipótese H0 para as medidas corrigidas pelo AIC e BIC
  # O teste padrão apresenta vies:
  # https://stats.stackexchange.com/questions/182020/zero-inflated-poisson-regression-vuong-test-raw-aic-or-bic-corrected-results
vuong(m1 = rs_bneg_box_exsp, m2 = rs_zero_bneg_box_exsp)

summary(rs_zero_bneg_box_exsp) # Não excluiu salário

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_zero_bneg_box_exsp)


#35 - Modelo ZINB - População Transformada--------------------------------------
rm_zero_bneg_box <- zeroinfl(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados
                      | populacao, #pipe
                      data = pop_lambda,
                      dist = "negbin")

# Teste de Vuong
vuong(m1 = rm_bneg_box, m2 = rm_zero_bneg_box)

#procedimento "stepwise"
rm_zero_bneg_box <- step(rm_zero_bneg_box, k = 3.841459)

summary(rm_zero_bneg_box) # Não excluiu salário

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_zero_bneg_box)


#36 - Modelo ZINB - População Transformada--------------------------------------
  # Sem São Paulo
rm_zero_bneg_box_exsp <- zeroinfl(formula = num_proc ~  populacao + salario + idh + pib_pc + ocupados
                         | populacao, #pipe
                         data = pop_lambda_exsp,
                         dist = "negbin")

# Teste de Vuong
vuong(m1 = rm_bneg_box_exsp, m2 = rm_zero_bneg_box_exsp)

#procedimento "stepwise"
rm_zero_bneg_box_exsp <- step(rm_zero_bneg_box_exsp, k = 3.841459)

summary(rm_zero_bneg_box_exsp) # Não excluiu salário

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_zero_bneg_box_exsp)


#37 - Modelo Pareto Generalizado - Número de Processos X População Transformada-

rs_pareto_box <- vglm(num_proc ~  populacao, 
                      family = VGAM::gpd(threshold = -0.1),
                      data = pop_lambda)

summary(rs_pareto_box)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_pareto_box)


#38 - Modelo Pareto Generalizado - Número de Processos X População Transformada-
# Sem São Paulo

rs_pareto_box_exsp <- vglm(num_proc ~  populacao, 
                           #+ salario + idh + pib_pc + ocupados,
                           family = VGAM::gpd(threshold = -0.1),
                           data = pop_lambda_exsp)

summary(rs_pareto_box_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rs_pareto_box_exsp)


#39 - Modelo Pareto Generalizado - População Transformada-----------------------

rm_pareto_box <- vglm(num_proc ~  populacao + salario + idh + pib_pc + ocupados, 
                       family = VGAM::gpd(threshold = -0.1),
                       data = pop_lambda)

summary(rm_pareto_box)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_pareto_box)


#40 - Modelo Pareto Generalizado - População Transformada-----------------------
  # Sem São Paulo

rm_pareto_box_exsp <- vglm(num_proc ~  populacao + salario + idh + pib_pc + ocupados, 
                      #+ salario + idh + pib_pc + ocupados,
                      family = VGAM::gpd(threshold = -0.1),
                      data = pop_lambda_exsp)

summary(rm_pareto_box_exsp)

# Indicadores de Qualidade do Modelo
qual_modelo <- qualidade(rm_pareto_box_exsp)

# Aferindo o modelo com o melhor "fit"
(qual_modelo <- qual_modelo |>
  arrange(desc(ll), desc(AIC)))

qual_modelo <- arrange(qual_modelo, desc(ll), desc(AIC))

save(qual_modelo, file="Documentos/qualidade_modelo.RData")
save(rm_bneg_box_exsp, file="Documentos/qualidade_modelo.RData")



##################################### FIM ######################################
