# Cria uma rede neural multi-camada recorrente

################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse", # pacote para manipulacao de dados
             'neuralnet',   # Pacote para fazer redes neurais
             "rattle",
             "rnn",
             "ggplot2",
             "car", # PowerTransform (transformar boxcox em modelos não lineares)
             "EnvStats", # boxcoxTransform()
             "fastDummies" # Transforma var categóricas em dummies
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

# Função para padronizar os dados
  # (x - valor mínimo de x) / (max_x - min_x)  

padroniza <- function(tabela = NULL){
  
  # Identifica as colunas que contém valores numéricos
  numericos <- sapply(tabela, is.numeric)
  
  # Separa as colunas com valores numéricos
  quant <- tabela[numericos]
  
  # Seleciona o maior valor em cada coluna ("2")
  max_data <- apply(quant, 2, max) 
  
  # Seleciona o menor valor em cada coluna ("2")
  min_data <- apply(quant, 2, min)
  
  # Padroniza os dados
  # (x - valor mínimo de x) / (max_x - min_x)
  quant <- quant |>
    scale(center = min_data, scale = max_data - min_data) |>
    as.data.frame()
  
  tabela <- tabela[!numericos]|>
    cbind(quant)
  
  # attributes(tabela)$max <- max_data
  # attributes(tabela)$min <- min_data

  return(tabela)
}

# Indicadores de qualidade do modelo
# indicadores <- data.frame(matrix(ncol = 3, nrow = 0))
# colnames(indicadores) <- c("mape", "rmse", "mae")
# 
# qualidade <- function(prev = previsao, teste = amostra_teste, df = indicadores){
#   erro = teste - prev$mean
#   mape <- mean(abs(erro) / (prev$mean + erro)) * 100
#   rmse <- sqrt(mean(erro ^ 2, na.rm = TRUE)) 
#   mae <- mean(abs(erro), na.rm = TRUE)
#   
#   linha <- cbind(mape, rmse, mae)
#   
#   df <- df |>
#     rbind(linha)
#   
#   return(df)
# }


rsq <- function(y_actual, y_predict)
{
  cor(y_actual,y_predict)^2
}

################################################################################
#                              REDE NEURAL                                     #
################################################################################

# Tabela com o número de processos sentenciados por ano de variaveis e Comarca.
  # (Criado em: 008 - Modelos_GLM.R)
load(here::here("Documentos", "ajuizamento.RData"))

# Tabela com as Comarcas divididas em Clusters
  # (Criado em: 009 - Analise_de_Agrupamento.R)
  # Já está definidco como variável categórica (factor)
load(here("Documentos", "cluster.RData"))

variaveis <- ajuizamento |>
  left_join(cluster, by = "comarca") |>
  dplyr::select(propositura, num_proc, cluster_k,populacao, salario, idh, pib_pc, ocupados)|>
  rename(cluster = cluster_k)

# Predição que exclui São Paulo
variaveis_exsp <- ajuizamento |>
  left_join(cluster, by = "comarca") |>
  filter(comarca != "São Paulo") |>
  dplyr::select(propositura, num_proc, cluster_k, populacao, salario, idh, pib_pc, ocupados)|>
  rename(cluster = cluster_k)

# Transformando a Variável População
lambda <- powerTransform(variaveis$populacao)$lambda

variaveis <- variaveis |>
  mutate(populacao =  boxcoxTransform(variaveis$populacao, lambda = lambda))

lambda_exsp <- powerTransform(variaveis_exsp$populacao)$lambda

variaveis_exsp <- variaveis_exsp |>
  mutate(populacao =  boxcoxTransform(variaveis_exsp$populacao, lambda = lambda_exsp))

rm(lambda, lambda_exsp)

# Procedimento n-1 dummies
variaveis <- dummy_columns(.data = variaveis,
                            select_columns = "cluster",
                            remove_selected_columns = T,
                            remove_most_frequent_dummy = T)

variaveis_exsp <- dummy_columns(.data = variaveis_exsp,
                           select_columns = "cluster",
                           remove_selected_columns = T,
                           remove_most_frequent_dummy = T)

# A existência de valores nulos prejudica a estimação do modelo
  # Estaria utilizando um valor desconhecido para prever o valor mediano
variaveis[is.na(variaveis) == TRUE]
variaveis_exsp[is.na(variaveis_exsp) == TRUE]


#1 - Padronizando os dados -----------------------------------------------------
  # Tende a melhorar a capacidade preditiva dos modelos de deep learning

variaveis$propositura <- as.factor(variaveis$propositura)
variaveis_exsp$propositura <- as.factor(variaveis_exsp$propositura)

variaveis_pad <- padroniza(variaveis)
variaveis_pad_exsp <- padroniza(variaveis_exsp)

# 2 - Criando as amostras ------------------------------------------------------

treino <- variaveis_pad |>
  filter(propositura %in% c(2017, 2018, 2019) ) |>
  dplyr::select(-propositura)

teste <- variaveis_pad |>
  filter(propositura == 2020 ) |>
  dplyr::select(-propositura)

treino_exsp <- variaveis_pad_exsp |>
  filter(propositura %in% c(2017, 2018, 2019) ) |>
  dplyr::select(-propositura)

teste_exsp <- variaveis_pad_exsp |>
  filter(propositura == 2020 ) |>
  dplyr::select(-propositura)


#3 - Testando um modelo ------------- ------------------------------------------

set.seed(2022)
nn <- neuralnet(num_proc ~ .,
                data = treino,
                hidden = c(5,4,3,2), # Neurônios em cada camada escondida
                  linear.output = T)

# plot(nn)

# Comparando com a amostra treino o resultado neuralnet::compute
previsao <- compute(nn, teste[,2:length(teste)]) # Exclui a coluna com a variável resposta

# Retornando a variável Y para os valores originais (sem escala)
valores_observados <- teste$num_proc * (max(variaveis$num_proc)-min(variaveis$num_proc))+min(variaveis$num_proc)
valores_previstos <- previsao$net.result * (max(variaveis$num_proc)-min(variaveis$num_proc))+min(variaveis$num_proc)

# Calculamos o erro quadrado médio.
mean((pr.nn_ - test.r)^2)

# Comparando os valoes observados na amostra teste com os valores preditos
plot(teste$num_proc,type = 'l',col="red",xlab = "x", ylab = "Número de Processos")
lines(previsao$net.result,col = "blue")

rm(nn, previsao, valores_observados, valores_previstos)


#4 - Alimentanndo a rede neural por amostras -----------------------------------

# A base de dados será separada em "samples", 
  # amostras que servirão para alimanter a rede neural aos poucos.

#É possível encaminhar toda a base de dados de uma vez para a rede neural,
  # mas isso impede que o algoritmo "aprenda" conforme novas informações
  #são processadas e ir gerando loops de recorrência.

# Misturando as observações da amostra treino
set.seed(2022)
treino <- treino[sample(1:nrow(treino), replace = F) , ]

# Variável dependente - Já colocada em escala
y_treino <- treino$num_proc

# Variáveis explicativas - Já colocada em escala
x1 <- treino$populacao
x2 <- treino$salario
x3 <- treino$idh
x4 <- treino$pib_pc
x5 <- treino$ocupados
x6 <- treino$cluster_2
x7 <- treino$cluster_3
x8 <- treino$cluster_4
x9 <- treino$cluster_5


# Vamos criar uma matriz de 32 linhas por coluna
x1 <- matrix(x1, nrow = 32)
x2 <- matrix(x2, nrow = 32)
x3 <- matrix(x3, nrow = 32)
x4 <- matrix(x4, nrow = 32)
x5 <- matrix(x5, nrow = 32)
x6 <- matrix(x6, nrow = 32)
x7 <- matrix(x7, nrow = 32)
x8 <- matrix(x8, nrow = 32)
x9 <- matrix(x9, nrow = 32)

y_treino <- matrix(y, nrow = 32)

# Unindo as variáveis explicativas

#Dim1 - o número de observações por amostra
#Dim2 - a quantidade de amostras que devem ser encaminhadas
#Dim3 - o número de variáveis explicativas que pretendemos utilizar
x_treino <- array(c(x1, x2, x3, x4, x5, x5, x6, x7, x8, x9), 
             dim = c(dim(x1), 9))

# Precisamos fazer o mesmo procedimento para a amostra TESTE
x1 <- teste$populacao
x2 <- teste$salario
x3 <- teste$idh
x4 <- teste$pib_pc
x5 <- teste$ocupados
x6 <- teste$cluster_2
x7 <- teste$cluster_3
x8 <- teste$cluster_4
x9 <- teste$cluster_5

x1 <- matrix(x1, nrow = 32)
x2 <- matrix(x2, nrow = 32)
x3 <- matrix(x3, nrow = 32)
x4 <- matrix(x4, nrow = 32)
x5 <- matrix(x5, nrow = 32)
x6 <- matrix(x6, nrow = 32)
x7 <- matrix(x7, nrow = 32)
x8 <- matrix(x8, nrow = 32)
x9 <- matrix(x9, nrow = 32)

x_teste <- array(c(x1, x2, x3, x4, x5, x6, x7, x8, x9), 
                 dim = c(dim(x1), 9))

rm(x1, x2, x3, x4, x5, x6, x7, x8, x9)

# Treinando o modelo
set.seed(2022)
model <- rnn::trainr(Y = y_treino, #Variavel "target"
                     X = x_treino, #variável "feature" (explicativa)
                     learningrate = 0.01, #Taxa de aprendizado
                     hidden_dim = 16,
                     # hidden_dim = c(5, 4, 3 , 2), # 1ª camada com 5 neurônios ...
                     network_type = "rnn", #Rede Neural Recorrente (Rede de Elman), GRU, LSTM ou outra
                     numepochs = 100)

# O algoritmo indica o "epoch error" - a função de perda para cada época.
# Caso o erro não caia de uma época para outra, a conclusão é que o modelo não está convergindo.

# Note que o erro está caindo conforme o transcurso das épocas.
plot(colMeans(model$error),type='l',xlab='epoch',ylab='errors')

# Vamos prever o valor de "y" a partir da amostra TESTE 
previsao <- predictr(model, x_teste)

# Transformando a matriz em uma só coluna
y_pred <- previsao |>
  matrix(nrow = 1) |>
  t() |>
  as.data.frame()

names(y_pred) <- "y_pred"

# Data Frame com valores pretditos e observados.
resultado <- teste[ , "num_proc", drop = F] |>
  cbind(y_pred)

rsq(resultado$num_proc,resultado$y_pred)

# grafico
plot(as.vector(t(resultado$num_proc)), col = 'red', type='l',
     main = "Numero de Processos Observados vs Previstos",
     ylab = "Y,Yp") +
lines(as.vector(t(previsao)), type = 'l', col = 'black') +
legend("bottomright", c("Predito", "Observado"),
       col = c("red","black"),
       lty = c(1,1), lwd = c(1,1))

#5 - Estimando diferentes parâmetros -------------------------------------------



##################################### FIM ######################################
