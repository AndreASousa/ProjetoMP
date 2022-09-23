################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse",
             "lubridate",
             "forecast",
             "prophet",#Utilizado para separar dados por mês e ano
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
#                 CRIAÇÃO E VISUALISAÇÃO DA SÉRIE TEMPORAL                     #
################################################################################

load(here::here("Documentos", "Distribuicao_df.RData"))

#1 - Criando A série temporal
serie_mensal <- distribuicao_df |>
  dplyr::select(processo, data) |>
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(y = n())|>
  ungroup () %>% droplevels(.) |>
  dplyr::select(y) |>
  ts(start = c(2017,8), end = c(2022,7), frequency = 12)


#2 - Vizualizando a série

# vizualizando os dados
forecast::autoplot(serie_mensal, main = "Nº Processos a Cada Mês", xlab = "Mês", ylab = "Nº de Processos")
# É possível notar que a sazonalidade permanece constante ao longo do tempo
# Isso sugere o uso de um "método aditivo" 

# Autocorrelação e autocorrelação parcial
ggtsdisplay(serie_mensal)

#3 - Verificando se a série segue a distribuiçao normal
hist(serie_mensal)

qqnorm(serie_mensal)
qqline(serie_mensal, col = "red")
# É possível notar que a distribuição se acentua no centro
# Podemos trabalhar com uma tendência a partir do ponto zero.
# A partir dos ponto 1 e -1, existe uma disfunção entre a distribuição normal e os dados.


#4 - Transformação por Logaritmo natual

# Embora a série pareça indicar a possibilidade de uso do método aditivo
# Aplicaremos o logaritmo natural para futura análise do modelo preditivo mais eficaz
serie_log <- serie_mensal |>
  log()

plot(serie_mensal)
plot(serie_log)

#5 - Análise de autocorrelação e Autocorrelação Parcial

# Autocorrelação
acf(serie_mensal, lag.max = 20) #Colocamos uma defasagem máxima de 20 observações.
# Autocorrelação Parcial
pacf(serie_mensal, lag.max = 20)

acf(serie_log, lag.max = 20)
pacf(serie_log, lag.max = 20)


################################################################################
#                    DECOMPOSIÇÃO DAS SÉRIES TEMPORAIS                         #
################################################################################

#1 - Separando a série conforme os componentes sazonal, de tendência e aleatório
serie_mensal |>
  decompose() |>
  plot(type = "o", pch = 20)

serie_log |>
  decompose() |>
  plot(type = "o", pch = 20)


#2 - Estimando o componente sazonal
serie_dessazonal <- decompose(serie_mensal) 
serie_dessazonal <- serie_mensal - serie_dessazonal$seasonal

log_dessazonal <- decompose(serie_log) 
log_dessazonal <- serie_log - log_dessazonal$seasonal

plot.ts(serie_mensal)
plot(serie_dessazonal)

plot.ts(serie_log)
plot(log_dessazonal)

#3 - A série logaritmica necessita de diferenciação sazonal
nsdiffs(serie_mensal)  # número necessário de diferenciações sazonais 
nsdiffs(serie_log)

serie_log_difs <- serie_log |>
  diff(lag = frequency(serie_log), differences=1)

plot(serie_log)
plot(serie_log_difs)

serie_log_difs |>
  decompose() |>
  plot(type = "o", pch = 20)


################################################################################
#                     NORMALIZAÇÃO E ESTACIONARIEDADE                          #
################################################################################

#1 - Identificando a necessidade de diferenciação

# A série mensal exige uma diferenciação para a remoção da tendência
ndiffs(serie_mensal)
# A série em escala logaritimica não exige diferenciação
ndiffs(serie_log_difs)

plot(serie_mensal)

# A diferenciação parece ter sido suficiente para assegurar a estacionariedade
serie_mensal|>
  diff(1)|>
  plot()

#2 - Testes de estacionariedade
  #Se p valor < 0.05 indica que a série é estacionária

# Teste Dickey-Fuller Aumentado
tseries::adf.test(serie_mensal)
tseries::adf.test(diff(serie_mensal, 1))
tseries::adf.test(serie_log)
tseries::adf.test(serie_log_difs) 

# Teste Kwiatkowski–Phillips–Schmidt–Shin (KPSS)
tseries::kpss.test(serie_mensal)
tseries::kpss.test(diff(serie_mensal, 1))
tseries::kpss.test(serie_log)
tseries::kpss.test(serie_log_difs)


################################################################################
#                    MODELO ARIMA - Série Original                             #
################################################################################
# Uma vez confirmado que a série original pode ser convertida em estacionária
# com uma diferenciação, é possível aplicar o método ARIMA: 
# Modelo de Média Móvel Integrada Autoregressiva (p, d, q)

#1 -  Dividindo a base em amostras
amostra_treino <- stats::window(serie_mensal, end = c(2021, 7))
amostra_teste <- stats::window(serie_mensal, start = c(2021, 8))

#2 - Aplicando a função auto.arima() para identificar os melhores parâmetros
auto.arima(amostra_treino, trace = TRUE, approximation = FALSE) 

#Modelo Autorregressivo (0,0,0), Modelo de Média Móvel dos Resíduos (0,1,1)

#3 -  Implementando o modelo arima sobre os dados
modelo <- Arima(amostra_treino, order = c(0, 0, 0), seasonal = c(0, 1, 1), 
                include.drift = TRUE)

#4 - Avaliar o modelo a partir dos resíduos
checkresiduals(modelo)

#5- Fazendo previsões com o modelo 
previsao <- forecast(modelo, h = 12)
autoplot(previsao)

#6 - Avaliando o modelo com o RMSE, MAE e MAPE.

# MAPE - erro absoluto do percentual da média
  # Mede a precisão como uma porcentagem.
  # Permite a análise relativa entre os modelos.

# RMSE - raiz do erro quadrático médio
  # Usado para avaliar a medida das diferenças entre os valores (amostra ou população) previstos
  # por mum modelo ou um estimador e os valores observados

# MAE - erro médio absoluto
  # Medida de erros entre observações emparelhadas que expressam o mesmo fenômeno

indicadores <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(indicadores) <- c("mape", "rmse", "mae")

qualidade <- function(prev = previsao, teste = amostra_teste, df = indicadores){
  erro = teste - prev$mean
  mape <- mean(abs(erro) / (prev$mean + erro)) * 100
  rmse <- sqrt(mean(erro ^ 2, na.rm = TRUE)) 
  mae <- mean(abs(erro), na.rm = TRUE)
  
  linha <- cbind(mape, rmse, mae)
  
  df <- df |>
    rbind(linha)
  
  return(df)
}

indicadores <- qualidade(previsao)

################################################################################
#                   MODELO ARIMA - 1 diferenciação                             #
################################################################################

#1 - Aplicando a função auto.arima() com uma diferenciação
auto.arima(amostra_treino, d = 1, trace = TRUE, approximation = FALSE)
#Modelo Autorregressivo (0,1,1), Modelo de Média Móvel dos Resíduos (0,1,1)

#2 -  Implementando o modelo arima sobre os dados
  # A funão Arima() realiza automaticamente o processo de remoção da
  # diferenciação quando esta consta expressamente entre seus parâmetros
modelo_dif <- Arima(amostra_treino, order = c(0, 1, 1), seasonal = c(0, 1, 1)) 

#3 - Avaliar o modelo a partir dos resíduos
checkresiduals(modelo_dif)

#4- Fazendo previsões com o modelo 
previsao_dif <- forecast(modelo_dif, h = 12)
autoplot(previsao_dif)

#5 - Avaliando o modelo com o RMSE, MAE e MAPE.
indicadores <- qualidade(previsao_dif)

################################################################################
#                  MODELO ARIMA - Transofrmação Logaritmica                    #
################################################################################

#1 - Aplicando a função auto.arima() com lambda = 0 (escala log)
  #Difrenciação sazonal = 1
auto.arima(amostra_treino, lambda = 0, D = 1, trace = TRUE, approximation = FALSE)  
#Modelo Autorregressivo (0,0,0), Modelo de Média Móvel dos Resíduos (0,1,1)

#2 -  Implementando o modelo arima sobre os dados
  # A funão Arima() realiza automaticamente a transformação inversa da escala
  # quando descrito expressamente entre seus parâmetros
modelo_log <- Arima(amostra_treino, order = c(0, 0, 0), seasonal = c(0, 1, 1), lambda = 0) 

#3 - Avaliar o modelo a partir dos resíduos
checkresiduals(modelo_log)

#4- Fazendo previsões com o modelo 
previsao_log <- forecast(modelo_log, h = 12)
autoplot(previsao_log)

#5 - Avaliando o modelo com o RMSE, MAE e MAPE.
indicadores <- qualidade(previsao_log)

################################################################################
#                          MODELO HOLT-WINTERS                                 #
################################################################################

# Suavização Exponencial Holt-Winters
  # Utilizado para dados com padrões e tendências sazonais.

suav_hw <- HoltWinters(amostra_treino)

previsao_hw <- forecast(suav_hw, h = 12)

autoplot(previsao_hw)

indicadores <- qualidade(previsao_hw)

accuracy(previsao_hw, amostra_teste)


################################################################################
#                            MODELO PROPHET                                    #
################################################################################

prophet_mensal <- distribuicao_df |>
  dplyr::select(processo, data) |>
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(y = n())|>
  ungroup () %>% droplevels(.) |>
  filter (mes < "2022-08-01") |> #Vamos excluir o mês em aberto
  rename(ds = mes) #A colunas devem ser nomeadas como "ds" e "y"

amostra_treino <- subset(prophet_mensal, ds < "2021-08-01")
amostra_teste <- subset(prophet_mensal, ds >= "2021-08-01")

m <- prophet(amostra_treino)

projecao <- make_future_dataframe(m, periods = 12, freq = "month")
tail(projecao)

previsao <- predict(m, projecao)

# Avaliação por valores brutos com o valor previsto por dia e intervalos de incerteza
tail(previsao[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(previsao)

# 3 - Inspecionando Componentes do Modelo
plot(m, previsao)
prophet_plot_components(m, previsao)

#5 - Avaliando o modelo com o RMSE, MAE e MAPE.

erro = amostra_teste$y - previsao$yhat
mape <- mean(abs(erro) / (previsao$yhat + erro)) * 100
rmse <- sqrt(mean(erro ^ 2, na.rm = TRUE)) 
mae <- mean(abs(erro), na.rm = TRUE)
linha <- cbind(mape, rmse, mae)

indicadores <- indicadores |>
  rbind(linha)
  
################################################################################
#                          COMPARANDO  MODELOS                                 #
################################################################################  

indicadores

modelo <- c("Base Original", "1 Defasagem", "Log", "Holt-Winters", "Prophet")

indicadores$modelo <- modelo
indicadores <- indicadores |>
  dplyr::select(modelo, everything())

colnames(indicadores) <- toupper(colnames(indicadores))

write.xlsx(indicadores, file = "E:/Andre/DataScience/Projeto MP/Documentos/indicadores_st.xlsx", overwrite = TRUE)

# Optamos pelo modelo ARIMA sobre a Base de Dados Original
modelo <- Arima(serie_mensal, order = c(0, 0, 0), seasonal = c(0, 1, 1), 
                include.drift = TRUE)

#4 - Avaliar o modelo a partir dos resíduos
checkresiduals(modelo)

#5- Fazendo previsões com o modelo 
previsao <- forecast(modelo, h = 12)
autoplot(previsao)

args(autoplot)

