################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse",
             "plotly","knitr","kableExtra","reshape2",
             "ggrepel", #Para colocar legendas em gráficos
             "fastDummies","lmtest","splines","jtools",
             "questionr", #Para construir uma tabela de frequência
             "MASS", #Para rodar modelos do tipo binomial negativo
             "pscl", #Para inflação de zeros em eventos raros "zero inflated"
             "overdisp", #Para idemtificação de superdispersão em dados de contagem.
             "magick","cowplot","beepr")

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
  #Existe uma função select() no pacote MASS
  dplyr::select(propositura, comarca) |>
  arrange(propositura, comarca)

# Vinculando os indicadores socioeconômicos aos processos ajuizados
ajuizamento <- ajuizamento |>
  group_by(propositura, comarca) |>
  # Conta o número de processos ajuizados em cada comarca a cada ano
  summarise(num_proc = n()) |>
  ungroup () %>% droplevels(.)   |>
  left_join(indicadores, by = c("propositura" = "ano", "comarca")) |>
  dplyr::select(num_proc, everything(), -c(propositura, comarca))

# Selecionando as variáveis número de processo, poulacao, idh e pib per capta
ajuizamento <- ajuizamento |>
  dplyr::select(num_proc, populacao, idh, pib_pc)

# Estatísticas descritivas univariadas e tabela de frequências
summary(ajuizamento)

#Histograma da variável dependente
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

################################################################################
#                         TESTE DE SUPERDISPERSÃO                              #
################################################################################
# Em uma distribuição de poisson perfeita, a média é matematicamente igual à variância.

#1 - Diagnóstico preliminar para observação de eventual igualdade entre a média e
  # a variância da variável dependente  "num_proc"
ajuizamento |> 
  summarise(Média = mean(num_proc),
            Variância = var(num_proc)) |>
  kable() |>
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 30)

# Note que a variância é 1189 vezes maior que a média de ajuizamentos por ano
  #É um indício de SUPERDISPERSÃO dos dados.

#2 - Teste de Superdispersão de Cameron
  # CAMERON, A. C.; TRIVEDI, P. K. Regression-based tests for overdispersion in
  # the Poisson model. Journal of Econometrics, v. 46, n. 3, p. 347-364, 1990.

  # Pode ser executado por meio da utilização da função overdisp::overdisp()
overdisp::overdisp(x = ajuizamento,
                   dependent.position = 1, # Variavel dependente na 1ª Coluna
                   predictor.position = 2:4) # Variáveis perditoras (colunas 2 a 4)

# p-value do parâmetro do lambda_poisson < 2.2e-16 
  # Como é abaixo do nível de significância de 5%, 
  # diagnostica-se a existência de superdispersão nos dados,
  # o que aconselha a estimação de um modelo binomial negativo.


################################################################################
#                         Estimação do Modelo Poisson                          #
################################################################################

modelo_poisson <- glm(formula = num_proc ~ .,
                      data = ajuizamento,
                      family = "poisson")

# Parâmetros do modelo_poisson
summary(modelo_poisson)

#Extração do valor de Log-Likelihood (LL)
logLik(modelo_poisson)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_poisson, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_poisson, scale = F, digits = 4)

#LR Test - função lrtest do pacote lmtest
#(likelihood ratio test para comparação de LL's entre modelos)
lrtest(modelo_poisson) #no caso, comparação com modelo nulo (somente com intercepto)

# Todas as variáveis preditoras se mostraram estatisticamente diferentes de zero,
# considerando-se um nível de significância de 0,1%, ceteris paribus.


################################################################################
#                   Estimação do Modelo Binomial Negativo                      #
################################################################################

# Estimação do modelo binomial negativo pela função glm.nb do pacote MASS
# Modelo Binomial Negativo do Tipo 2 (NB2)

modelo_bneg <- glm.nb(formula = num_proc ~ .,
                      data = ajuizamento)

#Parâmetros do modelo_bneg
summary(modelo_bneg)
#Note que o output dá o LL MULTIPLIDADO POR DOIS

# Parâmetro de forma da distribuição binomial negativa
1 / modelo_bneg$theta #phi
modelo_bneg$theta

# Estatística z de Wald do parâmetro theta para verificação da
# significância estatística
modelo_bneg$theta / modelo_bneg$SE.theta  # maior que 1.96
# Estamos dividindo "theta" por seu erro padrão
# Essa distribuição regride à distribuição normal padrão.
# Portanto, se maior do que o valor crítico de 1.96, ele é estatisticamente significante.

# Extração do valor de Log-Likelihood (LL)
logLik(modelo_bneg)

#Parâmetros do modelo_bneg
summ(modelo_bneg, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_bneg, scale = F, digits = 4)

#Comparando os modelos Poisson e Binomial Negativo
export_summs(modelo_poisson, modelo_bneg, scale = F, digits = 4,
             model.names = c("POISSON","BNEG"))

data.frame(LL_Poisson = round(logLik(modelo_poisson), 1),
           LL_Bneg = round(logLik(modelo_bneg), 1)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

#Likelihoo-ratio test
lrtest(modelo_poisson, modelo_bneg)

#Gráfico para a comparação dos LL dos modelos Poisson e Binomial Negativo
my_plot <-
  data.frame(Poisson = logLik(modelo_poisson),
             BNeg = logLik(modelo_bneg)) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = round(value, digits = 3)), 
            color = "black", 
            size = 4, 
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "orange")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot

#Com JPEG
ggdraw() +
  draw_image("https://cdn.pixabay.com/photo/2016/08/21/18/48/emoticon-1610518_960_720.png",
             x = -0.12, y = 0.23, scale = .43) +
  draw_plot(my_plot)


#COMPARAÇÕES ENTRE AS PREVISÕES:
#Qual seria a quantidade média esperada de violações de trânsito para um país
#cujo corpo diplomático seja composto por 23 membros, considerando o período
#anterior à vigência da lei e cujo índice de corrupção seja igual 0.5?

#Modelo Poisson:
predict(object = modelo_poisson, #linha 144 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

#type = "response" já retorna o valor de lambda após a exponenciação de "y"

#Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

#type = "response" já dá o valor da variável dependente.
# vai calcular o vertor e clacular sua exponencial


#Qual seria a quantidade média esperada de violações de trânsito para o mesmo
#país, porém agora considerando a vigência da lei?

#Modelo Poisson:
predict(object = modelo_poisson,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

#Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")


#Adicionando os fitted values dos modelos estimados até o momento, para fins de 
#comparação:
corruption %>%
  mutate(fitted_poisson = modelo_poisson$fitted.values,
         fitted_bneg = modelo_bneg$fitted.values) %>% 
  dplyr::select(country, code, violations, fitted_poisson, 
                fitted_bneg) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 19)


#Fitted values dos modelos POISSON e BINOMIAL NEGATIVO, considerando,
#para fins didáticos, apenas a variável preditora 'staff':
corruption %>%
  ggplot() +
  geom_point(aes(x = staff, y = violations), alpha = 0.5, size = 2) +
  geom_smooth(aes(x = staff, y = modelo_poisson$fitted.values,
                  color = "POISSON"), se = F, size = 1.5) +
  geom_smooth(aes(x = staff, y = modelo_bneg$fitted.values,
                  color = "BNEG"), se = F, size = 1.5) + 
  scale_color_manual("Estimação:",
                     values = c("orange", "#440154FF")) +
  labs(x = "Number of Diplomats (staff)",
       y = "Unpaid Parking Violations (violations)") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


#Note que a variável dependente está na ordenada
#Note que que, para valores pequenos da variável explicativa, existe pouca diferença
#entre as duas dispersões.
#Contudo, a Poisson não captura a cauda longa.