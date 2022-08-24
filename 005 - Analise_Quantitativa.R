################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
################################################################################

pacotes <- c("here",
             "tidyverse",
             "tidyr",
             "stringr",
             "purrr",
             "lubridate", #Utilizado para separar dados por mês e ano
             "openxlsx")

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
#                             TEMPORÁRIO!!!!!                                  #
################################################################################

load(here::here("Documentos", "Distribuicao_df.RData"))

#Vamos abrupar o dataset com base no mês e ano
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
       # title = "Distribuições Mês a Mês"
       ) +
  theme(axis.line = element_line(color="black", size = 1.5))

openxlsx::write.xlsx(distr_mes, file = "E:/Andre/DataScience/Projeto MP/Documentos/distr_mes.xlsx")



#Vamos Vizualizar o dataset com base no ano 
distr_ano <- distribuicao_df |> 
  group_by(ano = floor_date(data, unit = "year")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.) |>
  filter (ano < "2022-01-01" & ano > "2017-12-31")

(distr_ano$num_proc[4]/distr_ano$num_proc[1])*100
  
  
  ggplot(distr_ano) + 
  geom_line(aes(x = ano , y = num_proc)) +
  geom_smooth(aes(x = ano , y = num_proc), method = "lm") +
  labs(x = "Ano",
       y = "Número de Processos",
       title = "Distribuições a Cada Ano")
  

#Vamos Aferir quantos cargos ativos e quntos processos por cargo ativo

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


#Vamos Aferir cargos ativos a cada semana
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
  
  
  group_by(mes = floor_date(Data, unit = "month")) |>
  
#Morosidade da Justiça
  #Por fazer
morosidade <- distribuicao_df |> 
    group_by(ano = floor_date(data, unit = "year")) |>
    summarise(tempo_gasto = mean(distribuicao_df$propositura))|>
    ungroup () %>% droplevels(.) |>
    filter (ano < "2022-01-01" & ano > "2017-12-31")
  
##Identificar as categorias das variáveis "Natureza" e "Tipo"
sum(is.na(distribuicao_df$tipo))
sum(is.na(distribuicao_df$natureza))

table(distribuicao_df$natureza)
summary(distribuicao_df$natureza)
# É possível notar uma certa inconsistência no cadastramento das "Naturezas"
# A exemplo, ações de alimento aparecem tanto como "ALIMENT" quanto "ALIMENTO"

alimento <- c("ALIMENT", "REVISION", "ALIMENTO", "ALIMPRO", "ALPAREN", "EXE.ALM")

nrow(filter(distribuicao_df, natureza %in% alimento))
nrow(filter(distribuicao_df, natureza %in% alimento)) / nrow(distribuicao_df)


## Causas Que Versam sobre o Direito a alimentos
distr_alimentos <- distribuicao_df |> 
  filter(natureza %in% alimento) |> #Excluindo os casos distribuidos à Secretaria
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.) |>
  filter (mes < "2022-08-01") #Vamos excluir o mês em aberto

ggplot(data = distr_alimentos) + 
  geom_line(aes(x = mes , y = num_proc)) +
  geom_smooth(aes(x = mes , y = num_proc), method = "lm") +
  labs(x = "Mês",
       y = "Processos de Alimentos",
       title = "Processos Envolvendo o Direito a Alimentos")

#Média de vezes que um processo retorna ao MP

a <- table(distribuicao_df['processo'])
mean(a)


#table(distribuicao_df$tipo)
#vinculo_conjugal <- c("ANUL.CAS")
