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

################################################################################
#                            Análise Quantitativa                              #
################################################################################

load(here("Documentos", "Distribuicao_df.RData"))
load(here("Documentos", "codigos_sp.RData"))
load(here("Documentos", "indicadores.RData"))

# Estatísticas univariadas
summary(distribuicao_df)

table(distribuicao_df$codigo)

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

#5 - Morosidade da Justiça
summary(distribuicao_df$tipo)

filter(distribuicao_df, tipo == "MS", codigo == "0000" )

morosidade <- distribuicao_df |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         comarca != "Competência Originária",
         #Selecionamos apenas os tipos de processo
         tipo %in% c("AJ","AP", "AJ", "HDATA", "MI", "RECURSO","REEXAME")) |>
  distinct(processo, .keep_all = TRUE) |>
  group_by(ano = floor_date(data, unit = "month")) |>
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
         tipo %in% c("AJ","AP", "AJ", "HDATA", "MI", "RECURSO","REEXAME")) |>
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
  filter(tipo %in% c("AJ","AP", "AJ", "HDATA", "MI", "RECURSO","REEXAME")) |>
  count(processo)

mean(retorno$n)
rm(retorno)

# Proporção entre comarcas

prop_comarcas <- distribuicao_df |>
  left_join(codigos[ , c("codigo", "comarca")], by = "codigo") |>
  filter(tribunal == "8.26",
         comarca != "Competência Originária",
         tipo %in% c("AJ","AP", "AJ", "HDATA", "MI", "RECURSO","REEXAME")) |>
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





################################################################################
#                   Analisando Assuntos Específicos                            #
################################################################################

#1 - Identificar as categorias das variáveis "Natureza"

summary(distribuicao_df$natureza)
table(distribuicao_df$natureza)
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

# Um incremento de 18,4% entre agosto de 2017 a julho de 2022
distr_alim <- alimento |> 
  group_by(mes = floor_date(data, unit = "month")) |>
  summarise(num_proc = n())|>
  ungroup () %>% droplevels(.) |>
  filter (mes < "2022-08-01")

m12 <- distr_alim |>
  group_by(indice = c(0, rep(1:(nrow(distr_mes)-1)%/%12))) |>
  summarise(num_proc = sum(num_proc))|>
  ungroup () %>% droplevels(.)  

(m12$num_proc[5]/m12$num_proc[1] - 1)*100

rm(distr_alim)
