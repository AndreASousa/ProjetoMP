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
             "gridExtra" #para a funcao grid arrange
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
#                            Criando a Tabela                                  #
################################################################################

# Tabela com os indicadores socioeconômicos das comarcas paulistas
  # (Criado em: 004 - Indicadores_Socioeconomicos.R)
load(here("Documentos", "indicadores.RData"))

# selecionando os indicadores correspondentes a 2019
comarcas <- indicadores |>
  filter(ano == 2019) |>
  dplyr::select(comarca, populacao, salario, ocupados, pib_pc, idh) |>
  as.data.frame()
  
# Transformando os nomes de comarcas em nomes de linhas
rownames(comarcas) <- comarcas$comarca
comarcas$comarca <- NULL

# Padronizando as variáveis
comarcas_pad <- scale(comarcas)

################################################################################
#                             Análise de Agrupamento                           #
################################################################################

# Como o volume de dados não é muito grande,
  # Optamos pelo cluster hierárquico

# Calculando a matriz de distancias euclidianas
distancia <- dist(comarcas_pad, method = "euclidean")

#Calculando os Clusters
  # metodos disponiveis: "average", "single", "complete" e "ward.D"
cluster.hierarquico <- hclust(distancia, method = "single")

# Verificando o Elbow 
fviz_nbclust(comarcas_pad, FUN = hcut, method = "wss")

# Criando 4 grupos de Comarcas
grupo_comarca <- cutree(cluster.hierarquico, k = 4) |>
  data.frame()
names(grupo_comarca) <- "cluster"

table(grupo_comarca)

# Agrupando o cluster à Base Original
grupo_comarca <- tibble::rownames_to_column(grupo_comarca, "comarca")

indicadores <-  indicadores|>
  left_join(grupo_comarca, by = "comarca") |>
  dplyr::select(ano, comarca, cluster, populacao, everything())

# Vejamos como ficou cada grupo:
indicadores |>
  filter(cluster == 3) |>
  select(comarca) |>
  unique()

# Por que Barueri ficou sozinho em um cluster?
comarcas_pad["Barueri", ]
indicadores |>
  filter(comarca == "Barueri", ano == 2019)

comarcas_pad[c("Ilhabela", "Louveira", "Paulínia"), ]
indicadores |>
  filter(comarca == "Barueri", ano == 2019)

  
save(indicadores, file="Documentos/indicadores.RData")

