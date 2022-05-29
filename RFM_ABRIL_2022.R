setwd("C:/Users/ACER/Desktop/DSA/RFM")
getwd()

# Carregando os pacotes
library(dplyr)
library(arules)
library(arulesViz)
library(htmlwidgets)
library(writexl)
library(readxl)

# Carrengando e explorando o dataset
dados <- read_excel("SKUs_ABRIL_2022.xlsx")
dim(dados)
summary(dados)
str(dados)
View(dados)

# Verificando se temos valores ausentes no primeiro e segundo item de compra
sum(is.na(dados$`1`))
sum(is.na(dados$`2`))

# Verificando se temos valores ausentes representados por espaço em branco (usando expressão regular)
grepl("^\\s*$", dados$`2`)

# Número de itens distintos
n_distinct(dados)

# Número de itens distintos no segundo item de compra
n_distinct(dados$`2`)

# Preparando o dataset e convertendo as variáveis para o tipo fator 
# (variáveis que usaremos daqui em diante)

dados_02 <- dados
dados_02$`1` <- as.factor(dados_02$`1`)
dados_02$`2` <- as.factor(dados_02$`2`)

# Utilizarei apenas a associação de  2 produtos
dados_03 <- select(dados_02, `1`, `2`)
dados_03

# Verificando as regras:
regras_abril <- apriori(dados_03, parameter = list(supp = 0.027, conf = 0.9))

# Filtrando as regras redundantes
regras_abril_clean <- regras_abril[!is.redundant(regras_abril)]

# Regras
regras_abril_clean

# Inspeção das regras
inspect(regras_abril_clean)

# Convertendo para um dataframe
abril_df <- as(regras_abril_clean, "data.frame")


# Visualizando o dataframe, ordenado pelo maior número de associções
View(arrange(abril_df, desc(count)))

# Plotando
plot(regras_abril_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Salvando o arquivo no formato xlsx
write_xlsx(abril_df, "ASSOCIACOES_ABRIL_2022.xlsx")
