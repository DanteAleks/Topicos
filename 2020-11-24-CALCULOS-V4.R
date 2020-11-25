### Calculos para os resultados do topico Emprego e Renda ###

#install.packages("openxlsx")
#install.packages("tidyverse")

### Importando as bases de dados
library(openxlsx)

# Base 1: Matriz de Leontief
base1 <- read.xlsx("2020-11-23-BASES-EMPREGO-E-RENDA.xlsx",sheet=6)

# Base 2: RAIS compilada IBGE
base2 <- read.xlsx("2020-11-23-BASES-EMPREGO-E-RENDA.xlsx",sheet=7)

# Base 3: Distancias entre municipios
base3 <- read.xlsx("2020-11-23-BASES-EMPREGO-E-RENDA.xlsx",sheet=8)

# Calcular a equacao matricial: X=B*Y
# X = vetor producao
# B = (I-A)^-1 = inversa da Matriz de Leontief; já é a Base 1 do IBGE
# Y = demanda final

class(base1) # Data.frame
base1_mat <- data.matrix(base1[,-1]) # transformando Leontief em matriz

# Definicao dos inputs (valores de entrada)

setores <- colnames(base1[-1]) # Vetor com o nome dos 67 setores
length(setores) # Quantidade de setores produtivos
setor_escolhido <- 2 # Usuario pode escolher de 1 a 67 (exemplo: 15)
aumento_demanda <- 100 # Numero de unidades produzidas a mais no setor escolhido (exemplo: 300)

# Vetor coluna: demanda final
Y = matrix(c(rep.int(0,(setor_escolhido-1)),aumento_demanda,rep.int(0,(length(setores)-setor_escolhido))),nrow=length(setores))

X = base1_mat %*% Y # Calculo do vetor producao para o aumento da demanda (Y)

# Impacto em todos os setores devido ao aumento da demanda final no setor escolhido
delta <- X-base1_mat[,setor_escolhido] # variacao do vetor producao: final (X) menos inicial (Leontief)

# Selecao do municipio

# Primeiro: usuario devera selecionar a sigla da UF, em letras maiusculas
uf <- "RO" # UF selecionada pelo usuario (exemplo: Rondonia - RO)

base2$uf <- str_sub(base2$`Municipio/Setores`,8,9) # Inserindo a coluna UF na Base 2
base2$municipios <- str_sub(base2$`Municipio/Setores`,11) # Inserindo a coluna Municipios na Base 2
base2$codigo <- as.character(str_sub(base2$`Municipio/Setores`,1,6)) # Inserindo a coluna com codigo do municipio na Base 2

base2_uf <- base2[base2$uf==uf,] # Selecionado as linhas da UF indicada pelo usuario
municipios <- str_sub(base2_uf$municipio) # Nomes dos municipios da UF selecionada
length(municipios) # Quantidade de municipios para a UF escolhida
mun_selecionado <- municipios[17] # Municipio da UF selecionado pelo usuario (exemplo: 17 - Porto Velho)
# Escolhe o municipio via numero do vetor "municipios[]"

# Manipulando os dados da Base 3 (distancia entre municipios)
# Limitacao: a Base 3 considera somente os municipios de Rondonia como origem
# Por isso, os calculos somente serao realizados para a UF=="RO"

head(base3)

base3$uf_origem <- str_sub(base3$origem_uf_mun,1,2) # Insere coluna com a UF da origem
base3$mun_origem <- str_sub(base3$origem_uf_mun,4) # Insere coluna com o nome do municipio da origem
base3$uf_destino <- str_sub(base3$destino_uf_mun,1,2) # Insere coluna com a UF do destino
base3$mun_destino <- str_sub(base3$destino_uf_mun,4) # Insere coluna com nome do municipio do destino

table(base3$uf_origem) # Confirma que a Base 3 possui somente municipios de RO como origem
table(base3$uf_destino) # Confirma que como destino são previstos todos os municipios brasileiros

base3$distancia <- base3$distancia/1000

# Equacao Gravitacional
# Municipio A: escolhido pelo usuario
# Municipio B: demais municipios afetados pelo aumento da demanda
# O limite geografico do impacto sera a UF, dada a limitacao da base de dados das distancias

codigo_mun_a <- base2_uf[base2_uf$municipios==mun_selecionado,71] # Codigo do municipio A

alpha <- 1 # Parametro da equacao que pode, ou nao, ser definido: default = 1
beta <- 1 # Parametro da equacao que pode, ou nao, ser definido: default = 1

fator_1 <- sum(base2[,setor_escolhido+1])

codigo_mun_b <- base3[base3$origem==codigo_mun_a,2]
distancia_a_b <- base3[base3$origem==codigo_mun_a,3]
indice <- alpha*log(fator_1)+beta*log(1/distancia_a_b)

base3_origem <- subset(base3,base3$origem==codigo_mun_a)
base3_origem$indice <- indice

colnames(base2)[71] <- "codigo_mun_b"
colnames(base3_origem)[2] <- "codigo_mun_b"
base_final <- merge(base2,base3_origem,by="codigo_mun_b", all=FALSE)

# Distribuindo o efeito para os municipios

indice_a <- alpha*log(fator_1)

efeito <- (indice/(indice_a+indice))*delta[setor_escolhido,]
base_final$efeito <- efeito

print(efeito)

basemap5 <- base_final %>% select(codigo_mun_b, mun_destino, uf_destino, distancia, efeito) %>% 
  rename("Codigo Municipio de Destino" = "codigo_mun_b", "Municipio de destino" = "mun_destino", "UF de destino" = "uf_destino", "Distancia entre Municipio" = "distancia", "Efeito do Investimento" = "efeito")

head(basemap5$`Codigo Municipio de Destino`,4)
head(shp$CD_MUN,4)

mapa4 <- merge(shp, basemap5, by.x = "CD_MUN", by.y = "Codigo Municipio de Destino", duplicateGeoms = TRUE)

mapa5<- filter(mapa4, SIGLA_UF == "AC")


tmap_mode("view")
tm_shape(mapa5)+
  tm_polygons("Efeito do Investimento",n = 7, palette = mycols) 


# CONCLUSAO: Agora é plotar no mapa os valores dos efeitos nos municipios correspondentes




