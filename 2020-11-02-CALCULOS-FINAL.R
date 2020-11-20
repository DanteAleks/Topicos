### C?lculos para os resultados do topico Emprego e Renda ###

install.packages("openxlsx")
install.packages("dplyr")
install.packages("stringr")

### Importando as bases de dados
library(openxlsx)

# Base 1: Matriz de Leontief
base1 <- read.xlsx("C:/Documentos/Topicos/BASES-EMPREGO-E-RENDA.xlsx",sheet=6)

# Base 2: RAIS compilada IBGE
base2 <- read.xlsx("C:/Documentos/Topicos/BASES-EMPREGO-E-RENDA.xlsx",sheet=7)

# Base 3: Distancias entre municipios
base3 <- read.xlsx("C:/Documentos/Topicos/BASES-EMPREGO-E-RENDA.xlsx",sheet=8)

# Calcular a equacao matricial: X=B*Y
# X = vetor producao
# B = (I-A)^-1 = inversa da Matriz de Leontief; já é a Base 1 do IBGE
# Y = demanda final

class(base1) # data.frame
base1_mat <- data.matrix(base1[,-1]) # transformando Leontief em matriz

# Definicao dos inputs (valores de entrada)

setores <- colnames(base1[-1]) # vetor com o nome dos 67 setores
setores
length(setores) # quantidade de setores produtivos
setor_escolhido <- 3 # usuario pode escolher de 1 a 67
setor_escolhido
aumento_demanda <- 30 # numero de unidades produzidas a mais no setor escolhido

# Vetor coluna: demanda final
Y = matrix(c(rep.int(0,(setor_escolhido-1)),aumento_demanda,rep.int(0,(length(setores)-setor_escolhido))),nrow=length(setores))
Y
X = base1_mat %*% Y # calculo do vetor producao para o aumento da demanda (Y)



# Impacto em todos os setores devido ao aumento da demanda final no setor escolhido
delta <- X-base1_mat[,setor_escolhido] # variacao do vetor producao: final (X) menos inicial (Leontief)
delta
head(delta)



# Selecao do municipio



library(dplyr)
library(stringr)



# Primeiro: usuario devera selecionar a sigla da UF, em letras maiusculas
uf <- "SP" # UF selecionada pelo usuario



base2$uf <- str_sub(base2$`Municipio/Setores`,8,9) # inserindo a coluna UF na Base 2
base2$municipios <- str_sub(base2$`Municipio/Setores`,11) # inserindo a coluna Municipios na Base 2
base2$codigo <- str_sub(base2$`Municipio/Setores`,1,6) # inserindo a coluna com codigo do municipio na Base 2



base2_uf <- base2[base2$uf==uf,] # selecionado as linhas da UF indicada pelo usuario
municipios <- str_sub(base2_uf$municipio) # nomes dos municipios da UF selecionada
length(municipios) # quantidade de municipios para a UF escolhida
mun_selecionado <- municipios[563] # Municipio da UF selecionado pelo usuario
# Escolhe o municipio via numero do vetor "municipios[]"
# Como exemplo: municipio 563 da lista de municipios da UF "SP" e "SAO PAULO"

# Equacao Gravitacional
# Municipio A: escolhido pelo usuario
# Municipio B: demais municipios afetados pelo aumento da demanda
# O limite geografico do impacto sera a UF, dada a limitacao da base de dados das distancias

codigo_mun_a <- base2_uf[base2_uf$municipios==mun_selecionado,71] # codigo do municipio A
codigo_mun_a

alpha <- 1 # parametro da equacao que pode, ou nao, ser definido: default = 1
beta <- 1 # parametro da equacao que pode, ou nao, ser definido: default = 1

fator_1 <- base2_uf[,setor_escolhido+1]
fator_2 <- base3[base3$destino==codigo_mun_a,3]
i_mun_b = alpha*log(fator_1)+beta*log(fator_2) # calculo dos indices para cada municipio da UF escolhida
i_mun_b
base2_uf$indice <- i_mun_b # incluindo a coluna "indice" na Base 2_UF

# Distribuindo o efeito para os municipios

i_mun_a <- base2_uf[base2_uf$municipios==mun_selecionado,72]

efeito <- (i_mun_b/(i_mun_a+sum(base2_uf$indice>0)))*delta[setor_escolhido,]
efeito

base2_uf$efeito <- efeito # incluindo a variavel "efeito" na Base 2_UF



# CONCLUSAO:



# Dada a limitacao da base de dados 3, referente a distancia entre os municipios,
# o efeito do aumento da demanda em um municipio de uma UF sera calculado nos
# outros municipios da mesma UF, representado pela variavel "efeito" na base
# de dados 2 referente a UF escolhida "base2_UF"!
# Obs: alguns municipios apresentam valor para o efeito como "-Inf", pois esses
# municipios nao constam na base de dados 3, da distancia entre municipios, portanto,
# nao terao os respectivos efeitos representados.

