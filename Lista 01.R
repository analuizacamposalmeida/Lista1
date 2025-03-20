#install.packages("dplyr")
library(readxl)
library(dplyr)

dados <- read_excel("C:/Users/User/Documents/R/airbnb_NYC.xlsx")
dados <- as.data.frame(dados)

# Mostrar como os dados estão classificados:
str(dados)

# Questão 01
dados$id <- as.character(dados$id)
dados$id_original <- as.character(dados$id_original)
dados$grupo_de_bairro <- as.factor(dados$grupo_de_bairro)
dados$bairro <- as.factor(dados$bairro)
dados$tipo <- as.factor(dados$tipo)
dados$latitude <- as.numeric(dados$latitude)
dados$longitude <- as.numeric(dados$longitude)
dados$diaria <- as.numeric(dados$diaria)
dados$minimo_noites <- as.numeric(dados$minimo_noites)
dados$nro_de_reviews <- as.numeric(dados$nro_de_reviews)
dados$reviews_por_mes <- as.numeric(dados$reviews_por_mes)
dados$disponibilidade <- as.numeric(dados$disponibilidade)
dados$number_of_reviews_ltm <- as.numeric(dados$number_of_reviews_ltm)

str(dados)
names(dados)

# Questão 02 - Amostragem
summary(dados)
n <- nrow(dados)
set.seed(utf8ToInt("K"))
linhas_selecionadas <- sample(1:n, 2000)
amostra <- dados[linhas_selecionadas,]

# letra A
summary(dados)

# letra B
summary(amostra)

# Letra C


# Questão 03
# Letra A - Número de imóveis em cada grupo_de_bairro
# DADOS
table_grpbairro <- table(dados$grupo_de_bairro)   # transformamos em tabela
df_grpbairro <- as.data.frame(table_grpbairro)
df_grpbairro
names(df_grpbairro) <- c("Grupo de Bairro", "Absoluta")
df_grpbairro
df_grpbairro <- df_grpbairro %>% mutate(Relativa=Absoluta/sum(Absoluta))
df_grpbairro
df_grpbairro <- df_grpbairro %>% mutate(Acumulada = cumsum(Relativa))
df_grpbairro

# AMOSTRAGEM
table_grpb <- table(amostra$grupo_de_bairro)   # transformamos em tabela
df_grpb <- as.data.frame(table_grpb)
df_grpb
names(df_grpb) <- c("Grupo de Bairro", "Absoluta")
df_grpb
df_grpb <- df_grpb %>% mutate(Relativa=Absoluta/sum(Absoluta))
df_grpb
df_grpb <- df_grpb %>% mutate(Acumulada = cumsum(Relativa))
df_grpb

# B) Frequência de cada tipo de imóvel
# DADOS
table_tipo <- table(dados$tipo)   # transformamos em tabela
df_tipo <- as.data.frame(table_tipo)
df_tipo
names(df_tipo) <- c("Tipo de Imóvel", "Absoluta")
df_tipo
df_tipo <- df_tipo %>% mutate(Relativa=Absoluta/sum(Absoluta))
df_tipo
df_tipo <- df_tipo %>% mutate(Acumulada = cumsum(Relativa))
df_tipo

# AMOSTRAGEM
table_tp <- table(amostra$tipo)   # transformamos em tabela
df_tp <- as.data.frame(table_tp)
df_tp
names(df_tp) <- c("Tipo de Imóvel", "Absoluta")
df_tp
df_tp <- df_tp %>% mutate(Relativa=Absoluta/sum(Absoluta))
df_tp
df_tp <- df_tp %>% mutate(Acumulada = cumsum(Relativa))
df_tp

# c) Comente os resultados obtidos


# Questão 04:
# DADOS
n_classes_dados <- round(1+3.322 * log10(nrow(dados)), 0)
classes_dados <- round(seq(0,10000,10000/n_classes_dados),0) 

# AMOSTRAGEM
n_classes_amostra <- round(1+3.322 * log10(nrow(amostra)), 0)
classes_amostra <- round(seq(0,4200,4200/n_classes_amostra),0) 

# a) Apresente tabelas de frequências nos intervalos de classes
# DADOS
hist_diaria_dados <- hist(dados$diaria,breaks = classes_dados,right = F,plot = F)
tab_diaria_dados <- data.frame(faixa=paste(hist_diaria_dados$breaks[-16],
                                           hist_diaria_dados$breaks[-1],sep="-"),
                               absoluta = hist_diaria_dados$counts)
tab_diaria_dados <- tab_diaria_dados %>% mutate(relativa = absoluta/sum(absoluta))
tab_diaria_dados <- tab_diaria_dados %>% mutate(acumulada = cumsum(relativa))

# AMOSTRAGEM
hist_diaria_amostra <- hist(amostra$diaria,breaks = classes_amostra,right = F,plot = F)
tab_diaria_amostra <- data.frame(faixa=paste(hist_diaria_amostra$breaks[-13],
                                           hist_diaria_amostra$breaks[-1],sep="-"),
                               absoluta = hist_diaria_amostra$counts)
tab_diaria_amostra <- tab_diaria_amostra %>% mutate(relativa = absoluta/sum(absoluta))
tab_diaria_amostra <- tab_diaria_amostra %>% mutate(acumulada = cumsum(relativa))

# b) Apresente histrogramas para diaria dos dados e da amostra
# DADOS
hist(dados$diaria,breaks = classes_dados,right = F, axes = F,
     freq= T,labels = T , ylim = c(0,17000), col = "darkslategray4")
axis(1,classes_dados)
axis(2,seq(0,17000,by=1700))
box()
# AMOSTRAGEM
hist(amostra$diaria,breaks = classes_amostra,right = F, axes = F,
     freq=T,labels = T , ylim = c(0,2000), col = "darkslategray4")
axis(1,classes_amostra)
axis(2,seq(0,2000,by=200))
box()
