# ANÁLISE ESTATÍSTICA DO BANCO IRIS

library(tidyverse)
library(PMCMR)

# Preparando o banco iris.data
dados_iris <- read.table("iris.data",sep=",")
# renomeando variáveis
colnames(dados_iris) <- c("sepal_length","sepal_width","petal_length","petal_width","species") 
dados_iris$species <- gsub("Iris-", "", dados_iris$species) #retirando o prefixo "Iris-" das classes por simplicidade

# separando em bancos por grupo (species) para fazer a comparação de médias
dados_setosa <- dados_iris %>% filter(species=="setosa") %>% select(sepal_length)
dados_virginica <- dados_iris %>% filter(species=="virginica")
dados_versicolor <- dados_iris %>% filter(species=="versicolor")

# verificando normalidade das amostras de sepal_length
shapiro.test(dados_setosa$sepal_length)     # é normal
shapiro.test(dados_virginica$sepal_length)  # é normal
shapiro.test(dados_versicolor$sepal_length) # é normal
# todas são normais, pode-se usar anova

# anova para sepal_length
mod1 <- lm(sepal_length~species,data=dados_iris)
anova(mod1)
# como o valor-p de 2.2e-16 foi menor que o alfa, rejeitamos H0: existe diferença entre as médias

# comparações múltiplas para sepal_length
tuckey <- HSD.test(mod1, "species")
tuckey$groups
# existe diferença entre o comprimento da sépala para as três espécies


# verificando normalidade das amostras de sepal_width
shapiro.test(dados_setosa$sepal_width)      # é normal
shapiro.test(dados_virginica$sepal_width)   # é normal
shapiro.test(dados_versicolor$sepal_width)  # é normal
# todas são normais, pode-se usar anova

# anova para sepal_width
mod2 <- lm(sepal_width~species,data=dados_iris)
anova(mod2)
# como o valor-p de 2.2e-16 foi menor que o alfa, rejeitamos H0: existe diferença entre as médias

# comparações múltiplas para sepal_width
tuckey2 <- HSD.test(mod2, "species")
tuckey2$groups
# existe diferença entre a largura da sépala para as três espécies


# verificando normalidade das amostras de petal_length
shapiro.test(dados_setosa$petal_length)     # é normal
shapiro.test(dados_virginica$petal_length)  # é normal
shapiro.test(dados_versicolor$petal_length) # é normal
# todas são normais, pode-se usar anova

# anova para petal_length
mod3 <- lm(petal_length~species,data=dados_iris)
anova(mod3)
# como o valor-p de 2.2e-16 foi menor que o alfa, rejeitamos H0: existe diferença entre as médias

# comparações múltiplas para petal_length
tuckey3 <- HSD.test(mod3, "species")
tuckey3$groups
# existe diferença entre o comprimento da pétala para as três espécies


# verificando normalidade das amostras de petal_width
shapiro.test(dados_setosa$petal_width)      # NÃO é normal 
shapiro.test(dados_virginica$petal_width)   # é normal
shapiro.test(dados_versicolor$petal_width)  # NÃO é normal
# nem todas são normais, deve-se usar Kruskal-Wallis

# kruskal_wallis para petal_width
kruskal.test(petal_width~species,data=dados_iris)
# # como o valor-p de 2.2e-16 foi menor que o alfa, rejeitamos H0: existe diferença entre as médias

# comparações múltiplas para petal_width
posthoc.kruskal.nemenyi.test(dados_iris$petal_width ~ factor(dados_iris$species))
# existe diferença entre a largura da sépala para as três espécies

