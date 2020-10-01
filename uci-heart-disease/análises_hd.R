# SCRIPT DA ANÁLISE ESTATÍSTICA DO BANCO HEARTDISEASE ????

library(tidyverse)
library(broom)
library(nortest)
library(RVAideMemoire)
library(MASS)

# abrindo o banco
dados <- read.csv("datasets_33180_43520_heart.csv")
names(dados)[names(dados)=="ï..age"] <- "age" 
sapply(dados,class)
dados <- dados %>% mutate(across(-c(age,trestbps,chol,thalach,oldpeak), factor))

head(dados)


# verificando se existe correlação entre oldpeak e slope (já que ambas tem a ver com o segmento ST)
ajuste = lm(oldpeak ~ slope, data=dados)
summary(ajuste)
fiv <- 1/(1-0.3312)
fiv
# como o FIV foi menor do que 5, concluímos que não há multicolinearidade entre essas variáveis

# inspecionando o modelo cheio
mod_cheio <- glm(formula = target ~ age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal, 
            family = binomial(link = "logit"), data = dados)
summary(mod_cheio)

# utilizando o método stepwaise para construção do modelo
step(glm(target~1, data=dados, family = binomial(link="logit")), 
     scope=target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal, 
     direction = "both")

# o modelo abaixo apresenta o menor AIC
mod1 <- glm(formula = target ~ thal + ca + cp + oldpeak + slope + sex + trestbps + exang, 
            family = binomial(link = "logit"), data = dados)
summary(mod1)
# apenas as variáveis ca, cp, oldpeak, sex, trestbps e exang tem coeficientes significativos

mod2 <- glm(formula = target ~ ca + cp + oldpeak + sex + trestbps + exang, 
            family = binomial(link = "logit"), data = dados)
summary(mod2)
# como nem todos os níveis da variável ca são significativos, vamos tentar um modelo sem ela

mod3 <- glm(formula = target ~ cp + oldpeak + sex + trestbps + exang, 
            family = binomial(link = "logit"), data = dados)
summary(mod3)

plot(mod3, which=1)
# todos os coeficientes do modelo 3 são significativos
cbind(coef(mod3), confint(mod3))

# estatística generalizada de pearson para mod2
x2p_mod3 = sum(residuals(mod3, type = "pearson")^2 );
x2p.valorp_mod3 = 1-pchisq(x2p_mod3, 303-8);
x2p.valorp_mod3
# para alfa=0.05, temos que a deviance deste modelo é moderada ou pequena; ou seja, o ajuste é bom


# ANÁLISE DE RESÍDUOS

dc = cooks.distance(mod3)
# apenas o coeficiente ca no nível 4 não é significativo
dc[dc > 4/303] 
dc[dc > 0.02] 
# foram identificados alguns pontos influentes e alavancas
hatvalues(mod3)[hatvalues(mod3) > 2*length(coef(mod3))/303]

# resíduos versos valores ajustados
plot(mod3, which=1)
# a curva de lowess está com uma inclinação muito pequena, mas não se afasta do eixo 0


#####################################

dados_pto <- dados[-c(35,43,53,56,98,101,102,121,140,151,172,173,178,183,217,229,231,252,
  255,268,278,279,287,294,297,300,303),]

dados_pto <- dados[-c(1,2,9,14,18,25,35,43,53,56,67,78,80,84,98,101,102,111,118,121,140,151,
                      153,172,173,178,183,204,217,223,229,231,242,248,249,252,
                      255,260,264,268,272,278,279,287,294,297,300,303),]

#################################
# repetindo os procedimentos com os dados sem os pontos influentes
# utilizando o método stepwaise para construção do modelo
step(glm(target~1, data=dados_pto, family = binomial(link="logit")), 
     scope=target~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal, 
     direction = "both")

# o modelo abaixo apresenta o menor AIC
mod1_pto <- glm(formula = target ~ cp + ca + oldpeak + thal + slope + trestbps + 
                  sex + exang + restecg, family = binomial(link = "logit"), 
                data = dados_pto)
summary(mod1_pto)
# apenas as variáveis cp, oldpeak, sex, trestbps, exang e restecg tem coeficientes significativos
plot(mod1_pto, which=1)

mod2_pto <- glm(formula = target ~ cp + oldpeak + sex + trestbps + exang + restecg, 
                family = binomial(link = "logit"), data = dados_pto)
summary(mod2_pto)

# apenas o segundo nível da variavel restecg não apresenta coef significativo
cbind(coef(mod2_pto), confint(mod2_pto))
# avaliando a deviance
anova(mod2_pto, test="LR")

# faremos um modelo sem a variável restecg 
mod3_pto <- glm(formula = target ~ cp + oldpeak + sex + trestbps + exang, 
                family = binomial(link = "logit"), data = dados_pto)
summary(mod3_pto)

anova(mod3_pto, test="LR")

# estatística generalizada de pearson para mod3_pto
x2p_mod3_pto = sum(residuals(mod3_pto, type = "pearson")^2 );
x2p.valorp_mod3_pto = 1-pchisq(x2p_mod3_pto, length(dados_pto$target)-8);
x2p.valorp_mod3_pto
# para alfa=0.05, temos que a deviance deste modelo é moderada ou pequena; ou seja, o ajuste é bom


# ANÁLISE DE RESÍDUOS

dc_pto = cooks.distance(mod3_pto)
# apenas o coeficiente ca no nível 4 não é significativo
dc_pto[dc_pto > 4/303] 
# foram identificados alguns pontos influentes e alavancas
hatvalues(mod3_pto)[hatvalues(mod3_pto) > 2*length(coef(mod3_pto))/303]
# ainda encontramos algumas alavancas e outliers

# resíduos versos valores ajustados
plot(mod3_pto, which=1)
# a curva de lowess está melhor que a do modelo com os pontos influentes

# o ajuste melhorou com relação ao modelo com os pontos influentes
# portanto usaremos este modelo para a inferência (mod3_pto)


# TABELAS DE COEFS E VALORES-P
estimate_coef <- summary(mod3_pto)$coeff[,1]
valorp_coef <- summary(mod3_pto)$coeff[,4]
coef <- data.frame(estimate_coef,valorp_coef)
write_csv(coef,"coef.csv")
deviance <- summary(mod3_pto)$deviance
aic <- summary(mod3_pto)$aic
est_pearson = sum(residuals(mod3_pto, type = "pearson")^2 )
est_pearson_valorp = 1-pchisq(est_pearson, length(dados_pto$target)-8)

dados_deviance <- data.frame(deviance,est_pearson,est_pearson_valorp,aic)
write_csv(dados_deviance,"deviance.csv")


# resultados

plot(scale(exp(predict(mod3_pto))))

deltaMethod(mod3_pto,"exp(oldpeak)")
100*(deltaMethod(mod3_pto,"exp(oldpeak)")[,1]-1)
# diminuição de 79,23% na odds
deltaMethod(mod3_pto,"exp(trestbps)")
100*(deltaMethod(mod3_pto,"exp(trestbps)")[,1]-1)
# diminuição de 2,73% na odds
# cp, sex, exang
deltaMethod(mod3_pto,"exp(cp1)")
100*(deltaMethod(mod3_pto,"exp(cp1)")[,1]-1)
# aumento de 904% na odds
deltaMethod(mod3_pto,"exp(cp2)")
100*(deltaMethod(mod3_pto,"exp(cp2)")[,1]-1)
# aumento de 2958% na odds
deltaMethod(mod3_pto,"exp(cp3)")
100*(deltaMethod(mod3_pto,"exp(cp3)")[,1]-1)
# nenhum dos níveis de cp tem impacto significativo sobre a odds, pois seus intervalos incluem 1

deltaMethod(mod3_pto,"exp(sex1)")
100*(deltaMethod(mod3_pto,"exp(sex1)")[,1]-1)
# diminuição de 92.93% na odds se o indivíduo for de mulher para homem
deltaMethod(mod3_pto,"exp(exang1)")
100*(deltaMethod(mod3_pto,"exp(exang1)")[,1]-1)
# diminuição de 78.32% na odds se o indivíduo apresentar angina induzida por exercício


odds <- data.frame(rbind(deltaMethod(mod3_pto,"exp(cp1)"),
      deltaMethod(mod3_pto,"exp(cp2)"),
      deltaMethod(mod3_pto,"exp(cp3)"),
      deltaMethod(mod3_pto,"exp(oldpeak)"),
      deltaMethod(mod3_pto,"exp(sex1)"),
      deltaMethod(mod3_pto,"exp(trestbps)"),
      deltaMethod(mod3_pto,"exp(exang1)")))

odds <- round(odds[,-2],3)
odds$impacto <- c(paste(round(100*(deltaMethod(mod3_pto,"exp(cp1)")[,1]-1),2),"%"),
                  paste(round(100*(deltaMethod(mod3_pto,"exp(cp2)")[,1]-1),2),"%"),
                  paste(round(100*(deltaMethod(mod3_pto,"exp(cp3)")[,1]-1),2),"%"),
                  paste(round(100*(deltaMethod(mod3_pto,"exp(oldpeak)")[,1]-1),2),"%"),
                  paste(round(100*(deltaMethod(mod3_pto,"exp(sex1)")[,1]-1),2),"%"),
                  paste(round(100*(deltaMethod(mod3_pto,"exp(trestbps)")[,1]-1),2),"%"),
                  paste(round(100*(deltaMethod(mod3_pto,"exp(exang1)")[,1]-1),2),"%"))
write_csv(odds,"odds.csv")



