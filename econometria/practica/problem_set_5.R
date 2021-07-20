
library(readxl)
library(stargazer)
rm(list=ls())
datos<-read_excel("Base de Datos.xlsx",sheet = "RegresionMultiple")


regressor<-lm(Y~.,data = datos)
summary(regressor)
stargazer(regressor,type = "text")


t1<-0.447/0.104
t2<- -0.268/0.120
t3<- 5.412/1.384
tcritico<-2.7

ifelse(t1>tcritico,"significativo","no significativo")

ifelse(t2>tcritico,"significativo","no significativo")

ifelse(t3>tcritico,"significativo","no significativo")

# Con un tcritico de 2.7 el interceptor es significativo, x1 es significativo pero x2 no es significativo. 


####################################
###          Ejercicio 2 ###########
####################################

y<-c(10,20,30,40,50)
x1<-c(1,2,3,4,10)
x2<-c(10,8,7,5,0)

datos<-data.frame(y,x1,x2)

regressor2<-lm(y~.,data = datos)
summary(regressor2)
stargazer(regressor2,type="text")

x<-cbind(x1,x2)

betas<-solve(t(x) %*% x) %*% (t(x)%*%y)
betas



####################################
###          Ejercicio 4 ###########
####################################


rm(list=ls())
#install.packages("dplyr")
#install.packages("psych")
#install.packages("GGally")

library(dplyr)
datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)

library(psych)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")


library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

cor(datos)

reg=lm(formula = esp_vida ~ habitantes + ingresos + asesinatos 
       , data = datos)
summary(reg)
plot(datos$ingresos, datos$esp_vida)
points(datos$ingresos, reg$fitted.values , col = 'blue', lwd = 3)


