#############################################################
##############           PUNTO 1         ####################
#############################################################
rm(list=ls())

library(readxl)
eje1 <- read_excel("/Users/federicofavata/Dropbox/Docencia/UCES/R/Solución/Base de Datos.xlsx", 
                              sheet = "RegresionMultiple")

regresion_multiple=lm(Y ~ x1+x2, data=eje1)
library(stargazer)
stargazer(regresion_multiple,type="text")


#############################################################
##############           PUNTO 2         ####################
#############################################################
rm(list=ls())


y=c(10,
    20,
    30,
    40,
    50)

x1=c(1,
     2,
     3,
     4,
     10)

x2=c(10,
     8,
     7,
     5,
     0)

datos=data.frame(y,x1,x2)

regresion=lm(y ~ x1+x2, data=datos)
library(stargazer)
stargazer(regresion,type="text")



### MANUAL
X=c(sum((x1-mean(x1))^2),sum((x1-mean(x1))*(x2-mean(x2))),sum((x1-mean(x1))*(x2-mean(x2))),sum((x2-mean(x2))^2) )
X
XX=matrix(X, nrow = 2, ncol = 2)
XX

XY=c(sum((x1-mean(x1))*(y-mean(y))),sum((x2-mean(x2))*(y-mean(y))))
XY=matrix(XY, ncol = 1)
XY

B=solve(XX)%*%XY
B



#############################################################
##############           PUNTO 3        ####################
#############################################################
rm(list=ls())

library(readxl)
Mincer <- read_excel("/Users/federicofavata/Dropbox/Docencia/UCES/R/Solución/Base de Datos.xlsx", 
                   sheet = "Mincer")

regresion=lm(salario ~ edad+experiencia, data=Mincer)
library(stargazer)
stargazer(regresion,type="text")

plot(Mincer$edad,Mincer$salario)
points(Mincer$edad,regresion$fitted.values, col="red")

plot(Mincer$experiencia,Mincer$salario)
points(Mincer$experiencia,regresion$fitted.values, col="red")


#############################################################
##############           PUNTO 4         ####################
#############################################################
rm(list=ls())

library(dplyr)
datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)

library(psych)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")


#install.packages("GGally")
library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")


reg=lm(formula = esp_vida ~ habitantes + ingresos + asesinatos 
     , data = datos)
summary(reg)
plot(datos$ingresos, datos$esp_vida)
points(datos$ingresos, reg$fitted.values , col = 'blue', lwd = 3)


#############################################################
##############           PUNTO 5         ####################
#############################################################
rm(list=ls())

x1 = rnorm(30000,10,4)
x2 = rnorm(30000,5,8)

y=2+4*x1+7*x2+rnorm(30000,0,1)

datos=data.frame(y,x1,x2)



library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

plot(x1,y)
plot(x2,y)

reg=lm(formula = y ~ x1 + x2 
       , data = datos)
summary(reg)
points(datos$x2, reg$fitted.values , col = 'red', lwd = 0.2)



##### A MANO
# B=(X?X)^-1X?Y


X=c(sum((x1-mean(x1))^2),sum((x1-mean(x1))*(x2-mean(x2))),sum((x1-mean(x1))*(x2-mean(x2))),sum((x2-mean(x2))^2) )
X
XX=matrix(X, nrow = 2, ncol = 2)
XX

XY=c(sum((x1-mean(x1))*(y-mean(y))),sum((x2-mean(x2))*(y-mean(y))))
XY=matrix(XY, ncol = 1)
XY
  
B=solve(XX)%*%XY
B



#### SIN ORDENADA
reg=lm(formula = y ~ 0+ x1 + x2 
       , data = datos)
summary(reg)


X=c(sum((x1)^2),sum((x1)*(x2)),sum((x1)*(x2)),sum((x2)^2) )
X
XX=matrix(X, nrow = 2, ncol = 2)
XX

XY=c(sum((x1)*(y)),sum((x2)*(y)))
XY=matrix(XY, ncol = 1)
XY

B=solve(XX)%*%XY
B


#############################################################
##############           PUNTO 6         ####################
#############################################################
rm(list=ls())

datos <- read.table("http://www.diegocalvo.es/wp-content/uploads/2016/09/datos-regresion-lineal-multiple.txt", header = TRUE)


pairs(datos)

require(corrplot)
corrplot(round(cor(datos), digits = 3), type = "lower")

modelo1<- lm(datos$Tiempo~datos$N_cajas+datos$Distancia, data=datos)
summary(modelo1)


modelo2<- lm(datos$Tiempo~0+datos$N_cajas+datos$Distancia, data=datos)
summary(modelo2)

library(stargazer)
stargazer(modelo1,modelo2,type="text")


#############################################################
##############           PUNTO 7         ####################
#############################################################
rm(list=ls())
#install.packages("eph")
library(eph)

eph=data.frame(get_microdata(year = 2018, trimester = 2, wave = NA,
                             type = "individual", vars = "all"))


eph=subset(eph, eph$P21>0 & eph$P21<100000 & eph$NIVEL_ED==5)


plot(eph$CH06,eph$P21)

eph$CH06_2=eph$CH06^2
reg <- lm(eph$P21 ~ eph$CH06 +eph$CH06_2 )
summary(reg)
points(eph$CH06, reg$fitted.values , col = 'red', lwd = 0.2)

#############################################################
##############           PUNTO 8         ####################
#############################################################
rm(list=ls())

x=rnorm(10000,10,3)
y= 300-6*x+8*x^2+rnorm(10000,0,1)

plot(x,y)

reg=lm(y ~ x)
summary(reg)
points(x, reg$fitted.values , col = 'red', lwd = 0.2)

x2=x*x
reg2=lm(y ~ x+x2)
summary(reg2)
plot(x,y)
points(x, reg2$fitted.values , col = 'red', lwd = 0.2)


plot(x,y)
points(x, reg$fitted.values , col = 'blue', lwd = 0.2)
points(x, reg2$fitted.values , col = 'red', lwd = 0.2)


#############################################################
##############           PUNTO 9         ####################
#############################################################
rm(list=ls())
#install.packages("eph")
library(eph)

eph=data.frame(get_microdata(year = 2018, trimester = 2, wave = NA,
                             type = "individual", vars = "all"))


eph=subset(eph, eph$P21>0 & eph$P21<100000 & eph$NIVEL_ED==4)


plot(eph$CH06,eph$P21)


eph$lnP21=log(eph$P21)
eph$lnCH06=log(eph$CH06)


### MODELOS LIN LIN
reg <- lm(eph$P21 ~ eph$CH06)
summary(reg)

### MODELOS LOG LIN
loglin <- lm(eph$lnP21 ~ eph$CH06)
summary(loglin)


### MODELOS LIN LOG
linlog <- lm(eph$P21 ~ eph$lnCH06)
summary(linlog)

### MODELOS LOg LOG
linlin <- lm(eph$lnP21 ~ eph$lnCH06)
summary(linlin)

#############################################################
##############           PUNTO 10         ####################
#############################################################
rm(list=ls())

x=runif(1000,1,100)
y=3*x+2+rnorm(1000,0,30)

plot(x,y)

# REGRESION NORMAL
reg=lm(y ~ x)
summary(reg)

# REGRESION con X*10
x10=10*x
reg=lm(y ~ x10)
summary(reg)

# REGRESION con Y*10
y10=y*10
reg=lm(y10 ~ x)
summary(reg)

# REGRESION con Y*10 y X*10
y10=y*10
x10=x*10
reg=lm(y10 ~ x10)
summary(reg)


#############################################################
##############           PUNTO 11         ####################
#############################################################
rm(list=ls())

# Seteo semilla
set.seed(21212)

obs=4000
beta0=1
beta1=5

x=runif(n=obs,min=0,max=10)

hist(x)


y=beta0  + beta1*x + rnorm(obs,mean = 0,sd=2)

plot(x,y)

# Regresion con todos los datos

reg=lm(y ~ x)
summary(reg)

confint(object=reg, parm="x", level=0.95)


# MONTE CARLO
number_MCrepetitions  <-    1000 
vector_MCbeta   <-   c()
for (i in 1:number_MCrepetitions) {
  
  obs=4000
  beta0=1
  beta1=5
  x=runif(n=obs,min=0,max=10)
  
  y=beta0  + beta1*x + rnorm(obs,mean = 0,sd=2)
  reg=lm(y ~ x)
  beta_estimated=reg$coefficients[2]
  
  vector_MCbeta[i] <- beta_estimated 
}

hist(vector_MCbeta)


#############################################################
##############           PUNTO 12        ####################
#############################################################
rm(list=ls())


runs <- 3000
#runif samples from a uniform distribution
xs <- runif(runs,min=-0.5,max=0.5)

ys <- runif(runs,min=-0.5,max=0.5)

in.circle <- xs^2 + ys^2 <= 0.5^2

mc.pi <- (sum(in.circle)/runs)*4

plot(xs,ys,pch='.',col=ifelse(in.circle,"blue","grey")
     ,xlab='',ylab='',asp=1,
     main=paste("MC Approximation of Pi =",mc.pi))