#############################################################
##############           PUNTO 1         ####################
#############################################################
rm(list=ls())

x=rnorm(1000,1,10)
y=5+2*x+rnorm(1000,0,1)

plot(x,y)

### MODELO CON ORDENADA AL ORIGEN
summary(lm(y ~ x))

#A mano
#Y=b0+B1X+U
#b1= cov(x,y)/var(x)
b1=cov(x,y)/var(x)
b1

#b0= mean(y)-b1*mean(x)
b0= mean(y)-b1*mean(x)
b0

### MODELO SIN ORDENADA AL ORIGEN
#Y=B1X+U
summary(lm(y ~ 0+x))

b1=sum(x*y)/sum(x^2)
b1


###
reg=summary(lm(y ~ x))
plot(x,y)
abline(reg, col="red",lwd=3)



#############################################################
##############           PUNTO 2         ####################
#############################################################
rm(list=ls())

x=rnorm(10000,10,5)
y=1+3*x+rnorm(1000,0,2)


reg=lm(y ~ x)
summary(reg)
plot(x,y)
abline(reg, col="red",lwd=3)

plot(x,reg$fitted.values)
plot(x,reg$residuals)
mean(reg$resplotiduals)
sd(reg$residuals)

#############################################################
##############           PUNTO 3         ####################
#############################################################
rm(list=ls())

require(MASS)
require(ISLR)
data("Boston")
summary(Boston)


table(Boston$lstat)
hist(Boston$lstat)
plot(Boston$lstat,Boston$medv)


modelo_simple <- lm(data = Boston,formula = medv ~ lstat)
summary(modelo_simple)
plot(Boston$lstat,Boston$medv)
abline(modelo_simple, col="red",lwd=3)

plot(Boston$lstat,modelo_simple$residuals)
mean(modelo_simple$residuals)
sd(modelo_simple$residuals)

plot(modelo_simple)

#############################################################
##############           PUNTO 4         ####################
#############################################################
rm(list=ls())

data("cars")
summary(cars)

plot(cars$speed, cars$dist)
cor(cars$speed, cars$dist)


lm.dist.speed <- lm(cars$dist ~ cars$speed)
summary(lm.dist.speed)
abline(lm.dist.speed, col = "red")
-17.5791+3.9324*20 
(40+17.5791)/3.9324

# R CUADRADO EN REGRESION
summary(lm.dist.speed)
R2=0.6511
#SCT=SCE+SCR
#.....
#1=R2+SCR/SCT

# A MANO
r2=1-sum((lm.dist.speed$residuals)^2)/sum((cars$dist-mean(cars$dist))^2)
r2

# POR SER REGRESION SIMPLE
cor(cars$dist,cars$speed)*cor(cars$dist,cars$speed)

#############################################################
##############           PUNTO 5         ####################
#############################################################

rm(list=ls())
#install.packages("eph")
library(eph)

eph=data.frame(get_microdata(year = 2018, trimester = 2, wave = NA,
                             type = "individual", vars = "all"))


eph=subset(eph, eph$P21>0 & eph$P21<100000)

library(ggplot2)

eph$CH04 <- as.factor(eph$CH04)

# Basic violin plot
p <- ggplot(eph, aes(x=eph$CH04, y=eph$P21)) + 
  geom_violin()
p


plot(eph$CH06, eph$P21)
plot(eph$CH06[eph$NIVEL_ED==6], eph$P21[eph$NIVEL_ED==6])
plot(eph$CH06[eph$NIVEL_ED==6 & eph$CH04==2], eph$P21[eph$NIVEL_ED==6 & eph$CH04==2])
plot(eph$CH06[eph$NIVEL_ED==6 & eph$CH04==1], eph$P21[eph$NIVEL_ED==6 & eph$CH04==1])



reg <- lm(P21 ~ CH06, data=subset(eph,NIVEL_ED==6))
summary(reg)

regmujer <- lm(P21 ~ CH06, data=subset(eph,NIVEL_ED==6 & CH04==2))
summary(regmujer)

regvaron <- lm(P21 ~ CH06, data=subset(eph,NIVEL_ED==6 & CH04==1))
summary(regvaron)


library(stargazer)
stargazer(reg,regmujer,regvaron,type="text")

#############################################################
##############           PUNTO 6         ####################
#############################################################

rm(list=ls())
library(readxl)
Base_de_Datos <- read_excel("/Users/federicofavata/Dropbox/Docencia/UCES/R/Solución/Base de Datos.xlsx", 
                            sheet = "IngCons")
View(Base_de_Datos)

plot(Base_de_Datos$Ingreso,Base_de_Datos$Consumo)

salida <- lm(Consumo ~ Ingreso, data=Base_de_Datos)
summary(salida)

plot(Base_de_Datos$Ingreso,Base_de_Datos$Consumo)
abline(salida, col = "red")


plot(Base_de_Datos$Ingreso,salida$residuals)


#############################################################
##############           PUNTO 7         ####################
#############################################################

rm(list=ls())
library(readxl)
produc <- read_excel("/Users/federicofavata/Dropbox/Docencia/UCES/R/Solución/Base de Datos.xlsx", 
                            sheet = "Produccion")


plot(produc$Produccion,produc$Costo)

## Regresion lineal
salida <- lm(Costo ~ Produccion, data=produc)
library(stargazer)
stargazer(salida,type="text")

plot(produc$Produccion,produc$Costo)
abline(salida, col = "red")

# Coeficiente correlacion: Comando
cor(produc$Produccion,produc$Costo)

# Coeficiente correlacion: A Mano COV(X,Y)/(RAIZ(VAR(X))*RAIZ(VAR(Y)))
covarianza=cov(produc$Produccion,produc$Costo)
varx=var(produc$Produccion)
vary=var(produc$Costo)

correlacion=covarianza/(sqrt(varx)*sqrt(vary))
correlacion
#Pruebo que esigual
correlacion==cor(produc$Produccion,produc$Costo)

## RESULTADOS
resultados=data.frame(produc$Produccion, produc$Costo, salida$fitted.value,salida$residuals, (salida$residuals)^2)
View(resultados)


# PLOT ENTRE PRODUCCION Y ERROR
plot(resultados$produc.Produccion,resultados$salida.residuals)

# PLOT ENTRE PRODUCCION Y ERROR al cuadrado
plot(resultados$produc.Produccion,resultados$X.salida.residuals..2)

#############################################################
##############           PUNTO 8         ####################
#############################################################

rm(list=ls())

minutos=c(0,2,4,6,8)

costo=c(4, 4.4, 4.8, 5.2, 5.6)

datos=data.frame(costo,minutos)

plot(datos$minutos,datos$costo)

## Regresion lineal
salida <- lm(costo ~ minutos, data=datos)
summary(salida)
abline(salida, col = "red")


#############################################################
##############           PUNTO 9         ####################
#############################################################

rm(list=ls())

ansiedad=c(2,
          4,
          5,
          1,
          3)

aciertos=c(5,
           3,
           2,
           10,
           4)
  
ejercitacion=data.frame(ansiedad,aciertos)

regresion_lineal <- lm(aciertos ~ ansiedad, data=ejercitacion)
summary(regresion_lineal)

plot(ejercitacion$ansiedad,ejercitacion$aciertos, col="red")
abline(regresion_lineal, col = "blue")


#############################################################
##############           PUNTO 10         ####################
#############################################################

rm(list=ls())

obs=40

PBI=36
EXPO=3120
VAR_PBI=4
COV_PBI_EXPO=2.11

# IMPO=B0+B1*PBI+ U

#BETA1=COV(X,Y)/VAR(X)
BETA1=COV_PBI_EXPO/VAR_PBI
BETA1

#BETA0=YPROM- B1*XPROM
BETA0= EXPO/obs - BETA1*PBI/obs
BETA0

#FINALMENTE
c(BETA0,BETA1)

#############################################################
##############           PUNTO 11         ####################
#############################################################

rm(list=ls())

BETA=0.75
VAR_BETA=0.16
SD_BETA=sqrt(VAR_BETA)

t=BETA/SD_BETA
t

# Ho: NO ES SIGNIFICATIVO
# H1: ES SIGNIFICATIVO.

t_tabla=2.11

t<t_tabla
#NO ES SIGNIFICATIVO


#############################################################
##############           PUNTO 12         ####################
#############################################################

rm(list=ls())

BETA=2.8
VAR_BETA=0.11
SD_BETA=sqrt(VAR_BETA)

t=BETA/SD_BETA
t

# Ho: NO ES SIGNIFICATIVO
# H1: ES SIGNIFICATIVO.

t_tabla=2.11

t<t_tabla
#ES SIGNIFICATIVO


#############################################################
##############           PUNTO 13         ####################
#############################################################

rm(list=ls())

#A) Por cada unidad que aumenta el ingreso, el consumo en promedio aumenta en 0,702 unidades. En caso que el ingreso es cero,
# el consumo promedio es 155.30.

#B) Teoria macroecononica del consumo. CONSUMO=Co+a*Yd

#C)  ¿Son significativos?. Compare con un t de tabla de 2.12.

######Ingreso####
t=0.702/0.125 
t
# Al ser mayor de 2.12 (VER NEGATIVO), es significativo. Es decir, el ingreso incide en el consumo.

#######Constante####
# YA NOS DA EL T.
t=1.2696 
t

# Al ser menor de 2.12 (VER NEGATIVO), no es significativo. ¿Se debe sacar la constante? OJO!

#############################################################
##############           PUNTO 14         ####################
#############################################################

rm(list=ls())
library(readxl)
EjemploMuestreo <- read_excel("/Users/federicofavata/Dropbox/Docencia/UCES/R/Solución/Base de Datos.xlsx", 
                     sheet = "EjemploMuestreo")

summary(EjemploMuestreo)
dim(EjemploMuestreo)
nrow(EjemploMuestreo)

# Poblacional
plot(EjemploMuestreo$x,EjemploMuestreo$y, col="red")
FRP=lm(y ~ x, data=EjemploMuestreo)
summary(FRP)

# MUESTREO
set.seed(123)
Muestra1 = EjemploMuestreo[sample(1:10000, 120,), ]
Muestra2 = EjemploMuestreo[sample(1:10000, 300,), ]
Muestra3 = EjemploMuestreo[sample(1:10000, 4000,), ]


FRM1=lm(y ~ x, data=Muestra1)
FRM2=lm(y ~ x, data=Muestra2)
FRM3=lm(y ~ x, data=Muestra3)


library(stargazer)
stargazer(FRM1,FRM2,FRM3,FRP,type="text")

  
