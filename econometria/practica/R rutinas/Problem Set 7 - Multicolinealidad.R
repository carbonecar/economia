#############################################################
##############           PUNTO 1         ####################
#############################################################
rm(list=ls())

lambda=2

x1 = rnorm(30000,10,4)
x2 = lambda*x1

y=2+4*x1+7*x2+rnorm(30000,0,1)

datos=data.frame(y,x1,x2)

cor(x1,x2)


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

#############################################################
##############           PUNTO 2         ####################
#############################################################
rm(list=ls())

# a) A pesar de la multicolinealidad perfecta, los estimadores de MCO son MELI.
# Falso. No se puede obtener todos los betas.


# b) Las correlaciones altas entre parejas de regresoras no sugieren una alta multicolinealidad.
# Falso. Indica multicolinealidad.

#############################################################
##############           PUNTO 3        ####################
#############################################################
rm(list=ls())

x1 = rnorm(30000,1,2)
x2 = x1+rnorm(30000,0,1)

cor(x1,x2)

y=2+5*x1+2*x2+rnorm(30000,0,1)
datos=data.frame(y,x1,x2)

reg=lm(formula = y ~ x1 + x2 
       , data = datos)
summary(reg)

#############################################################
##############           PUNTO 4        ####################
#############################################################
rm(list=ls())

x1 = rnorm(30000,1,2)
x2 = x1+rnorm(30000,0,0.001)
cor(x1,x2)
y=2+6*x1+4*x2+rnorm(30000,0,1)
datos=data.frame(y,x1,x2)
reg=lm(formula = y ~ x1 + x2 
       , data = datos)
summary(reg)

#############################################################
##############           PUNTO 5       ####################
#############################################################
rm(list=ls())

x1 = rnorm(30000,1,2)
x2 = x1+rnorm(30000,0,10)
cor(x1,x2)
y=2+6*x1+4*x2+rnorm(30000,0,1)
datos=data.frame(y,x1,x2)
reg=lm(formula = y ~ x1 + x2 
       , data = datos)
summary(reg)



#############################################################
##############           PUNTO 6       ####################
#############################################################
rm(list=ls())
library(stargazer)


consumo=c(6127.9,
          6863.1,
          10687.4,
          5518.4,
          9783.9,
          4828.9,
          8614.3,
          4311.9,
          5216.1,
          7486.6,
          4898.3,
          7924.3,
          4473.3,
          9409.9,
          9000.1,
          6316.5,
          5176.0)

ingresop=c(8467.0,
           9113.0,
           14162.0,
           7392.0,
           13067.0,
           6357.0,
           11969.0,
           5647.0,
           6323.0,
           9638.0,
           6159.0,
           10229.0,
           5989.0,
           13147.0,
           12003.0,
           7895.0,
           6500.0)

ingresod=c(79.1,
           82.8,
           128.7,
           66.0,
           121.0,
           56.8,
           111.9,
           50.9,
           58.0,
           90.1,
           56.5,
           95.6,
           54.4,
           121.7,
           107.2,
           71.1,
           58.6)

datos=data.frame(consumo,ingresop,ingresod)

library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")


## MODELO COMPLETO

reg=lm(formula = consumo ~ ingresod + ingresop 
       , data = datos)
stargazer(reg,type="text")
# ¿Que pasa con la signific?

## MODELO SIN INGRESOD
reg2=lm(formula = consumo ~ ingresop
       , data = datos)
stargazer(reg2,type="text")

## MODELO SIN INGRESOP
reg3=lm(formula = consumo ~ ingresod
        , data = datos)
stargazer(reg3,type="text")


stargazer(reg,reg2,reg3,type="text")



#############################################################
##############           PUNTO 7       ####################
#############################################################
rm(list=ls())


consumo=c(6430.9,
          7384.1,
          10402.4,
          6293.4,
          9090.9,
          5106.9,
          9569.3)

ingresop=c(8467.0,
           9113.0,
           14162.0,
           7392.0,
           13067.0,
           6357.0,
           11969.0)

ingresod=c(79.1,
           82.8,
           128.7,
           66.0,
           121.0,
           56.8,
           111.9)

datos=data.frame(consumo,ingresop,ingresod)

library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

## MODELO COMPLETO

reg=lm(formula = consumo ~ ingresod + ingresop 
       , data = datos)
stargazer(reg,type="text")
# Mismo problema que antes pero mas grave... Misma solucion. Buscar en teoria pero elegir la variable correspondiente.




