#############################################################
##############           PUNTO 1         ####################
#############################################################
rm(list=ls())

set.seed(11)

x1=runif(1000,5,300)
x2=runif(1000,0,5)


y=4.5+2*x1+5*x2+rnorm(1000,0,1)

datos=data.frame(y,x1,x2)



##### SOLO CONTRA x1
reg=lm(y ~ x1, data=datos)
summary(reg)
# Ho: No hay evidencia empirica de un modelo mal especificado
# H1: Modelo mal especificado
library(lmtest)
resettest(reg)

##### CONTRA X1 + X2
reg=lm(y ~ x1+x2, data=datos)
summary(reg)

# Ho: No hay evidencia empirica de un modelo mal especificado
# H1: Modelo mal especificado
library(lmtest)
resettest(reg)


#############################################################
##############           PUNTO 2         ####################
#############################################################
rm(list=ls())

set.seed(11)

x=runif(1000,5,300)


y=4.5+2*x-3*x^2+rnorm(1000,0,10000)

datos=data.frame(y,x)

plot(datos$x,datos$y)

##### SOLO CONTRA x
reg=lm(y ~ x, data=datos)
summary(reg)
# Ho: No hay evidencia empirica de un modelo mal especificado
# H1: Modelo mal especificado
library(lmtest)
resettest(reg)
points(datos$x,reg$fitted.values, col="red", type="l")
# ¿QUE PASO?

##### Correcto
reg=lm(y ~ x+I(x^2), data=datos)
summary(reg)

# Ho: No hay evidencia empirica de un modelo mal especificado
# H1: Modelo mal especificado
library(lmtest)
resettest(reg)

plot(datos$x,datos$y)
points(datos$x,reg$fitted.values, col="blue")

#############################################################
##############           PUNTO 3         ####################
#############################################################
rm(list=ls())

data(hprice1, package='wooldridge')

# Con Comando
orig <- lm(price ~ lotsize+sqrft+bdrms, data=hprice1)

# RESET test
library(lmtest)
resettest(orig)


# Manual
data(hprice1, package='wooldridge')

# original linear regression
orig <- lm(price ~ lotsize+sqrft+bdrms, data=hprice1)

# regression for RESET test
RESETreg <- lm(price ~ lotsize+sqrft+bdrms+I(fitted(orig)^2)+ 
                 I(fitted(orig)^3), data=hprice1)
RESETreg

# RESET test. H0: all coeffs including "fitted" are=0 
library(car)
linearHypothesis(RESETreg, matchCoefs(RESETreg,"fitted"))

#############################################################
##############           PUNTO 4         ####################
#############################################################
rm(list=ls())

educ=runif(100,7,15)
habilidad=(educ/15+rnorm(100,0,0.1))

cor(educ,habilidad)

salario=5000+125*educ+330*habilidad+rnorm(100,0,3)


# MOdelo original
original <- lm(salario ~ educ+habilidad)
summary(original)

# ¿Como mido habilidad?. Corro solo contra educ. 
# el error incluye habilidad.. ¿COV(U,X)=0?
estimado <- lm(salario ~ educ)
summary(estimado)

plot(educ, salario)
# COMENTARIO: VER MACHINE LEARNINGG


#http://www.urfie.net/downloads09.html

#############################################################
##############           PUNTO 5         ####################
#############################################################
rm(list=ls())
data(rdchem, package='wooldridge')


salario=c(4014,
          4026,
          4008,
          5000,
          4012,
          4015,
          4012)

educ=c(7,
       8,
       9,
       10,
       5,
       6,
       9)

plot(educ,salario)

# Regresion
reg <- lm(salario~educ)
summary(reg)

hist(reg$residuals, freq=FALSE)
lines(density(reg$residuals))


# TEST OUTLIER

# Genero residuo
residuo=reg$residuals

#genero sigma cuadrado: SUM E^2/N-K 
sigma_cuadrado=sum(reg$residuals^2)/5
#genero sigma: Raiz de sigma cuadrado
sigma=sqrt(sigma_cuadrado)


#Genero estadistico de outlier:
estadistico=abs(residuo/sigma)

list(estadistico)
plot(estadistico, type="l")


# Ho: No hay outliers
# H1: Hay outliers
outlierTest(reg)