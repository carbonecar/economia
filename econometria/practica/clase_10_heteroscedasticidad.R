#############################################################
##############           PUNTO 1         ####################
#############################################################
rm(list=ls())

#En presencia de heteroscedasticidad, los estimadores de MCO son sesgados e ineficientes:
# Falso: Son insesgados pero ineficientes (varianza de los beta mal estimada)

#b)	Si los residuos estimados mediante una regresión por MCO son constantes,
#significa que hay heteroscedasticidad en los datos.
# Falso: Si los errores son constantes es porque es homoscedastico.

#############################################################
##############           PUNTO 2         ####################
#############################################################
rm(list=ls())
library(wooldridge)


data(gpa3, package='wooldridge')

# Cargo los paquetes
library(lmtest)
library(car)

# Estimo el modelo: cumgpa= B0+B1*sat+B2*hsperc+B3*tothrs
reg <- lm(cumgpa~sat+hsperc+tothrs, data=gpa3)
summary(reg)

# Uso la matriz de varianzas y covarianzas por MCO:
coeftest(reg)
# Uso la matriz de varianzas y covarianzas corregida por White (Errores Robustos):
coeftest(reg, vcov=hccm)


#############################################################
##############           PUNTO 3        ####################
#############################################################
rm(list=ls())

data(hprice1, package='wooldridge')

# Estimamos el modelo:
reg <- lm(price~lotsize+sqrft, data=hprice1)
summary(reg)
# Metodo grafico
plot(hprice1$lotsize,(reg$residuals)^2)
plot(hprice1$sqrft,(reg$residuals)^2)
plot(hprice1$price,(reg$residuals)^2)

# Breuch-Pagan test
# Ho: Homoscedastico
# H1: Heteroscedastico
bptest(reg)
library(lmtest)

# White test
# Ho: Homoscedastico
# H1: Heteroscedastico
bptest(reg, ~ fitted(reg) + I(fitted(reg)^2) )


# Uso la matriz de varianzas y covarianzas por MCO:
coeftest(reg)
# Uso la matriz de varianzas y covarianzas corregida por White (Errores Robustos):
coeftest(reg, vcov=hccm)


#############################################################
##############           PUNTO 4        ####################
#############################################################
rm(list=ls())

y=c(5,
    2,
    0,
    1,
    3)

x=c(3,
    2,
    1,
    1,
    2)

datos=data.frame(y,x)


##### A
reg <- lm(y~x, data=datos)
summary(reg)

##### B
# Sin embargo alguien me dice la forma funcional de la varianza. 
# No hace falta hacer test ya que sigma cuadrado depende de i.
# Sigma^2_i= Sigma^2*X_i
#y=Bo+B1x+u
# DIVIDO TODO POR RAIZ DE X_i

#y/sqrt(x)=Bo*1/sqrt(x)+B1x/sqrt(x)+u/sqrt(x)

datos$y2=y/sqrt(x)
datos$unosobrerx=1/sqrt(x)
datos$xnueva=x/sqrt(x)

reg2 <- lm(y2~0+unosobrerx+xnueva, data=datos)
summary(reg2)


##### C
# Sin embargo alguien me dice la forma funcional de la varianza. 
# No hace falta hacer test ya que sigma cuadrado depende de i.
# Sigma^2_i= Sigma^2*X_i^2
#y=Bo+B1x+u
# DIVIDO TODO POR RAIZ DE X_i^2: Es decir divido todo por x (se cancela cuadrado con raiz)

#y/x=Bo*1/x+B1x/x+u/x
#y/x=Bo*1/x+B1+u/x

datos$y3=y/x
datos$unosobrex=1/x

reg3 <- lm(y3~unosobrex, data=datos)
summary(reg3)


#############################################################
##############           PUNTO 5         ####################
#############################################################
rm(list=ls())

### BETA
b0=10
b1=12
b2=-5


######################## MCO
### Desvio estandar de MCO
eeMCO_B0=12
eeMCO_B1=0.4
eeMCO_B2=6

### T para cada caso
b0/eeMCO_B0 #al ser menor que 2.1, no es significativo
b1/eeMCO_B1 #al ser mayor que 2.1, es significativo
b2/eeMCO_B2 #al ser mayor que 2.1, es significativo

######################## WHITE
### Desvio estandar de White
eeW_B0=12
eeW_B1=0.6
eeW_B2=2.5

### T para cada caso
b0/eeW_B0 #al ser menor que 2.1, no es significativo
b1/eeW_B1 #al ser mayor que 2.1, es significativo
b2/eeW_B2 #al ser menor que 2.1, no es significativo




