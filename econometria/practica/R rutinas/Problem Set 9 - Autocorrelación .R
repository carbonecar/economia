#############################################################
##############           PUNTO 1         ####################
#############################################################
rm(list=ls())
t=c(1,
    2,
    3,
    4,
    5)

y=c(5,
    2,
    0,
    1,
    3)

x=c(3,
    2,
    -1,
    -2,
    0)

datos=data.frame(t,y,x)
datos

plot(datos$x,datos$y)

### REGRESION MCO
reg <- lm(y~x, data=datos)
summary(reg)

### TEST DE Durbin-Watson
# H0: No hay autocorrelacion
# H1: Hay autocorrelacion
library(lmtest)
dwtest(reg)
# En este caso no hay autocorrelacion. Dejamos el modelo como esta.

#############################################################
##############           PUNTO 2        ####################
#############################################################
rm(list=ls())
library(readxl)
datos <- read_excel("/Users/federicofavata/Dropbox/Docencia/UNSAM/Econometria I/Base de datos/Base de Datos.xlsx", 
                   sheet = "autocorrelacion")

datos

plot(datos$x,datos$y)

### REGRESION MCO
reg <- lm(y~x, data=datos)
summary(reg)

### TEST DE Durbin-Watson
# H0: No hay autocorrelacion
# H1: Hay autocorrelacion
library(lmtest)
dwtest(reg)
# En este caso hay autocorrelacion. Solucion: Como no conozco RHO (p) uso matriz de varianzas y cov estimadas correctamente

# Uso la matriz de varianzas y covarianzas por MCO:
coeftest(reg)
# Uso la matriz de varianzas y covarianzas corregida por White (Errores Robustos):
library(car)
coeftest(reg, vcov=hccm)

#############################################################
##############           PUNTO 3        ####################
#############################################################
rm(list=ls())

############# A

#Valores criticos DW= 0.894, 1.828, 2.172 y 3.106. 
# DW=2(1-p)
# DW/2 = 1-p
# 1 - DW/2   = p

# Con DW= 0.894
1 -0.894/2 

# Con DW= 1.828
1 -1.828/2 

# Con DW= 2.172
1 -2.172/2 

# Con DW= 3.106
1 -3.106/2 


############# B

#Valores criticos DW=1.728, 1.81, 2.19 y 2.272
# DW=2(1-p)
# DW/2 = 1-p
# 1 - DW/2   = p

# Con DW= 1.728
1 -1.728/2 

# Con DW= 1.81
1 -1.81/2 

# Con DW= 2.19
1 -2.19/2 

# Con DW= 2.272
1 -2.272/2 
