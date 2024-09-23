#############################################################
##############           PUNTO 1         ####################
#############################################################
rm(list=ls())

#Consumo=B1*lnaIngreso
#Ln Ingreso	= 328.5 
#Pvalue:	0.02

# Modelo Lin-Log
# Al aumentar 1% el ingreso, el consumo subo 3.28 pesos. 
# El ingreso es significativo al 10% y 5%. No lo es al 1%.

#############################################################
##############           PUNTO 2         ####################
#############################################################
rm(list=ls())

#LnQ=B_0+B_1*lnaPrecio
#Ln Precio= -1..22 	
#Pvalue:0.001 (0.1%)

# Modelo Log-Log
# Al aumentar 1% el precio, las cantidades caen un 1.22%. 
# El precio es significativo al 10%, 5% y 1%

#############################################################
##############           PUNTO 3         ####################
#############################################################
rm(list=ls())

Beta=0.7
ds_Beta=0.2

tabla=2.1

# Intervalo Beta+-ds*t
Beta-ds_Beta*tabla
Beta+ds_Beta*tabla

# Diferente de 0.6?
t=(Beta-0.6)/ds_Beta
t
0.5>tabla
# No es estadisticamente distinto de 0.6

# Diferente de cero?
t=(Beta-0)/ds_Beta
t
3.5>tabla
# Es significativo

#############################################################
##############           PUNTO 4         ####################
#############################################################
rm(list=ls())

Beta=2.1
ds_Beta=0.5

tabla=2.1

# Intervalo Beta+-ds*t
Beta-ds_Beta*tabla
Beta+ds_Beta*tabla

# Diferente de 2?
t=(Beta-2)/ds_Beta
t
0.2>tabla
# No es estadisticamente distinto de 2

# Diferente de cero?
t=(Beta-0)/ds_Beta
t
4.2>tabla
# Es significativo

#############################################################
##############           PUNTO 5         ####################
#############################################################
rm(list=ls())

#Consumo=B1*lnaIngreso
#Ln Ingreso	= 120.1 
#Pvalue:	0.002

# Modelo Lin-Log
# Al aumentar 1% el ingreso, el consumo subo 1.2 pesos. 
# El ingreso es significativo al 10%, 5% y 1%


#############################################################
##############           PUNTO 6         ####################
#############################################################
rm(list=ls())

#Ln Ingreso=B1*educ
#educ	= 0.0098 
#t= 2.1
#t de tabla= 2.05

# Modelo Log-Lin
# Al aumentar un año de educacion, el ingreso subo 0.98%. 
# Como t>t de tabla: 2.1>2.05 entonces es significativo.

#############################################################
##############           PUNTO 7         ####################
#############################################################
rm(list=ls())

consumo=c(31,
          42,
          44,
          57)

salario=c(31,
          42,
          53,
          58)

datos=data.frame(consumo,salario)

plot(datos$salario,datos$consumo)

# MODELO SIN ORDENADA AL ORIGEN.. GUARDA!!!!
reg=lm(consumo ~ 0+ salario 
       , data = datos)
summary(reg)

# Varianza del modelo: SumE^2/N-K
varianza=sum((reg$residuals)^2)/(5-2)
sqrt(varianza)

# El R cuadrado 0.9937.

datos$lconsumo=log(datos$consumo)
datos$lsalario=log(datos$salario)


# LOG LOG
log_log=lm(lconsumo ~ 0+ lsalario 
       , data = datos)
summary(log_log)
#Al aumentar 1% el salario, el consumo sube 0.98%.

# LIN LOG
lin_log=lm(consumo ~ 0+ lsalario 
           , data = datos)
summary(lin_log)
#Al aumentar 1% el salario, el consumo sube 11.54/100=0.1154 unidades

# LOGG LIN
lin_log=lm(lconsumo ~ 0+ salario 
           , data = datos)
summary(lin_log)
#Al aumentar 1 unidad el salario, el consumo sube 7.8%.


#############################################################
##############           PUNTO 8         ####################
#############################################################
rm(list=ls())

x=runif(1000,5,300)
y=3*x+1.1+rnorm(1000,0,25)

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
##############           PUNTO 9        ####################
#############################################################
rm(list=ls())

B=2.1
Alfa=0.5

k=runif(5000,0,200)
l=runif(5000,0,2000)
Y=B*l^(Alfa)*k^(1-Alfa)+rnorm(5000,0,1)

library(plot3D)
# GRAFICO Y, X, Z
scatter3D( k, l, Y,xlab = "l",
           ylab ="k", zlab = "Y")
reg=lm(Y ~ k +l )
summary(reg)

lnY=log(Y)
lnl=log(l)
lnk=log(k)

reg=lm(lnY ~ lnk +lnl)
summary(reg)
reg2=c(exp(reg$coefficients[1]),round(reg$coefficients[2],3),round(reg$coefficients[3],3))
reg2


#probar con Y=B*l^(Alfa)*k^(1-Alfa)+rnorm(5000,0,1)


#############################################################
##############           PUNTO 10         ###################
#############################################################
rm(list=ls())

x=runif(1000,1,50)
Y=3+2*1/x +rnorm(1000,0,0.1)


plot(x,Y)

#regresion I
reg=lm(Y ~ x)
summary(reg)
abline(reg , col="red")


#regresion II
xdiv=1/x
reg2=lm(Y ~ xdiv)
summary(reg2)
points(x,reg2$fitted.values , col="blue")
