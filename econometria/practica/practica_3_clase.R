require(MASS)

require(ISLR)


summary(Boston)

#lstat tasa de probrea


plot(Boston$lstat,Boston$medv)

linearMod <- lm(medv ~ lstat, data=Boston)
summary(linearMod)


# Al aumentar la tasa de probreza en una unidad el precio  PROMEDIO de la vivienda cae 0.95 unidades
abline(linearMod,col="red",lwd=3)

plot(Boston$lstat,linearMod$residuals)

mean(linearMod$residuals)


# EJERCICIO 6
# Forma funcional de in


#    c(I)= c=b0+b1*I+u

require(car)

library(readxl)
read_excel()

library(stargazer)

summary


