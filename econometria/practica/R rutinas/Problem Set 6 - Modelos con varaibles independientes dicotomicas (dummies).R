
###### ESTOS SON EJEMPLOS EXTRAS....


#############################################################
##############          Modelos con dummies        ##########
#############################################################
rm(list=ls())

y = c(10,18,34,41,48,60)
x = c(1,2,3,4,5,6)
d= c(1,1,1,2,2,4)


plot(x,y)

reg=lm(y ~ x +factor(d))
summary(reg)

#############################################################
##############          Modelos con dummies  2       ##########
#############################################################
rm(list=ls())

library(wooldridge)
wage2=data.frame(wage2)

table(wage2$black)

# NOTAR QUE COMO BLACK ES UYNA VARIABLE 0 y 1, es lo mismo tratarla como factor como si no...

########### REGRESION COINCIDENTE
# LnSal= b1+ b2*age 
reg1=lm(log(wage) ~ age , data = wage2)
summary(reg1)

########### REGRESION PARALELA
# LnSal= b1+ b2*age + D1*COLOR
reg2=lm(log(wage) ~ age + factor(black), data = wage2)
summary(reg2)
# Para black=0 = 6.126 + 0.020*AGE
# Para black=1 = 6.126-0.28 + 0.020*AGE= 5.846 - 0.020*AGE


########### REGRESION CONCURRENTES
# CREO VARIABLE QUE MULTIPLICA EDAD POR BLACK
wage2$black_age=wage2$black*wage2$age

# LnSal= b1+ b2*age + D1*COLOR*age
reg3=lm(log(wage) ~ age + black_age, data = wage2)
summary(reg3)
# Para black=0 = 6.08 + 0.021966*AGE
# Para black=1 = 6.08 + 0.021966*AGE - 0.008756*AGE  = 6.08 + 0.01321*AGE

########### REGRESION DISIMBOLICAS

# LnSal= b1+ b2*age + D1*COLOR*age + D2*Black
reg4=lm(log(wage) ~ age + black + black_age, data = wage2)
summary(reg4)
# Para black=0 = 6.054755 + 0.022997*AGE
# Para black=1 = 6.054755 + 0.259970 + 0.022997*AGE -0.016602*AGE= 6.314725 + 0.006395*AGE=


library(stargazer)
stargazer(reg1,reg2,reg3,reg4, type="text")



# EXTRA: EDAD AL CUADRADO
# LnSal= b1+ b2*age + b3*age^2
reg=lm(log(wage) ~ age + I(age^2), data = wage2)
summary(reg)

plot(wage2$age,reg$fitted.values)


#############################################################
##############          Modelos con dummies         #########
#############################################################
rm(list=ls())
#install.packages("eph")
library(eph)

eph=data.frame(get_microdata(year = 2018, trimester = 2, wave = NA,
                             type = "individual", vars = "all"))


eph=subset(eph, eph$P21>0 & eph$P21<50000 & eph$ESTADO==1)


library(ggplot2)
eph$REGION <- factor(eph$REGION)

p <- ggplot(eph, aes(x=eph$REGION, y=eph$P21)) + 
  geom_violin()
p

table(eph$REGION)
# VER CODIGOS EN EPH pero 1 es GBA, 40 es NOA, 41 es NEA, 42 cuyo, 43 es pamepana y 44 es patagonia.
eph$REGION <- factor(plyr::mapvalues(x=eph$REGION,
                                         from=c(1,40,41,42,43,44),
                                         to=c('GBA','NOA','NEA','Cuyo','Pampeana','Patagonia')))
table(eph$REGION)

hist(eph$P21)
summary(eph$P21)

regln=lm(log(P21) ~  factor(REGION)  , data = eph)
summary(regln)

reglin=lm(P21 ~  factor(REGION) , data = eph)
summary(reglin)


library(stargazer)
stargazer(regln,reglin, type="text")