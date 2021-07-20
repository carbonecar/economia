#############################################################
##############          Modelos con dummies        ##########
#############################################################
rm(list=ls())

y = c(10,18,34,41,48,60)
x = c(1,2,3,4,5,6)
d= c(1,1,1,2,2,4)


plot(x,y)

reg=lm(y ~ x +factor(d))

reg1=lm(y ~x)

abline(reg1,col='blue')
abline(reg,col="red")
summary(reg)
summary(reg1)

#############################################################
##############          ejercicio 2        ##########
#############################################################
rm(list=ls())
library(wooldridge)
library(stargazer)
# 4 regresiones 

# coincidente

reg1<-lm(lwage~age,data=wage2)

# paralela
reg2<-lm(lwage~age+black,data=wage2)

summary(reg2)
# concurrente

reg3<-lm(lwage~age+wage2$black_age,data=wage2)

summary(reg3)

# disimbola

reg4<-lm(lwage~age+wage2$black_age+black,data=wage2)
summary(reg4)
stargazer(reg1,reg2,reg3,reg4,type='text')

#suma de cuadrados residuales. 
deviance(reg4)
sum(resid(reg4)^2)


