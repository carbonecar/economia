#############################################################
##############           EJERCICIO 1   ############
#############################################################
rm(list=ls())

n=200
S=0
K=3

JB= n*((S^2)/6+ ((K-3)^2)/24)
JB
#############################################################
##############           EJERCICIO 2   ############
#############################################################
rm(list=ls())


n=200
S=1
K=3.2

JB= n*((S^2)/6+ ((K-3)^2)/24)
JB



#############################################################
##############           EJERCICIO 3   ############
#############################################################
rm(list=ls())

library(wooldridge)
data("wage1")
attach(wage1)
library(normtest)


reg1 <- lm(lwage~1+educ+exper+tenure )
summary(reg1)

hist(reg1$residuals)
# Veo prueba A^2 
ad.test(reg1$residuals)
# Veo JB
jb.norm.test(reg1$residuals)

library(ggplot2)

ggplot( data=reg1, aes(x = reg1$residuals)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "#0C3D7D9F", args = list(mean = mean(reg1$residuals), sd = sd(reg1$residuals)))