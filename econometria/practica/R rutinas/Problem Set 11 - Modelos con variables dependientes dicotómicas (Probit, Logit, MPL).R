

## PRACTICA EXTRAAAAAA::

#############################################################
######### Modelos de Probabilidad Lineal (MPL) ##############
#############################################################
rm(list=ls())

#install.packages("car")

library(car)  
data(mroz, package='wooldridge')


# Estimamos por MPL si esta o no en la fuerza laboral

linprob <- lm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,data=mroz)
summary(linprob)
summary(linprob$fitted.values)

hist(linprob$fitted.values)
plot(mroz$age,linprob$fitted.values)
plot(mroz$age,mroz$inlf)
abline(linprob) 

plot(mroz$hours,mroz$inlf)
abline(linprob) 

#############################################################
########################## LOGIT ############################
#############################################################

logitres<-glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,
              family=binomial(link=logit),data=mroz)
# Summary of results:
summary(logitres)
# Log likelihood value:
logLik(logitres) 
# McFadden's pseudo R2:
1 - logitres$deviance/logitres$null.deviance

#############################################################
########################## PROBIT ###########################
#############################################################

# Estimate probit model
probitres<-glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,
               family=binomial(link=probit),data=mroz)
# Summary of results:
summary(probitres)

# Log likelihood value:
logLik(probitres) 
# McFadden's pseudo R2:
1 - probitres$deviance/probitres$null.deviance

#############################################################
########################## MARGINAL EFFECTS #################
#############################################################

# EFECTOS MARGINALES
#install.packages("margins")
library("margins")
library("mfx")

regprob=probitmfx(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6, data = mroz, atmean = TRUE)
reglog=logitmfx(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6, data = mroz, atmean = TRUE)

summary(linprob)
reglog
regprob

# GRAFICOS EFECTOS MARGINALES
MPL=margins(linprob)
probit=margins(probitres)
logit=margins(logitres)


plot(probit)
plot(logit)
plot(MPL)



#############################################################
################# A MANO MARGINAL EFFECTS   #################
#############################################################

xb.log <- predict(logitres)
xb.prob<- predict(probitres)
# APE factors = average(g(xb))
factor.log <- mean( dlogis(xb.log) )
factor.prob<- mean( dnorm(xb.prob) )
cbind(factor.log,factor.prob)

# average partial effects = beta*factor:
APE.lin <- coef(linprob) * 1
APE.log <- coef(logitres) * factor.log
APE.prob<- coef(probitres) * factor.prob

# Table of APEs
cbind(APE.lin, APE.log, APE.prob)


#############################################################
######### EJERCICIO 1                         ##############
#############################################################
rm(list=ls())


# VER TEORICO

#############################################################
######### EJERCICIO 2                          ##############
#############################################################


# VER TEORICO

#############################################################
######### EJERCICIO 3                         ##############
#############################################################
rm(list=ls())
mydata <- read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")

## convert rank to a factor (categorical variable)
mydata$rank <- factor(mydata$rank)

head(mydata)

mympl=lm(admit ~ gre + gpa + rank, 
         data = mydata)
summary(mympl)
hist(mympl$fitted.values)


myprobit <- glm(admit ~ gre + gpa + rank, family = binomial(link = "probit"), 
                data = mydata)
summary(myprobit)
hist(myprobit$fitted.values)


mylogit <- glm(admit ~ gre + gpa + rank, family = binomial(link = "logit"), 
               data = mydata)
summary(mylogit)
hist(mylogit$fitted.values)



library("margins")

MPL=margins(mympl)
probit=margins(myprobit)
logit=margins(mylogit)
plot(probit)
plot(logit)
plot(MPL)


#############################################################
######### EJERCICIO 4                        ##############
#############################################################
rm(list=ls())

aprob=c(1,1,0,0)
nota= c(10,8,4,2)

datos=data.frame(aprob,nota)
MPL <- lm(aprob~nota,data=datos)

summary(MPL)

list(MPL$fitted.values)

