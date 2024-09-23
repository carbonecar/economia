#############################################################
##############           EJERCICIO 1    #############
#############################################################
rm(list=ls())


SRCR<-52000
SRC1<-32000
SRC2<-17000

#Saco K
k<-2
n1n2k <- 300 + 200 - 2*k

# Obtengo F
Fc<-((SRCR- (SRC1+SRC2))*n1n2k)/((SRC1+SRC2)*k)
Fc
Ft<- 17.5

resultado=ifelse(Fc<Ft, {print("No hay evidencia de quiebre estructural")},print("Hay evidencia de quiebre estructural"))
resultado


Ft<- 13.2

resultado=ifelse(Fc<Ft, {print("No hay evidencia de quiebre estructural","TRUE")},print("Hay evidencia de quiebre estructural"))
resultado

#############################################################
##############           EJERCICIO 2     #############
#############################################################
rm(list=ls())

library(wooldridge)
data("wage1")
attach(wage1)

hist(wage)
hist(wage[female==0])
hist(wage[female==1])

modelR <- lm(lwage~educ+exper+tenure )
modeln1 <- lm(lwage~educ+exper+tenure, data=wage1[female==1,] )
modeln2 <- lm(lwage~educ+exper+tenure, data=wage1[female==0,] )

# Resultados
stargazer(modelR, modeln1, modeln2, type="text", title="Results", align=TRUE)

SRCr<-sum(residuals(modelR)^2)
SRCn1<-sum(residuals(modeln1)^2)
SRCn2<-sum(residuals(modeln2)^2)

# H0 b1=b2=b
k<-length(coefficients(modeln1))
n1n2k <- sum(female==1) + sum(female==0) - 2*k

Fc<-((SRCr- (SRCn1+SRCn2))*n1n2k)/((SRCn1+SRCn2)*k)
Ft<-qf(0.10, k, n1n2k, lower.tail=FALSE)
Fc
Ft

Ft<-qf(0.05, k, n1n2k, lower.tail=FALSE)
Ft

Ft<-qf(0.01, k, n1n2k, lower.tail=FALSE)
Ft
