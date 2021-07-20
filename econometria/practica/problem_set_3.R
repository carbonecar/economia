################################################
########              P1               #########
#################################################
rm(list=ls())


x<-rnorm(1000,1,10)
u<-rnorm(1000,0,1)

plot(x,y)

lm_model<-lm(y~x)
summary(lm_model)

beta2<-cov(y,x)/var(x)
beta1<-mean(y)-beta2*mean(x)

beta1
beta2

#Regresion lineal simple sin constante
lm_mode_sin_constante<-lm(y~0+x)
summary(lm_mode_sin_constante)


#regresión lineal simple sin constante de forma manual a través de la fórmula de MCO.
beta2_sin_constante<-sum(x*y)/sum(x^2)
beta2_sin_constante

plot(lm_model)
plot(lm_mode_sin_constante)

plot(x,y)

abline(lm_model,col="red")
abline(lm_mode_sin_constante,col="blue",lwd=3)


################################################
########              P2               #########
################################################

rm(list=ls())

x<-rnorm(10000,10,5)
u<-rnorm(10000,0,2)
y<-1+3*x+u


lm_model<-lm(y~x)
summary(lm_model)

#diagrama de dispersion y la regresion lineal
plot(x,y)
abline(lm_model,col="red",lwd=3)

#diagrama de dispersion y los residuos
plot(x,lm_model$residuals)
mean(lm_model$residuals) #Esto es muy cercano a cero. Se relaciona con el supuesto Gauss-Markov donde E(y,x)=0


################################################
########              P2               #########
################################################










interpretame<-function(mylm_model,xString,yString){
  mylm_model$coefficients
  
  coeffs<-summary(lm_model)$coefficients
  
  interceptor<-coeffs[1,1]
  slope<-coeffs[2,1]
  cat(sprintf("Por cada unidad que se incrementa %s, %s varia en  %.2f unidades en promedio\n",xString,yString,slope))
  
  cat(sprintf("Cuando %s varia en una unidad %s varia %.2f unidades en promedio",xString,yString,slope))
}

interpretame(lm_model,"X","Y")
    