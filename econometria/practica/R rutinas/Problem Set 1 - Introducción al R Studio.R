#############################################################
##############           PUNTO 1         ####################
#############################################################

rm(list=ls())

x= rnorm(1000,0,1)
hist(x)
mean(x)
sd(x)


y= rnorm(1000,10,2)
hist(y)
mean(y)
sd(y)
var(y)


z= runif(1000,0,20)
hist(z)
mean(z)
sd(z)
var(z)
(20-0)^2/12

D=seq(1,1000,1)


curve(x^2+3*x-2,-10,10)
curve(x^3-3*x+1,-10,10)


#############################################################
##############           PUNTO 2         ####################
#############################################################

rm(list=ls())

a <- c(1,2,3,4,5,6,7,8,9,10) 
A <- matrix(a, nrow = 5, ncol = 2)
A

B <- matrix(a, nrow = 5, ncol = 2, byrow = TRUE) 
B

C <- matrix(a, nrow = 2, ncol = 5, byrow = TRUE)
C

t(C)
B%*%C
D <- C%*%B 
D
det(D)
inv=solve(D)
inv
inv%*%D

#############################################################
##############           PUNTO 3         ####################
#############################################################

rm(list=ls())

R <- seq(2,12,2)

M <- matrix(R, nrow = 3, ncol = 2, byrow = TRUE) 
M

Mt <- t(M)
Mt

det(M)
det(Mt)

#############################################################
##############           PUNTO 4         ####################
#############################################################

rm(list=ls())

y <- c(1,2,3,-1,0,-1,2,1,2) 
x<- c(0,1,2,-2,1,-2,0,-1,1)

hist(y)
hist(x)

summary(x)
summary(y)

plot(y,x)
boxplot(y,x)

#############################################################
##############           PUNTO 5         ####################
#############################################################
rm(list=ls())

nota<-c(7,9,6,10,4)
nombre<-c("Pedro","Manuel","Aldana","Lucia","Fabian")
datos=data.frame(nombre, nota, aprobado=nota>7)
table(datos$aprobado)


#############################################################
##############           PUNTO 6         ####################
#############################################################
rm(list=ls())

vector=sample(1:10,100,replace=T)


vector=data.frame(vector)

library(ggplot2)
g <- ggplot(vector, aes(vector)) + 
  geom_bar(fill="steelblue") 
g

hist(vector$vector)


#############################################################
##############           PUNTO 7         ####################
#############################################################
rm(list=ls())


vector=c(1,2,3,4,5)

alpha <- c(2, 4,5,2,4,5,10,1,10)
clasif=ifelse(alpha>3,"Mayor a tres","Menor a tres")
dato=data.frame(alpha,clasif)
dato
table(dato)

#############################################################
##############           PUNTO 8         ####################
#############################################################
rm(list=ls())

x=rnorm(400,0,1)
hist(x)
mean(x)
var(x)

#Puedo guardar los datos en un vector
dato_prom=mean(x)
dato_varianza=var(x)

# Datos principales
summary(x)



