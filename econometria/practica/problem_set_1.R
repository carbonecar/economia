puntoC_datosPosicion<-function(value){
  cat(sprintf("mean: %.6f\n",mean(value)))
  cat(sprintf("sd: %.6f\n",sd(value)))
  cat(sprintf("var: %.6f\n",var(value)))
}

X<-rnorm(1000,0,1)
Y<-rnorm(1000,10,5)
Z<-runif(1000,0,20)
D<-seq(1,1000,1)


hist(X)
hist(Y)
hist(Z)
hist(D)

puntoC_datosPosicion(X)
puntoC_datosPosicion(Y)
puntoC_datosPosicion(Z)
puntoC_datosPosicion(D)


######################################
##########Plot a function ############
######################################

dfunction<-function(x){
  x^2+3*x-2
}

efunction<-function(x){
  x^3-3*x+1
}

curve(dfunction, from=1, to=50, , xlab="x", ylab="y")
curve(efunction, from=1, to=50, , xlab="x", ylab="y")
curve(3*x+2, from=1, to=50, , xlab="x", ylab="y")


######################################
##########   Punto 2      ############
######################################

a <- c(1,2,3,4,5,6,7,8,9,10)

#Matriz a partir del vector divida por columna
A=matrix(data=a,5,2)

#Idem anterior pero dvidiendo por columna
B=matrix(data=a,5,2,byrow = TRUE)

#c) Crea una matriz llamada C con 2 filas y 5 columnas. (a partir de los datos de a)
#ordenando los valores por fila.
library(matlib)

C=matrix(data=a,2,5,byrow = TRUE)

t(C)
B%*%C

D<-C%*%B
det(D)
D%*%(D^-1)

#Inversa de D
DI<-solve(D)

DI%*%D


######################################
##########   Punto 3      ############
######################################
R<-seq(from =2,to=12,by=2)

M<-matrix(data=R,nrow=3,ncol=2,byrow = FALSE)
MT<-t(M)
######################################
##########   Punto 4      ############
######################################

y <- c(1,2,3,-1,0,-1,2,1,2)
x<- c(0,1,2,-2,1,-2,0,-1,1)

hist(x)
hist(y)

# cuartiles
summary(x)
summary(y)
#quintiles
quantile(x,0.20)
quantile(x,0.60)

quantile(y,0.20)
quantile(y,0.60)

#Plot o nube de puntos
plot(x,y)
#Box plot. La caja esta entre el cuartil 1 y el cuartil 3. La linea gruesa 
# es la medianta y las puntas son los maximos y minimos
boxplot(x,y)



######################################
##########   Punto 5      ############
######################################

nota<-c(7,9,6,10,4)
nombre<-c("Pedro","Manuel","Aldana","Lucia","Fabian")

data<-data.frame(nombre,nota,aprobado=nota>7)
View(data)

table(data$aprobado)


######################################
##########   Punto 6      ############
######################################

distribucionValoresEnteros=sample(1:10,100,replace=TRUE)

hist(distribucionValoresEnteros)

distribucionValoresEnteros=data.frame(distribucionValoresEnteros)

library(ggplot2)
g<-ggplot(distribucionValoresEnteros,aes(distribucionValoresEnteros)) +
  geom_bar(fill="steelblue")
g


######################################
##########   Punto 7      ############
######################################

alpha <- c(2,4,5,2,4,5,10,2,10)
clasif=ifelse(alpha>3,"Mayor a 3","Menor a Tres")
dato=data.frame(alpha,clasif)
table(dato)

######################################
##########   Punto 8      ############
######################################

x<-rnorm(400,0,1)
hist(x)
summary(x)
var(x)
