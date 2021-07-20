##########################
########P2P1#############3##
##########################

set.seed(123)
Y<-rnorm(10000,1,2)
X<-rnorm(10000,0,10)
Z<-rnorm(10000,3,10)


datos=data.frame(Y,X,Z)

var(datos) # la varianzas estan en la diagonal principal

cor(datos)

plot(Y,X)

plot(Y,Z)
var(X)
var(datos$X)

pairs(datos) ## tods las nubes de puntos


##########################
########P2P2#############
##########################

seq(1:10)

qt(p=.975, df = 8) ##valor de la distribucion t que da .975 de probabilidad 
qnorm(.975,10,5)  # valor X de la distribucion normal que da un probabilidad de P

set.seed(123)
M<-rnorm(30,10,sqrt(25))

summary(M)
var(M)
sd(M)

#asumiendo distribucion t, es decir no conozco la varianza poblacional.
t.test(M,NULL,conf.level = 0.95)

n <- length(M)   # El tamaÒo v·lido de la muestra
media <- mean(M) # la media 
desv <- sd(M)  # La desviaciÛn est·ndar. Datos histÛricos
nivelconfianza = 0.95

qnerror.est <- desv/sqrt(n) # Calculamos el error est·ndar
margen.error <- qnorm * error.est # nivel de confianza de 90% 

lim.inf <- media - margen.error # LÌmite inferior del intervalo
lim.sup <- media + margen.error # LÌmite superior del intervalo
lim.sup
lim.inf


qt(.95,length(M-1))

  
##########################
########P2P4##############
##########################


A<-diag(1:6)
B<-matrix(rep(1:6,6),ncol=6)

b<-diag(B)

t(b)%*%(A-B)

##########################
########P2P5##############
##########################

dat<-c(7.3, 6.8, 0.005, 9, 12, 2.4, 18.9, .9)

mean(dat)

dat_cuad<-sqrt(dat)

dat[dat>1]
dat[dat>sqrt(dat)]
sum(dat>1)  ## cuantos datos son mayores a 1

##########################
########P2P6##############
##########################

sal1=rchisq(10000,5)
sal2=rchisq(10000,150)  ## cuando a los grados de libertad tienen a infinito la chi2 parece una normal

data=data.frame(sal1,sal2)

boxplot(data$sal1,data$sal2)

hist(data$sal1)
hist(data$sal2)


##########################
########P2P7##############
##########################

dado<-sample(1:6,size=1000,replace = TRUE)

hist(dado)

x<-rnom(10000,10,2)
y<-rnom()

ce
