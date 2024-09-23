#############################################################
##############           PUNTO 1         ####################
#############################################################

rm(list=ls())

set.seed(123)
y=rnorm(10000,1,2)
x=rnorm(10000,0,10)
z=rnorm(10000,3,10)

datos=data.frame(y,x,z)

var(datos)
cor(datos)

plot(y,x)
plot(y,z)

pairs(datos)
#############################################################
##############           PUNTO 2         ####################
#############################################################

rm(list=ls())
M=rnorm(2500,10,5)
mean(M)
sd(M)
var(M)


#BAJO NORMAL

n <- length(M)   # El tamaño válido de la muestra
media <- mean(M) # la media 
desv <- sd(M)  # La desviación estándar. Datos históricos
nivelconfianza = 0.95

qnorm=qnorm(1-nivelconfianza,0,1)
error.est <- desv/sqrt(n) # Calculamos el error estándar
margen.error <- qnorm * error.est # nivel de confianza de 90% 

lim.inf <- media - margen.error # Límite inferior del intervalo
lim.sup <- media + margen.error # Límite superior del intervalo
lim.sup
lim.inf


#BAJO T
qnorm=qt(1-nivelconfianza,length(M))
error.est <- desv/sqrt(n) # Calculamos el error estándar
margen.error <- qnorm * error.est # nivel de confianza de 90% 

lim.inf <- media - margen.error # Límite inferior del intervalo
lim.sup <- media + margen.error # Límite superior del intervalo
lim.sup
lim.inf

hist(M)

#############################################################
##############           PUNTO 3         ####################
#############################################################

rm(list=ls())
#install.packages("eph")
library(eph)

eph=data.frame(get_microdata(year = 2018, trimester = 2, wave = NA,
              type = "individual", vars = "all"))


eph=subset(eph, eph$P21>0 & eph$P21<50000 )

library(ggplot2)

eph$REGION <- as.factor(eph$REGION)

# Basic violin plot
p <- ggplot(eph, aes(x=eph$REGION, y=eph$P21)) + 
  geom_violin()
p
# Rotate the violin plot
p + coord_flip()




ggplot(data = eph,
       mapping = aes(x = eph$P21,
                     fill = factor(eph$CH04))) +
  geom_histogram(bins = 9,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Distribución del salario',
       fill = 'Hombre vs Mujer',
       x = 'Salario',
       y = 'conteos',
       caption = 'Elaborado por alumnos de Econometria')




#Media comando
mean(eph$P21)

#Media a mano
sum(eph$P21)/length(eph$P21)


#varianza comando
var(eph$P21)

#varianza a mano sesgada
(sum((eph$P21-mean(eph$P21))^2))/length(eph$P21)
#varianza a mano insesgada
(sum((eph$P21-mean(eph$P21))^2))/(length(eph$P21)-1)


#############################################################
##############           PUNTO 4         ####################
#############################################################
rm(list=ls())


A <- diag(1:6)
B <- matrix(rep(1:6,6),ncol=6)
b <- diag(B)


t(b) %*%(A+B)
t(b) %*%(A %*%B)
A - 1*B

#############################################################
##############           PUNTO 5         ####################
#############################################################
rm(list=ls())

dat <- c(7.3, 6.8, 0.005, 9, 12, 2.4, 18.9, .9)
mean(dat)
sqrt(dat)
dat[dat>sqrt(dat)]
sum(dat>1)
sqrt(datr<-round(dat,2))
dat-datr

#############################################################
##############           PUNTO 6         ####################
#############################################################
rm(list=ls())

sal1=rchisq(10000,5)
sal2=rchisq(10000,150)


data=data.frame(sal1,sal2)
boxplot(data$sal1,data$sal2)
hist(data$sal1)
hist(data$sal2)

#############################################################
##############           PUNTO 7         ####################
#############################################################
rm(list=ls())

x=rnorm(100000,10,2)
mean(x)
var(x)

y=rnorm(100000,-2, 3 )
mean(y)
var(y)

#E[X+Y]=E[X]+E[Y]=10-2=8
xy=x+y
mean(xy)

#E[2X-3Y]=2E[X]-3E[Y]=2*10-3*-2=20+6=26
xy=2*x-3*y
mean(xy)

#V[X+Y]=V[X]+V[Y]=4+9=13
xy=x+y
var(xy)

#V[3X+2Y]=V[3X]+V[2Y]=9V[X]+4V[Y]=9*4+4*9=72
xy=3*x+2*y
var(xy)

#V[3X-2Y]=V[3X]+V[2Y]=9V[X]+4V[Y]=9*4+4*9=72
xy=3*x-2*y
var(xy)

#Diferencia por covarianza
var(3*x)+var(2*y)+cov(3*x,2*y)
xy=3*x+2*y
var(xy)

#############################################################
##############           PUNTO 8         ####################
#############################################################
rm(list=ls())

(dado = sample(1:6, size=1000) )

(dado = sample(1:6, size=1000, replace = TRUE) )


table(dado)
hist(dado)



