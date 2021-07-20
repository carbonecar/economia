######################## problem set 5 ###################


library(readxl)
library(stargazer)
dataset=read_excel("Base de Datos.xlsx",sheet="RegresionMultiple") 

regressor=lm(dataset$Y~.,data=dataset)

summary(regressor)
stargazer(regressor,type="text")

# comparacion contra un estadistico particular

t=(b-0)/sd_b

my_t=0.4471/(0.1040 ) # beta/sd

## si t>t_prueba, entonces RHN y es significativo, 

##### Punto 2

y=c(10,20,30,40,50)
x1=c(1,2,3,4,10)
x2=c(10,8,7,5,0)

datos=data.frame(y,x1,x2)

regressor1<-lm(y~x1,data=datos)
regressor2<-lm(y~ .,data=datos)

summary(regressor2)
summary(regressor1)
stargazer(regressor2,type="text")

t.test(x1,y=NULL,mu=30,conf.level = .05)

#manual



#ejercicio 3

dataset=read_excel("Base de Datos.xlsx",sheet="Mincer") 

regressor<-lm(dataset$salario~ . ,data=dataset)


plot(dataset$edad,dataset$salario)

points(dataset$edad,regressor$fitted.values, col="red")

points(dataset$experiencia,regressor$fitted.values, col="blue")


#cual tiene mas fuerza hay que estandarizar las variables. 

library(eph)



