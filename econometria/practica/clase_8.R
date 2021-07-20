# montecarlo 

#problema 11
rm(list=ls())
set.seed(21212)
obs=400
beta0=1
beta1=5

x=runif(n=obs,min=0,max=10)

hist(x)

y=beta0+beta1*x+rnorm(obs,mean=0,sd=2)


reg=lm(y~x)
summary(reg)

confint(object = reg,param="x",level=0.95)


#Monte Carlo

number_MCrepetitions<-1000
vector_MCBeta<-c()

for (i in 1:number_MCrepetitions){
  
  obs=4000
  beta0=1
  beta1=5
  
  x=runif(n=obs,min=0,max=10)
  

  y=beta0+beta1*x+rnorm(obs,mean=0,sd=2)
  reg=lm(y~x)
  
  beta_estimated=reg$coefficients[2]
  vector_MCBeta[i]=beta_estimated
}

hist(vector_MCBeta)



#Practica 6 punto 1


#problema 2

library(wooldridge)
wage2=data.frame(wage2)
#modelo log-lin
reg1=lm(log(wage)~age,data=wage2)

summary(reg1)

reg2=lm (log(wage)~age+factor(black),data=wage2)
summary(reg2)

#Cuando black toma el valor 0  por cada unidad que aumenta age el salario sube un 2%
#Cuando black tomo el valor 1 por cada unidad que aumenta age el salario subte un 2%
# pero cobran un 28% menos los de tes oscura que los que no son de tes oscura. 




#Regresion concurrente
wage2$black_age=wage2$black*wage2$age

reg3=lm(log(wage)~age+black_age,data=wage2)
summary(reg3)




