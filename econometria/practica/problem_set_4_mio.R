#################### P4 P7#######################


rm(list=ls())

consumo=c(31,42,44,57)
salario=c(31,42,53,58)

datos_consumo<-data.frame(consumo,salario)


#Estimadore sdel modelo son b0 y b1
#Modelo Consumoi = B1 salarioi + ui
lm_consumo=lm(consumo~salario,data=datos_consumo)

summary(lm_consumo)
# b0=5.4493, b1=0.8272 
# Interpretacion para cada incremento de una unidad en el salario el consumo se incrementa en 0.8272 unidades
#  para la pendientes es p-value: 0.06681 por lo tanto esto es 6.68% p<10% es significativo al 10% pero no al 5% ni al 1%
#  para el interceptor p-value es 0.6590 = 65.90% esto es un valor grande p-value < alpha es la condicion de RECHAZO esto seria No rechazo
#  por lo tanto no es significativo

#Regresion sin constante
lm_consumo=lm(consumo~0+salario,data=datos_consumo)
summary(lm_consumo)
# p-value: 0.0002126 por lo tanto es significativo a 10,5 y 1% 
#b1 =0.93987. Eso significa que que al aumentar el consumo en una unidad el salario se incrementa en 0.93987 unidades

# MODELO log-log
datos_consumo_log=data.frame(consumo=log(datos_consumo$consumo),salario=log(datos_consumo$salario))
datos_consumo$lnconsumo=log(datos_consumo$consumo)
datos_consumo$lnsalario=log(datos_consumo$salario)

lm_log_log<-lm(consumo~0+salario,data=datos_consumo_log)
summary(lm_log_log)

#El p-value no cambia y es significativo a todos los valores porque es muy pequeño
#Interpretacion. Cuando el salario varian en 1% el consumo sube en .98%


## MODELO lin-log ==> Consumoi = B1 ln(salarioi) + ui

lm_lin_log<-lm(consumo~0+lnsalario,data=datos_consumo)
summary(lm_lin_log)

# Interpretacion. Al variar en consumo en 1% el salario varia e 11.542/100 unidaddes

#### Modelo log-lin

lm_log_lin<-lm(lnconsumo~0+salario,data = datos_consumo)
summary(lm_log_lin)
# p-value: 0.002038   significativo al 10% y al 5% pero no significativo al 1%
# Interpretacion: aumentar el salario en una unidad el consumo sube en 0.078500 *100=7.8% 



#Varianza del modelo
varianza=sum( (lm_consumo$residuals)^2)/(length((datos_consumo))-1)
sqrt(varianza)


varianza2=var(lm_consumo$residuals)

mean(datos_consumo$consumo)

mean(datos_consumo$salario)

residuals<-c(1.864,2.525,-5.813,2.487)
var(residuals)


mean(residuals)

sum(residuals-mean(residuals)^2)/3


dt(0.25,1)

qt(.1,1,lower.tail = FALSE)
