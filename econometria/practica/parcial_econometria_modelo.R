#consumo = b0+b1*salario+u

consumo<-c(4,5,7,9,10,13)
salario<-c(5,7,8,10,11,14)

datos_consumo<-data.frame(consumo,salario)
datos_consumo$msalario=datos_consumo$salario-mean(datos_consumo$salario)
lm_datos_consumo<-lm(consumo~salario,data=datos_consumo)
summary(lm_datos_consumo)

#b0=-1.55738    b1=1.04262

# a) calculo de los estimadores centrados
b1<-cov(consumo,salario)/var(salario)
b0<-mean(consumo)-b1*mean(salario)

# b) varianza del modelo 
varianza_modelo<-(sum(lm_datos_consumo$residuals^2))/(length(consumo)-2)

# c) varianza estimadores
varianza_b1=varianza_modelo/(sum( (datos_consumo$salario-mean(datos_consumo$salario))^2))
varianza_b0= (varianza_modelo * sum(datos_consumo$salario^2))/( length(salario) * sum(datos_consumo$msalario^2))

# e) son estadisticamente significativos?
summary(lm_datos_consumo)
# Segun el p-value: 
#         el salrio es significativo al 10%, al 5% y al 1%
#         el el intercepto es signitficativo al 10% pero no al 5% ni al 1%. 

# Test de hipotesis para B1 B1<>0 prueba de dos colas, tomamos un alpha de 0.05 y comparamos contra un t=1.9 
# Aca el enunciado esta mal, t no puede ser 1.9 porque los gl son length(salario)-2 = 6-2=4 y para 4 grados de libertad los valores posibles son
# 0,7407;1.5332;2,1318;2,7764;3,7468;4,6471. 
# Asi que motmo apha =0,05 y uso 2,7765 como valor de t para comparar

t_estimado_b1<-b1/sqrt(varianza_b1)
#como t_estimado> 2.7765 entonces el modelo es significativo (y por mucho)
#idem para b0
t_estimado_b0<- b0/sqrt(varianza_b0)
# como  t_estimado=-2.68 > T entonces NRHN por lo tanto el t no es significativo al 5%. 

sd_b1<-sqrt(varianza_b1)
sb_b0<-sqrt(varianza_b0)
b_1critico<-2.7765*sd_b1
b_0critico<-2.7765*sb_b0

#como b>b_1 critico (1.05>0.16) RHN y el modelo es valido o significativo
#b0>-b0critoco NRHN por lo tanto b0 no es significativo al 5%. 


# e) Intervalo de confianza para los betas
#b1   b1+-t*sb
limite_inferior_b1<-b1-2.7765*sd_b1
limite_superior_b1<-b1+2.7765*sd_b1

#b0   b0+-t*sb
limite_inferior_b0<-b0-2.7765*sd_b0
limite_superior_b0<-b0+2.7765*sd_b0

# predict(cars.lm, newdata = new.dat, interval = 'prediction')

plot(lm_datos_consumo)
plot(datos_consumo$salario, datos_consumo$consumo, xlab='Salario', ylab='Consumo')
abline(lm_datos_consumo)
mean(datos_consumo$consumo)
