# Punto 1 

# Punto A
library(stargazer)
vectorX_punto_1<-rnorm(1000,1,10)
vectorU<-rnorm(1000,0,2)

#Tomamos b0=3 y b1=5
vectorY<-3+5*vectorX_punto_1
vectorY=vectorY+vectorU

#Nube de puntos del vector y,x
plot(vectorX_punto_1,vectorY)
# Los puntos parecen seguir una funcion lineal

datos=data.frame(vectorX_punto_1,vectorY)

#Punto B
modelo_1<-lm(vectorY~vectorX_punto_1,data = datos)
abline(modelo_1)

modelo_2<-lm(vectorY~0+vectorX_punto_1,data = datos)
abline(modelo_2)
abline(h = mean(vectorX_punto_1), col="red", lwd=3, lty=2)


plot(media)
abline(media)

stargazer(modelo_1,type="text")
stargazer(modelo_2,type="text")

stargazer(modelo_1,modelo_2,type="text")


summary(modelo_1)

summary(modelo_2)
