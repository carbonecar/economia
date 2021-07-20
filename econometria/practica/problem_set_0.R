#############################################
############### P0 ##########################
#############################################

notasEstadistica<-c(10,10,8,8,7,9,6,4,2,2)
notasEconometria<-c(9,10,8,6,7,9,5,3,2,1)

puntoA_CalculoMedias<-function(){
  print("Summary for Estadistica")
  print(summary(notasEstadistica))
  print("Symmary for econometria")
  print(summary(notasEconometria))
  
  cat(sprintf("Media para estadistica %.2f\n",mean(notasEstadistica)))

  cat(sprintf("Media para econometria %.2f\n",mean(notasEconometria)))
  
}

## Varianza muestral
puntoB_CalculoVarianzas<-function(){
  cat(sprintf("variance notasEstadistica %.2f\n",var(notasEstadistica)))
  cat(sprintf("variance notasEconometria %.2f\n",var(notasEconometria)))
  
  sprintf("Another way is taken standar deviation squaring")
  ## Otra forma de calcular la varianza
  cat(sprintf("variance of notasEstadistica %.2f\n",sd(notasEstadistica)^2))
  cat(sprintf("variance of notasEconometria %.2f\n",sd(notasEconometria)^2))
}

puntoC_CalculoCovarianzas<-function(){
  #Covarianzas entre estadistica y econometria (muestrales)
  covariance<-cov(notasEstadistica,notasEconometria)
  cat(sprintf("covariance estadistica y econometria: %.2f\n",covariance))
}

puntoD_CoeficienteCorrelacion<-function(){
  #coeficiente de correlacion
  cor.test(notasEstadistica,notasEconometria)
  cor(notasEstadistica,notasEconometria) 
}

puntoExtra_calculoRegresionLineal<-function(){
  #Otra forma es calcular la regresion directamente
  #Aca devuelve el coeficiente de determinacion (R-squared y no la correlacion)
  
  notas<-matrix(c(notasEstadistica,notasEconometria), nrow = 10, ncol = 2)
  notaslm<-lm(notasEconometria ~ notasEstadistica,data=as.data.frame(notas))
  summary(notaslm)
}

puntoA_CalculoMedias()
puntoB_CalculoVarianzas()
puntoC_CalculoCovarianzas()
puntoD_CoeficienteCorrelacion()
puntoExtra_calculoRegresionLineal()



#############################################
############### P1 ##########################
#############################################

# library
plot(notasEstadistica,notasEconometria)
plot(notasEconometria,notasEstadistica)

scatter.smooth(notasEconometria,notasEstadistica)


