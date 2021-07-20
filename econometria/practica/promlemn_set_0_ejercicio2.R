notasEstadistica<-c(10,10,8,8,7,9,6,4,2,2)
notasFilosofia<-c(4,2,2,4,5,4,5,8,9,10)


puntoA_CalculoMedias<-function(){
  print("Summary for Estadistica")
  print(summary(notasEstadistica))
  print("Symmary for filosofia")
  print(summary(notasFilosofia))
  
  cat(sprintf("Media para estadistica %.2f\n",mean(notasEstadistica)))
  
  cat(sprintf("Media para filosofia %.2f\n",mean(notasFilosofia)))
  
}

## Varianza muestral
puntoB_CalculoVarianzas<-function(){
  cat(sprintf("variance notasEstadistica %.2f\n",var(notasEstadistica)))
  cat(sprintf("variance notasFilosofia %.2f\n",var(notasFilosofia)))
  
  sprintf("Another way is taken standar deviation squaring")
  ## Otra forma de calcular la varianza
  cat(sprintf("variance of notasEstadistica %.2f\n",sd(notasEstadistica)^2))
  cat(sprintf("variance of notasFilosofia %.2f\n",sd(notasFilosofia)^2))
}

puntoC_CalculoCovarianzas<-function(){
  #Covarianzas entre estadistica y filosofia (muestrales)
  covariance<-cov(notasEstadistica,notasFilosofia)
  cat(sprintf("covariance estadistica y filosofia: %.2f\n",covariance))
}

puntoD_CoeficienteCorrelacion<-function(){
  #coeficiente de correlacion
  cor.test(notasEstadistica,notasFilosofia)
  cor(notasEstadistica,notasFilosofia) 
}

puntoExtra_calculoRegresionLineal<-function(){
  #Otra forma es calcular la regresion directamente
  #Aca devuelve el coeficiente de determinacion (R-squared y no la correlacion)
  
  notas<-matrix(c(notasEstadistica,notasFilosofia), nrow = 10, ncol = 2)
  notaslm<-lm(notasFilosofia ~ notasEstadistica,data=as.data.frame(notas))
  summary(notaslm)
}

puntoA_CalculoMedias()
puntoB_CalculoVarianzas()
puntoC_CalculoCovarianzas()
puntoD_CoeficienteCorrelacion()
puntoExtra_calculoRegresionLineal()







