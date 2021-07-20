aniosEducacion<-c(6,7,8,9,10,11,12,13,14,15)
salarioHorario<-c(69,74,80,86,93,103,113,124,137,151)

summary(aniosEducacion)

puntoA_CalculoMedias<-function(){
  print("Summary for Anios educacion")
  print(summary(aniosEducacion))
  print("Symmary salarioHorario")
  print(summary(salarioHorario))
  
  cat(sprintf("Media para aniosEducacion %.2f\n",mean(aniosEducacion)))
  
  cat(sprintf("Media para salarioHorario %.2f\n",mean(salarioHorario)))
  
}

## Varianza muestral
puntoB_CalculoVarianzas<-function(){
  cat(sprintf("variance aniosEducacion %.2f\n",var(aniosEducacion)))
  cat(sprintf("variance salarioHorario %.2f\n",var(salarioHorario)))
  
  sprintf("Another way is taken standar deviation squaring")
  ## Otra forma de calcular la varianza
  cat(sprintf("variance of aniosEducacion %.2f\n",sd(aniosEducacion)^2))
  cat(sprintf("variance of salarioHorario %.2f\n",sd(salarioHorario)^2))
}

puntoC_CalculoCovarianzas<-function(){
  #Covarianzas entre estadistica y filosofia (muestrales)
  covariance<-cov(aniosEducacion,salarioHorario)
  cat(sprintf("covariance estadistica y filosofia: %.2f\n",covariance))
}

puntoD_CoeficienteCorrelacion<-function(){
  #coeficiente de correlacion
  cor.test(aniosEducacion,salarioHorario)
  #cat(sprintf("coeficiente de correlacion %.2f\n",cor(aniosEducacion,salarioHorario)))
}

puntoExtra_calculoRegresionLineal<-function(){
  #Otra forma es calcular la regresion directamente
  #Aca devuelve el coeficiente de determinacion (R-squared y no la correlacion)
  
  notas<-matrix(c(aniosEducacion,salarioHorario), nrow = 10, ncol = 2)
  notaslm<-lm(salarioHorario ~ aniosEducacion,data=as.data.frame(notas))
  summary(notaslm)
}

puntoA_CalculoMedias()
puntoB_CalculoVarianzas()
puntoC_CalculoCovarianzas()
puntoD_CoeficienteCorrelacion()
puntoExtra_calculoRegresionLineal()







