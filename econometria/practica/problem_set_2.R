media_desvio_varianza<-function(distribution){
  cat(sprintf("mean %.2f\n",mean(distribution)))
  cat(sprintf("standar deviation %.2f\n",sd(distribution)))
  cat(sprintf("variance %.2f\n",var(distribution)))
}


x<-rnorm(1000,0,1)
hist(x)

sprintf("%s",summary(x))

cat(summary(x))
y<-rnorm(1000,10,5)
hist(y)

z<-runif(1000,0,20)
hist(z)


media_desvio_varianza(x)
media_desvio_varianza(y)
media_desvio_varianza(z)


