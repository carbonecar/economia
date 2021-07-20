myVector <- c(1:3,6,5:1,6)

myVector
c
which.max(myVector)

which(myVector == max(myVector))


head(cars)



setwd("/Users/carbonecar/testprojects/test_r")
scatter.smooth(x=cars$speed,y=cars$dist,main="Dist - Speed")
cor(cars$speed, cars$dist)
linearMod <- lm(dist ~ speed, data=cars)
print(linearMod)
summary(linearMod)

empleados=read.csv("edad_salario.csv",header = TRUE)
head(empleados)

linearMod<-lm(salario ~ edad,data=empleados)

summary(linearMod)

consumo=read.csv("consumo_vs_pib.csv",header=TRUE)

consumolm<-lm(GCP_Y ~ PIB_X,data=consumo)
summary(consumolm)


setwd("/Users/carbonecar/testprojects/test_r")

bitcoineth=read.csv("eth_btc_price.csv",header=TRUE)

bitcoineth_lm<-lm(ETH ~ BTC,data=bitcoineth)
summary(bitcoineth_lm)

scatter.smooth(x=bitcoineth$BTC,y=bitcoineth$ETH,main="ETH - BTC")
cor(bitcoineth$BTC, bitcoineth$ETH)


mean(bitcoineth$BTC)
sum(bitcoineth$BTC)/424
sum(sqrt(empleados$salario))
empleados$salario


sum(empleados$salario^2)

sum(bitcoineth$BTC^2)

library(Rcmdr)

t.test()


