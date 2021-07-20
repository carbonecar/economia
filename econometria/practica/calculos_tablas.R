qt()

qt(0.0,4997)

1 - pt(1:5, df = 1)
qt(.975, df = c(1:10,20,50,100,1000))

library(wooldridge)

regressor_wage<-lm(log(wage1$wage)~wage1$educ+wage1$exper+wage1$tenure,data = wage1)

summary(regressor_wage)

exp<-wage1[,3]
t.test(exp)



plot(data_mincer$edad,data_mincer$ingreso)
1-pnorm(1.85)

qf
pt(1.85,40,lower.tail = FALSE)


library(wooldridge)

test_1<-lm(log(mlb1$salary)
           ~mlb1$years+mlb1$gamesyr+mlb1$bavg+mlb1$hrunsyr+mlb1$rbisyr)

summary(test_1)

sum(test_1$residuals^2)
       
qf(3,28)

n1n2k <- sum(wage1$female==1) + sum(wage2$female==0) - 2*3

data=wage1[wage1$female==1,]

qf(0.01,3,60,lower.tail = FALSE)


pnorm(4428.413402,lower.tail = FALSE)
