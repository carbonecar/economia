library(wooldridge)

salary_roe<-data.frame(ceosal1$salary,ceosal1$roe)

ceosal_lm<-lm(ceosal1.salary ~ ceosal1.roe,data=data.frame(ceosal1$salary,ceosal1$roe))

summary(ceosal_lm)

cor(salary_roe)

ceosal_minimal<-data.frame(ceosal1$salary,ceosal1$roe)

ceosal_minimal<-data.fram(ceosal)


ceosal_minimal<-head(ceosal_minimal)

summary(ceosal_minimal)

summary(ceosal_minimal$ceosal1.salary)
cov(ceosal_minimal)
mean(ceosal_minimal$ceosal1.salary)
sd(ceosal_minimal$ceosal1.salary)

var(ceosal_minimal$ceosal1.salary,na.rm = TRUE)

test <- c(41,34,39,34,34,32,37,32,43,43,24,32)

ceosal_minimal$ceosal1.salary

sum(ceosal_minimal$ceosal1.salary)

summary(ceosal_minimal$ceosal1.salary)

hist(ceosal_minimal$ceosal1.salary)
 ceosal_minimal[1,]-mean(ceosal_minimal$ceosal1.salary)
 
 
ceosal_lm<-lm(ceosal_minimal$ceosal1.salary ~ ceosal_minimal$ceosal1.roe,data=ceosal_minimal)

summary(ceosal_lm)


#############################################
############# Regresion multiple ############
#############################################

multiple_lm<-lm(gpa1$colGPA~gpa1$hsGPA + gpa1$ACT,data =gpa1)
summary(multiple_lm)
library(stargazer)
stargazer(multiple_lm, type = "text")

