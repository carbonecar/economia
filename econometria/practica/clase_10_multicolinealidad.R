x1=rnorm(30000,1,2)
x2=x1+rnorm(30000,0,1)

cor(x1,x2)

y=2+5*x1+2*x2+rnorm((30000,0,1))


reg=lm(y~.)




#punto 2

library(stargazer)

library(GGally)
#install.packages('GGally')
