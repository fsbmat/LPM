
options(scipen=999)

remove(list=ls())
n=1e5

b0=.5
b1=1
b2=-0.75
set.seed(1)
large.data=data.frame(eps=rnorm(n,mean=0,sd=.1))
set.seed(3)
large.data$X=runif(n,-.5,.5)
#   runif(n,-.5,.5)

set.seed(4)
large.data$X1=runif(n)

large.data$Y=with(large.data,b0+b1*X+eps+b2*X1)  #+b2*X1
sd(large.data$Y)

large.data$Y.binary=with(large.data,ifelse(Y>=median(Y),1,-1))
large.data$Y.binary1=with(large.data,ifelse(Y>=median(Y),1,0))

coef(lm(Y~X+X1,large.data))
coef(lm(Y.binary1~X+X1,large.data))

coef(lm(Y.binary~X+X1,large.data))*2*sd(large.data$X)/.798





large.data$Y1=scale(large.data$Y)
lm(Y1~X+X1,large.data)
lm(Y.binary~X+X1,large.data)

coef(lm(Y1~X+X1,large.data))*.798
coef(lm(Y1~X+X1,large.data))
coef(lm(Y.binary~X+X1,large.data))/.798


coef(lm(Y.binary1~X+X1,large.data))*2/.798

sd(large.data$Y.binary1)

lm(Y.binary1~X+X1,large.data)
coef(lm(Y.binary~X+X1,large.data))*0.798/0.5
lm(Y~X+X1,large.data)

