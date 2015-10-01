
rm(list=ls())

n=1e6
set.seed(1)
e=rnorm(n,0,0.1)
x=runif(n,-0.5,0.5)

b0=0.5
b1=1

y= b0+b1*x+e

yb=rbinom(n,1,y)
summary(yb)

lm(y~x)
lm(yb~x)


##
set.seed(1)
s=.9
e=rnorm(n,0,s)
x=runif(n,-0.5,0.5)

b0=0.5
b1=.1

y= b0+b1*x+e

yb=rbinom(n,1,y)
summary(yb)

lm(y~x)
lm(yb~x)

xb=b0+b1*x
eps=(dnorm(-xb/s)-dnorm((1-xb)/s))/(pnorm(-xb/s)-pnorm((1-xb)/s))
hist(eps)
mean(y)
