
###  Figure 1 (3 densities overlay)

rm(list = ls())
n=1e6
set.seed(123)
e1=rnorm(n)
set.seed(231)
e2=rlogis(n,0,sqrt(3)/pi)
set.seed(312)
e3=runif(n,min=-sqrt(3),max=sqrt(3))

### density plot
pdf("Fig1-Density.pdf",7,4)
par(mfrow=c(1,2),mar=c(1.2,1,0,0)+0.2,mgp=c(0.45,0.45,0))
plot(density(e2),xlab="",main="",xlim=c(-3,3),lty=2,cex.axis=0.7,
     ylab="")
lines(density(e1))
lines(density(e3),lty=4)
#
arrows(-2,0.38,-0.2,.445,length=0.1,code=2)
text(-2,.35,"Logistic")
#
arrows(2,0.36,0.2,.39,length=0.1,code=2)
text(2,.34,"Normal")
#
arrows(0,0.2,0,.28,length=0.1,code=2)
text(0,.18,"Uniform")

### CDF
rm(list = ls())
n=1e4
set.seed(123)
e11=rnorm(n)
set.seed(231)
e12=rlogis(n,0,sqrt(3)/pi)
set.seed(312)
e13=runif(n,min=-sqrt(3),max=sqrt(3))

plot(e13,punif(e13,min=-sqrt(3),max=sqrt(3)),pch=".",xlim=c(-3,3),
     ylab="",xlab="",col="gray48",cex.axis=0.7)
points(e12,plogis(e12,0,sqrt(3)/pi),pch=".",col="darkgray")
points(e11,pnorm(e11,0,1),pch=".")
abline(h=0,col="grey")
abline(h=1,col="grey")
#
arrows(-0.7,0.82,0.8,.845,length=0.06,code=2)
text(-2,.82,"Logistic")
#
arrows(2,.6,0.9,0.8,length=0.06,code=2)
text(2,.55,"Normal")
#
arrows(0,0.2,-1,.2,length=0.06,code=2)
text(1,.2,"Uniform")

dev.off()


##############
### Simulation for Explanatory
#############
rm(list=ls())
n=1e3
set.seed(123)
data=data.frame(x1=runif(n,-1,1))
set.seed(231)
data$x2=rnorm(n,0,0.1)
set.seed(312)
require(mvtnorm)
t=rmvnorm(n,sigma=matrix(c(.5,-.3,-.3,.5),2,2))
data$x3=t[,1]; data$x4=t[,2]
# error
set.seed(123)
data$e1=rnorm(n,0,1)
set.seed(231)
data$e2=rlogis(n,0,1) #sqrt(3)/pi
set.seed(321)
data$e3=runif(n,min=0,max=1) #-sqrt(3); sqrt(3)
## temporary
data$x1=with(data,x1)
data$x2=with(data,x2)
data$x3=with(data,x3)
data$x4=with(data,x4)
data$x12=with(data,x1^2)
data$x22=with(data,x2^2)
data$x3x4=with(data,x3*x4)
# latent
data$y1=with(data,1*x1-1*x2+0.5*x3-0.5*x4+1*x12-1*x22+0.5*x3x4+e1)
data$y2=with(data,1*x1-1*x2+0.5*x3-0.5*x4+1*x12-1*x22+0.5*x3x4+e2)
data$y3=with(data,1*x1-1*x2+0.5*x3-0.5*x4+1*x12-1*x22+0.5*x3x4+e3)

# summary(data)
# observed binary
data$z1=with(data,ifelse(y1>=mean(y1),1,0))
data$z2=with(data,ifelse(y2>=mean(y2),1,0))
data$z3=with(data,ifelse(y3>=mean(y3),1,0))

# [The variance scales by 1.612^2, so the coefficients 
# themselves scale by 1.61; see Amemiya, T. (1981) 
# Quantitative response models: a survey, 
# Journal of Economic Literature 19, 1483-1536.]

# sample datasets
n.sample=c(50,500,50000)
set.seed(123)
sample.s=data[sample.int(n,n.sample[1]),]
set.seed(231)
sample.m=data[sample.int(n,n.sample[2]),]
set.seed(312)
sample.l=data[sample.int(n,n.sample[3]),]
##
m1=as.formula(z1~x1+x2+x3+x4)
m2=as.formula(z2~x1+x2+x3+x4)
m3=as.formula(z3~x1+x2+x3+x4)
#####
## Coefficient significance ###
require(boot)
require(ggplot2)
require(gridExtra)

#
dt=sample.l
require(microbenchmark)
require(brglm)
microbenchmark(lm(m1,data = dt))
microbenchmark(glm(m2,data = dt,family = binomial(link="logit")))
microbenchmark(glm(m3,data = dt,family = binomial(link="probit")))
microbenchmark(brglm(m2,data = dt,family = binomial(link="logit")))
microbenchmark(brglm(m3,data = dt,family = binomial(link="probit")))

#
dt=data
## model with interactions
mi1=lm(y1~x1+x2+x3+x4+I(x1^2)+I(x2^2)+x3*x4,data = data)
mi2=glm(z1~x1+x2+x3+x4+I(x1^2)+I(x2^2)+x3*x4,data = data,family = binomial(link = "probit"))
mi3=glm(z2~x1+x2+x3+x4+I(x1^2)+I(x2^2)+x3*x4,data = data,family = binomial(link = "logit"))
mi4=lm(z3~x1+x2+x3+x4+x12+x22+x3x4,data = data)

stargazer::stargazer(mi1,mi2,mi3,mi4)


## plots for unbounded predictions
fm1=lm(m3,data = sample.l)
summary(fm1)
a=fm1$fitted.values
sum(a<=0)/length(a)
sum(a>=1)/length(a)
#
a[a<=0]=0.001; a[a>=1]=0.999
w=1/sqrt(a*(1-a))
fm11=lm(m3,data=sample.l,weights = w)
summary(fm11)
summary(fm1$fitted.values)
##
fm2=lm(m1,data = sample.l)
summary(fm2)
b=fm2$fitted.values
sum(b<=0)/length(b)
sum(b>=1)/length(b)
#
b[b<=0]=0.001; b[b>=1]=0.999
w1=1/sqrt(b*(1-b))
fm21=lm(m1,data=sample.l,weights = w1)
summary(fm21)
summary(fm1$fitted.values)
####

stargazer::stargazer(list(fm2,fm21),list(fm1,fm11))

#
flm1=glm(m3,data = sample.s,family = binomial(link = "logit"))
summary(flm1)
#
flm2=glm(m1,data = sample.s,family = binomial(link = "logit"))
#
pdf("unbnd-pred.pdf",7,4)
par(mfrow=c(1,2))

#
plot(fm21$fitted.values,predict.glm(flm2,type = "response"),ylim = c(-.5,1.5),
     xlim = c(-.5,1.5),xlab="LPM Predictions",ylab = "Logistic Predictions")
abline(h=0,lty=2)
abline(h=1,lty=2)
abline(v=0,lty=2)
abline(v=1,lty=2)
#
plot(fm1$fitted.values,predict.glm(flm1,type = "response"),ylim = c(-.5,1.5),
     xlim = c(-.5,1.5),xlab="LPM Predictions",ylab = "")
abline(h=0,lty=2)
abline(h=1,lty=2)
abline(v=0,lty=2)
abline(v=1,lty=2)
#
dev.off()

#
small11=boot.coef(nBoot=100,dt,formula=m1,size = 50) #normal error
small21=boot.coef(nBoot=100,dt,formula=m2,size = 50) #Logis error
small31=boot.coef(nBoot=100,dt,formula=m3,size = 50) #unif error
#
small12=boot.coef(nBoot=100,dt,formula=m1,size = 500) #normal error
small22=boot.coef(nBoot=100,dt,formula=m2,size = 500) #Logis error
small32=boot.coef(nBoot=100,dt,formula=m3,size = 500) #unif error
#
small13=boot.coef(nBoot=100,dt,formula=m1,size = 50000) #normal error
small23=boot.coef(nBoot=100,dt,formula=m2,size = 50000) #Logis error
small33=boot.coef(nBoot=100,dt,formula=m3,size = 50000) #unif error

##
small.sig1=rbind(small11$sig,small21$sig,small31$sig)
small.sig2=rbind(small12$sig,small22$sig,small32$sig)
small.sig3=rbind(small13$sig,small23$sig,small33$sig)

s1=ggplot(small.sig1,aes(x=sig,y=freq,group=model))+
  facet_wrap(~err+Coef,ncol=4)+theme_bw()+
  geom_bar(stat="identity", position="dodge",
           aes(group=model,fill=model))+xlab("Significance Level")+
  ylab("Frequency")+
  ggtitle("Sample Size = 50")+
  scale_fill_manual(values=c("grey50", "black", "grey80"))


s2=ggplot(small.sig2,aes(x=sig,y=freq,group=model))+
  facet_wrap(~err+Coef,ncol=4)+theme_bw()+
  geom_bar(stat="identity", position="dodge",
           aes(group=model,fill=model))+xlab("Significance Level")+
  ylab("Frequency")+
  ggtitle("Sample Size = 500")+
  scale_fill_manual(values=c("grey50", "black", "grey80"))

s3=ggplot(small.sig3,aes(x=sig,y=freq,group=model))+
  facet_wrap(~err+Coef,ncol=4)+theme_bw()+
  geom_bar(stat="identity", position="dodge",
           aes(group=model,fill=model))+xlab("Significance Level")+
  ylab("Frequency")+
  ggtitle("Sample Size = 50,000")+
  scale_fill_manual(values=c("grey50", "black", "grey80"))
  


require(gridExtra)
#
pdf("all-sig.pdf",11,11)
grid.arrange(s1,s2,s3)
dev.off()

##
small1=rbind(small11$mar,small21$mar,small31$mar)
small2=rbind(small12$mar,small22$mar,small32$mar)
small3=rbind(small13$mar,small23$mar,small33$mar)

#
small1$Model=factor(small1$Model,levels=c(1,2,3),labels = c("Probit","Logit","LPM"))
small2$Model=factor(small2$Model,levels=c(1,2,3),labels = c("Probit","Logit","LPM"))
small3$Model=factor(small3$Model,levels=c(1,2,3),labels = c("Probit","Logit","LPM"))
#
m1=ggplot(subset(small1,Coef.name!='x0'),aes(x=Coef.name,y=Coef))+
  facet_wrap(~err,ncol=3)+theme_bw()+
  geom_boxplot(aes(fill=Model),alpha=0.8)+
  scale_fill_manual(values=c("grey80", "grey50", "black"))+
  xlab("")+ylab(expression(paste("Marginal Effect (",hat(ME),")")))+
  ggtitle("Sample Size = 50")
  

m2=ggplot(subset(small2,Coef.name!='x0'),aes(x=Coef.name,y=Coef))+
  facet_wrap(~err,ncol=3)+theme_bw()+
  geom_boxplot(aes(fill=Model),alpha=0.8)+
  scale_fill_manual(values=c("grey80", "grey50", "black"))+
  xlab("")+ylab(expression(paste("Marginal Effect (",hat(ME),")")))+
  ggtitle("Sample Size = 500")
  
m3=ggplot(subset(small3,Coef.name!='x0'),aes(x=Coef.name,y=Coef))+
  facet_wrap(~err,ncol=3)+theme_bw()+
  geom_boxplot(aes(fill=Model),alpha=0.8)+
  scale_fill_manual(values=c("grey80", "grey50", "black"))+
  xlab("")+ylab(expression(paste("Marginal Effect (",hat(ME),")")))+
  ggtitle("Sample Size = 50,000")
  

#
pdf("all-mar.pdf",11,7)
grid.arrange(m1,m2,m3)
dev.off()



##### function to bootstrap
boot.coef=function(nBoot,dt,formula,size){
#
b01=matrix(NA,nBoot,5)
b02=matrix(NA,nBoot,5)
b03=matrix(NA,nBoot,5)
#
b11=matrix(NA,nBoot,5)
b12=matrix(NA,nBoot,5)
b13=matrix(NA,nBoot,5)
#
b21=matrix(NA,nBoot,5)
b22=matrix(NA,nBoot,5)
b23=matrix(NA,nBoot,5)
#
b31=matrix(NA,nBoot,5)
b32=matrix(NA,nBoot,5)
b33=matrix(NA,nBoot,5)
#
b41=matrix(NA,nBoot,5)
b42=matrix(NA,nBoot,5)
b43=matrix(NA,nBoot,5)
#
c1=matrix(NA,nBoot,5)
c2=matrix(NA,nBoot,5)
c3=matrix(NA,nBoot,5)
#
for(i in 1:nBoot)
{
  set.seed(i)
  Bsample=dt[sample.int(dim(dt)[1],size,replace = T),]
  f1=glm(formula,data=Bsample,family = binomial(link="probit"))
  f2=glm(formula,data=Bsample,family = binomial(link="logit"))
  f4=lm(formula,data=Bsample)
  xb=f4$fitted.values
  xb[which(xb>=1)]=0.999;xb[which(xb<=0)]=0.001
  Bsample$w=with(Bsample,1/sqrt(xb*(1-xb)))
  f3=lm(formula,data=Bsample,weights = w)
  #
  bin=c(0,0.001,0.01,0.05,0.1,1)
  c1[i,]=mean(dnorm(f1$linear.predictors))*summary(f1)$coefficients[,1]
  c2[i,]=mean(dlogis(f2$linear.predictors))*summary(f2)$coefficients[,1]
  c3[i,]=summary(f3)$coefficients[,1]
#   mean(dnorm(f1$linear.predictors))*summary(f1)$coefficients[,1]
#   mean(dlogis(f2$linear.predictors))*summary(f2)$coefficients[,1]
#   summary(f3)$coefficients[,1]
  #
  sf1=summary(f1)$coefficients[,4]
  sf2=summary(f2)$coefficients[,4]
  sf3=summary(f3)$coefficients[,4]
  #
  b01[i,]=hist(sf1[1],bin,plot=F)$counts
  b11[i,]=hist(sf1[2],bin,plot=F)$counts
  b21[i,]=hist(sf1[3],bin,plot=F)$counts
  b31[i,]=hist(sf1[4],bin,plot=F)$counts
  b41[i,]=hist(sf1[5],bin,plot=F)$counts
  #
  b02[i,]=hist(sf2[1],bin,plot=F)$counts
  b12[i,]=hist(sf2[2],bin,plot=F)$counts
  b22[i,]=hist(sf2[3],bin,plot=F)$counts
  b32[i,]=hist(sf2[4],bin,plot=F)$counts
  b42[i,]=hist(sf2[5],bin,plot=F)$counts
  #
  b03[i,]=hist(sf3[1],bin,plot=F)$counts
  b13[i,]=hist(sf3[2],bin,plot=F)$counts
  b23[i,]=hist(sf3[3],bin,plot=F)$counts
  b33[i,]=hist(sf3[4],bin,plot=F)$counts
  b43[i,]=hist(sf3[5],bin,plot=F)$counts
  #
}

cb1=data.frame('Coef'='b1',sig=c('<0.001','<0.01','<0.05','<0.1','n.s')
               ,'Pr'=colSums(b11),'Lo'=colSums(b12),'LPM'=colSums(b13))
cb2=data.frame('Coef'='b2',sig=c('<0.001','<0.01','<0.05','<0.1','n.s')
               ,'Pr'=colSums(b21),'Lo'=colSums(b22),'LPM'=colSums(b23))
cb3=data.frame('Coef'='b3',sig=c('<0.001','<0.01','<0.05','<0.1','n.s')
               ,'Pr'=colSums(b31),'Lo'=colSums(b32),'LPM'=colSums(b33))
cb4=data.frame('Coef'='b4',sig=c('<0.001','<0.01','<0.05','<0.1','n.s')
               ,'Pr'=colSums(b41),'Lo'=colSums(b42),'LPM'=colSums(b43))
cb=rbind(cb1,cb2,cb3,cb4)
cb$err=with(cb,ifelse(formula==m1,'Normal',ifelse(formula==m2,'Logistic','Uniform')))
cb.reshape=reshape(cb,varying = c("Pr","Lo","LPM"),
                   v.names = "freq",timevar = "model",
                   times = c("Probit","Logit","LPM"),
                   direction = "long")

mb=rbind(cbind(1,c1),cbind(2,c2),cbind(3,c3))
colnames(mb)=c('Model','x0','x1','x2','x3','x4')
mb=as.data.frame(mb)
mb$err=with(mb,ifelse(formula==m1,'Normal',ifelse(formula==m2,'Logistic','Uniform')))
mb.reshape=reshape(mb,varying = c("x0","x1","x2","x3","x4"),
                   v.names = "Coef",timevar = "Coef.name",
                   times = c("x0","x1","x2","x3","x4"),
                   direction = "long")
# return
return(list(sig=cb.reshape,mar=mb.reshape))
}
###############



##################
#### selection model
#################

rm(list=ls())

require(mvtnorm) # for rmvnorm()

n=1e6
rho=-0.4
set.seed(123)
data=data.frame(x1=runif(n))
set.seed(231)
data$x2 <- rnorm(n)
require(mvtnorm)
set.seed(321)
t=rmvnorm(n,sigma=matrix(c(.5,.3,.3,.5),2,2))
data$x3=t[,1]; data$x4=t[,2]
cm <- diag(0.5,2); cm[1,2] <- cm[2,1] <- rho
ee <- rmvnorm(n=n, mean=c(0,0), sigma=cm)
data$e1=ee[,1]
data$e2=ee[,2]

data$y1=with(data,0.5*x1-0.5*x2+1.5*x3-1*x4-0.5+e1)
data$y=with(data,-1.5*x1+0.5*x2+1*x3+0.5+e2)
data$s=with(data,ifelse(y1>0,1,0))
data$yb=with(data,ifelse(y>median(y),1,0))
table(data$yb)
#
require(sampleSelection)
#
n.sample=c(500,5000,50000)
set.seed(123)
sample.s=data[sample.int(n,n.sample[1]),]
set.seed(231)
sample.m=data[sample.int(n,n.sample[2]),]
set.seed(312)
sample.l=data[sample.int(n,n.sample[3]),]
##
m1=as.formula(s~x1+x2+x3+x4)
m2=as.formula(y~x1+x2+x3)

## Prediction comparison
require(ROCR)
lpm.mod=lm(m1,data=sample.m)
logit.mod=glm(m1,data=sample.m,family=binomial(link = "logit"))
probit.mod=glm(m1,data=sample.m,family=binomial(link = "probit"))
#
lpm.pred=predict.lm(lpm.mod,newdata=sample.s,type = "response")
logit.pred=predict.glm(logit.mod,newdata=sample.s,type = "response")
probit.pred=predict.glm(probit.mod,newdata=sample.s,type = "response")

roc.legend=function(auc,lab)
{ 
  return(paste(lab,"=",round(auc,4)))
}    
#
pdf("pred-comp.pdf",7,4)
par(mfrow=c(1,2))
#
plot(performance(prediction(lpm.pred,sample.s$s),"tpr","fpr"),col="black",xaxis.cex.axis=0.5,
     yaxis.cex.axis=0.5,xlab.cex=0.5,ylab.cex=0.5)
lpm.auc=as.numeric(performance(prediction(lpm.pred,sample.s$s),"auc")@y.values)
plot(performance(prediction(logit.pred,sample.s$s),"tpr","fpr"),col="grey50",add=T,lty=2)
logit.auc=as.numeric(performance(prediction(logit.pred,sample.s$s),"auc")@y.values)
plot(performance(prediction(probit.pred,sample.s$s),"tpr","fpr"),col="grey80",add=T,lty=3)
probit.auc=as.numeric(performance(prediction(probit.pred,sample.s$s),"auc")@y.values)

lpm.roc=roc.legend(lpm.auc,"LPM")
logit.roc=roc.legend(logit.auc,"Logit")
probit.roc=roc.legend(probit.auc,"Probit")

legend.fill=c("black","grey50","grey80")
legend(.6,.6,c(lpm.roc,logit.roc,probit.roc),fill=legend.fill,
       col=legend.fill,cex=0.5,title="AUC")

if(!require(pROC)){
  install.packages("pROC")
  require(pROC)
}

#
lab1=c("LPM","LPM")
lab2=c("Log","Log")
lab3=c("Pro","Pro")

boxplot(lpm.pred~sample.s$s,boxwex=.15,medlwd=0.05,names=lab1,cex.axis=0.5)
boxplot(logit.pred~sample.s$s,boxwex=.15,medlwd=0.05,add=T,at=1:2-.32,names=lab2,cex.axis=0.5,border="grey30")
boxplot(probit.pred~sample.s$s,boxwex=.15,medlwd=0.05,add=T,at=1:2-.16,names=lab3,cex.axis=0.5,border="grey50")
abline(h=0.5,lty=2)
#
dev.off()

######
###
dt=data
nBoot=100
#
b1=selection.boot(nBoot,dt=data,m1,m2,size=500)
b2=selection.boot(nBoot,dt=data,m1,m2,size=5000)
b3=selection.boot(nBoot,dt=data,m1,m2,size=50000)
#
b=rbind(b1,b2,b3)
#
require(ggplot2)
s=ggplot(b,aes(x=Coef.name,y=Coef))+
  facet_wrap(~samplesize,ncol=3)+theme_bw()+
  geom_boxplot(aes(fill=Method),alpha=0.8)+
  scale_fill_manual(values=c("grey80", "grey50", "black"))+
  xlab("")+ylab(expression(paste("Coefficient (",hat(beta),")")))


pdf("selection.pdf",7,3)
print(s)
dev.off()



##########
selection.boot=function(nBoot,dt,m1,m2,size)
{
ml.o.coef=matrix(NA,nBoot,3)
hp.o.coef=matrix(NA,nBoot,3)
ol.o.coef=matrix(NA,nBoot,3)
for(i in 1:nBoot){
  set.seed(i)
  Bsample=dt[sample.int(dim(dt)[1],size,replace = T),]
  ml=selection(m1,m2,data=Bsample,method="2sls")
  ml.o.coef[i,]=ml$estimate[7:9]
    #
  hp=glm(m1,data=Bsample,family=binomial(link="probit"))
  Bsample$imr=dnorm(hp$linear.predictors)/pnorm(hp$linear.predictors)
  ol=lm(m1,data=Bsample)
  Bsample$ocor=ol$fitted.values-1
  Bsample1=subset(Bsample,s==1)
  
  hp.o=lm(y~x1+x2+x3+imr,data=Bsample1)
  hp.o.coef[i,]=hp.o$coef[-c(1,length(hp.o$coef))]
  #
  ol.o=lm(y~x1+x2+x3+ocor,data=Bsample1)
  ol.o.coef[i,]=ol.o$coef[-c(1,length(ol.o$coef))]
  ###
}
coef=rbind(cbind(1,ml.o.coef),cbind(2,hp.o.coef),cbind(3,ol.o.coef))
colnames(coef)=c("Method","x1","x2","x3")
coef.reshape=reshape(as.data.frame(coef),varying = c("x1","x2","x3"),
                   v.names = "Coef",timevar = "Coef.name",
                   times = c("x1","x2","x3"),
                   direction = "long")
coef.reshape$Method=factor(coef.reshape$Method,levels=c(1,2,3),
                           labels = c("MLE","Heckman(Probit)","Olsen(LPM)"))
coef.reshape$samplesize=paste('n=',size)
return(coef.reshape)
}
#######

### Bi Probit

nBoot=100
sim.size=c(5e2,5e3,5e4)
margin=vector("list",length(sim.size))

marpp=matrix(NA,nBoot,3)
marpl=matrix(NA,nBoot,3)
marlp=matrix(NA,nBoot,3)
marll=matrix(NA,nBoot,3)
for(i in 1:length(sim.size))
{
  for(j in 1:nBoot)
  {
    set.seed(j)
    mc.data=data[sample.int(dim(data)[1],sim.size[i],replace=T),]
    # data1=as.data.frame(mc.data)
    
    prob=glm(s~x1+x2+x3+x4,data=mc.data,family=binomial(link="probit"))
    lpm=lm(s~x1+x2+x3+x4,data=mc.data)
    #   creating millers ratio & olsen correction
    mc.data$lp.s=prob$linear.predictors
    mc.data$lam.s=with(mc.data,dnorm(lp.s)/pnorm(lp.s))
    mc.data$delta.s=with(mc.data,lam.s*(lam.s-lp.s))
    mc.data$lpmf=with(mc.data,lpm$fitted.values-1)
    # subsetting the data
    mc.data1=subset(mc.data,s==1)
    table(mc.data1$yb)
    # probit-probit
    prob.prob=glm(yb~x1+x2+x3+lam.s,data=mc.data1,family=binomial(link="probit"))
    # probit-lpm
    prob.lpm=lm(yb~x1+x2+x3+lam.s,data=mc.data1)
    # lpm-probit
    lpm.prob=glm(yb~x1+x2+x3+lpmf,data=mc.data1,family=binomial("probit"))
    # lpm-lpm
    lpm.lpm=lm(yb~x1+x2+x3+lpmf,data=mc.data1)
    ########################
    # Marginal calculation
    mar.const=mean(mc.data1$delta.s)
    marpp[j,1]=mean(dnorm(prob.prob$linear.predictors))*(coef(prob.prob)[2]-coef(prob.prob)[5]*coef(prob)[2]*mar.const)
    marpp[j,2]=mean(dnorm(prob.prob$linear.predictors))*(coef(prob.prob)[3]-coef(prob.prob)[5]*coef(prob)[3]*mar.const)
    marpp[j,3]=mean(dnorm(prob.prob$linear.predictors))*(coef(prob.prob)[4]-coef(prob.prob)[5]*coef(prob)[4]*mar.const)
    #
    marpl[j,1]=coef(prob.lpm)[2]-coef(prob.lpm)[5]*coef(prob)[2]*mar.const
    marpl[j,2]=coef(prob.lpm)[3]-coef(prob.lpm)[5]*coef(prob)[3]*mar.const
    marpl[j,3]=coef(prob.lpm)[4]-coef(prob.lpm)[5]*coef(prob)[4]*mar.const
    #
    marlp[j,1]=mean(dnorm(lpm.prob$linear.predictors))*(coef(lpm.prob)[2]+coef(lpm.prob)[5]*coef(lpm)[2])
    marlp[j,2]=mean(dnorm(lpm.prob$linear.predictors))*(coef(lpm.prob)[3]+coef(lpm.prob)[5]*coef(lpm)[3])
    marlp[j,3]=mean(dnorm(lpm.prob$linear.predictors))*(coef(lpm.prob)[4]+coef(lpm.prob)[5]*coef(lpm)[4])
    #
    marll[j,1]=coef(lpm.lpm)[2]+coef(lpm.lpm)[5]*coef(lpm)[2]
    marll[j,2]=coef(lpm.lpm)[3]+coef(lpm.lpm)[5]*coef(lpm)[3]
    marll[j,3]=coef(lpm.lpm)[4]+coef(lpm.lpm)[5]*coef(lpm)[4]
  }
  
  temp=rbind(c(marpp,"Prob-Prob"),)
  margin[i]=
  margin[i,1:2]=c(mean(marpp[,1]),sd(marpp[,1]))
  margin[i,3:4]=c(mean(marpp[,2]),sd(marpp[,2]))
  margin[i,5:6]=c(mean(marpp[,3]),sd(marpp[,3]))
  #
  margin[i,7:8]=c(mean(marpl[,1]),sd(marpl[,1]))
  margin[i,9:10]=c(mean(marpl[,2]),sd(marpl[,2]))
  margin[i,11:12]=c(mean(marpl[,3]),sd(marpl[,3]))
  #
  margin[i,13:14]=c(mean(marlp[,1]),sd(marlp[,1]))
  margin[i,15:16]=c(mean(marlp[,2]),sd(marlp[,2]))
  margin[i,17:18]=c(mean(marlp[,3]),sd(marlp[,3]))
  #
  margin[i,19:20]=c(mean(marll[,1]),sd(marll[,1]))
  margin[i,21:22]=c(mean(marll[,2]),sd(marll[,2]))
  margin[i,23:24]=c(mean(marll[,3]),sd(marll[,3]))
}



#plotting x1
load("Logit.margin")
temp=rbind(margin[,1:2],margin[,7:8],margin[,13:14],margin[,19:20])
logMardf=as.data.frame(temp)
logMardf$sample=rep(sim.size,4)
logMardf$label=c(rep("Prob-Prob",3),rep("Prob-LPM",3),rep("LPM-Prob",3),rep("LPM-LPM",3))
logMardf$sid=rep(1:3,4)
head(logMardf)
options(scipen = 99)
require(ggplot2)
p1=ggplot(logMardf,aes(x=sid,y=V1,group=label))+
  facet_wrap(~label,ncol = 4)+
  geom_errorbar(aes(ymin=V1-V2,ymax=V1+V2))+
  geom_line()+
  geom_point(size=3, shape=21, fill="white")+
  xlab("Sample Sizes")+
  ylab("Marginal Effect for x1")+
  theme_bw()

p1=p1+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background=element_blank())
p1
#plotting x2
temp1=rbind(margin[,3:4],margin[,7:8],margin[,11:12],margin[,15:16])
logMardf1=as.data.frame(temp1)
logMardf1$sample=rep(sim.size,4)
logMardf1$label=c(rep("Prob-Prob",7),rep("Prob-LPM",7),rep("LPM-Prob",7),rep("LPM-LPM",7))
logMardf1$sid=rep(1:7,4)
head(logMardf1)
options(scipen = 99)
require(ggplot2)
p2=ggplot(logMardf1,aes(x=sid,y=V1,group=label))+
  facet_wrap(~label,ncol=4)+
  geom_errorbar(aes(ymin=V1-V2,ymax=V1+V2))+
  geom_line()+
  geom_point(size=3, shape=21, fill="white")+
  xlab("Sample Sizes")+
  ylab("Marginal Effect for x2")+
  theme_bw()

p2=p2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background=element_blank())

require(gridExtra)
pdf("LogVsProb.pdf",11,5)
grid.arrange(p1,p2)
dev.off()

################################
######## Biprobit
###################################
rm(list=ls())
require(VGAM)    # for pbinorm()
require(mvtnorm) # for rmvnorm()
require(fBasics) # for makePositiveDefinite()
require(foreign) # for read.dta()

## =========================================== ##
##  Heckman probit: Log-likelihood function
## =========================================== ##
heckprob.ll <- function(pars,data){
  ## rho constrained b/ween -1, 1
  rho <- (exp(2*pars[length(pars)])-1)/(exp(2*pars[length(pars)])+1) 
  ## design matrix for the selection stage
  xg <- data$x.sel %*% pars[1:ncol(data$x.sel)] 
  ## design matrix for the outcome stage
  xb <- data$x.out %*% pars[(ncol(data$x.sel)+1):(ncol(data$x.sel)+ncol(data$x.out))]
  p0 <- pnorm(-xg)                                  ## Pr(sel=0)
  p2 <- pmax(pbinorm(xg,  xb,  cov12 = rho), 1e-10) ## Pr(sel=1 & out=1)
  p1 <- pmax(1-p0-p2, 1e-10)                        ## Pr(sel=1 & out=0)
  ## Use pmax to avoid p1<0 and p1==0
  loglik <- (data$y==0)*log(p0) + (data$y==1)*log(p1) + (data$y==2)*log(p2)
  return(sum(loglik))
}

## ==================================================
##  some functions for ML implementation
## ==================================================
## wrapper function
estim.mle <- function(likfn, data, method="Nelder-Mead",r=1){
  kg <- ncol(data$x.sel)
  kb <- ncol(data$x.out)
  k <- kg + kb + r
  set.seed(123)
  out <- try(optim(par = rep(0,k),  ## initial values
                   fn = likfn,
                   data = data,
                   method = method, ## BFGS, Nelder-Mead, SANN, CG
                   control = list(fnscale = -1, maxit = 10000),
                   hessian = T), silent=T)
}

## function to display results
display.mle <- function(fit, rnd=3){
  if(class(fit)=="try-error") { return(0)}
  else {
    beta <- fit$par
    vcov <- -solve(fit$hessian)
    vcov <- makePositiveDefinite(vcov)
    se <- sqrt(diag(vcov))
    z <- beta/se
    pval <- 2*(1-pnorm(abs(beta/se)))
    if (fit$convergence==0){
      rnd <- rnd}
    else rnd <- 0
    out <- cbind(beta = round(beta,rnd),
                 se = round(se,rnd),
                 z = round(z,rnd),
                 pval = round(pval,rnd))
    return(out)
  }
}

## function to transform atanh(rho) back to rho
get.rho <- function(x) (exp(2*x)-1)/(exp(2*x)+1)

## =========================================== ##
##  Try using these functions to estimate some
##  fake data
## =========================================== ##



gen.data <- function(n = 1000, rho = 0,dist=1){
  x1 <- runif(n)
  x2 <- rnorm(n)
  t=rmvnorm(n,sigma=matrix(c(.5,.3,.3,.5),2,2))
  x3 <- t[,1];x4 <- t[,2];
  cm <- diag(0.5,2); cm[1,2] <- cm[2,1] <- rho
  ee <- rmvnorm(n=n, mean=c(0,0), sigma=cm)
  y11=0.5*x1-0.5*x2+1.5*x3-1*x4-0.5+ ee[,1]
  y21=-1.5*x1+0.5*x2+1*x3+0.5 + ee[,2]
  y1=ifelse(y11>median(y11),1,0)
  y2=ifelse(y21>median(y21),1,0)
  
  y <- y1; y[y1==1 & y2==1] <- 2
  x.sel <- as.matrix(cbind(x1=x1,x2=x2,x3=x3,x4=x4,cons=1))
  x.out <- as.matrix(cbind(x1=x1,x2=x2,x3=x3,cons=1))
  out <- list(x.sel=x.sel, x.out=x.out,y=y)
  return(out)
}

mc.data=gen.data(n=5e4,rho=-0.4)
## fit the model
fit.mc.heck <- estim.mle(likfn=heckprob.ll, data=mc.data,method = "BFGS")
tbl <- display.mle(fit.mc.heck)
rownames(tbl) <- c("x.sel.x1","x.sel.x2","x.sel.x3","x.sel.x4","cons","x.out.x1","x.out.x2","x.out.x3","x.out.cons","atanh(rho)")
print(tbl)
######################################################################
##### marginal estimates
require(sampleSelection)

nBoot=100
sim.size=c(500, 5000, 50000)
margin=vector("list",length(sim.size))

marpp=matrix(NA,nBoot,3)
marpl=matrix(NA,nBoot,3)
marlp=matrix(NA,nBoot,3)
marll=matrix(NA,nBoot,3)
for(i in 1:length(sim.size))
{
  for(j in 1:nBoot)
  {
    mc.data=gen.data(n=sim.size[i],rho=-0.4)
    data=as.data.frame(mc.data)
    names(data)
    table(data$y)
    data$y1=with(data,ifelse(y>0,1,0))
    data$y2=data$y-1
    table(data$y2)
    prob=glm(y1~x.sel.x1+x.sel.x2+x.sel.x3+x.sel.x4,data=data,family=binomial("probit"))
    lpm=lm(y1~x.sel.x1+x.sel.x2+x.sel.x3+x.sel.x4,data=data)
    #   creating millers ratio & olsen correction
    data$lp.s=prob$linear.predictors
    data$lam.s=with(data,dnorm(lp.s)/pnorm(lp.s))
    data$delta.s=with(data,lam.s*(lam.s-lp.s))
    data$lpmf=with(data,lpm$fitted.values-1)
    # subsetting the data
    data1=subset(data,y>0)
    table(data1$y2)
    # probit-probit
    prob.prob=glm(y2~x.out.x1+x.out.x2+x.out.x3+lam.s,data=data1,family=binomial(link="probit"))
    # probit-lpm
    prob.lpm=lm(y2~x.out.x1+x.out.x2+x.out.x3+lam.s,data=data1)
    # lpm-probit
    lpm.prob=glm(y2~x.out.x1+x.out.x2+x.out.x3+lpmf,data=data1,family=binomial("probit"))
    # lpm-lpm
    lpm.lpm=lm(y2~x.out.x1+x.out.x2+x.out.x3+lpmf,data=data1)
    ########################
    # Marginal calculation
    mar.const=mean(data1$delta.s)
    marpp[j,1]=mean(dnorm(prob.prob$linear.predictors))*(coef(prob.prob)[2]-coef(prob.prob)[5]*coef(prob)[2]*mar.const)
    marpp[j,2]=mean(dnorm(prob.prob$linear.predictors))*(coef(prob.prob)[3]-coef(prob.prob)[5]*coef(prob)[3]*mar.const)
    marpp[j,3]=mean(dnorm(prob.prob$linear.predictors))*(coef(prob.prob)[4]-coef(prob.prob)[5]*coef(prob)[4]*mar.const)
    #
    marpl[j,1]=coef(prob.lpm)[2]-coef(prob.lpm)[5]*coef(prob)[2]*mar.const
    marpl[j,2]=coef(prob.lpm)[3]-coef(prob.lpm)[5]*coef(prob)[3]*mar.const
    marpl[j,3]=coef(prob.lpm)[4]-coef(prob.lpm)[5]*coef(prob)[4]*mar.const
    #
    marlp[j,1]=mean(dnorm(lpm.prob$linear.predictors))*(coef(lpm.prob)[2]+coef(lpm.prob)[5]*coef(lpm)[2])
    marlp[j,2]=mean(dnorm(lpm.prob$linear.predictors))*(coef(lpm.prob)[3]+coef(lpm.prob)[5]*coef(lpm)[3])
    marlp[j,3]=mean(dnorm(lpm.prob$linear.predictors))*(coef(lpm.prob)[4]+coef(lpm.prob)[5]*coef(lpm)[4])
    #
    marll[j,1]=coef(lpm.lpm)[2]+coef(lpm.lpm)[5]*coef(lpm)[2]
    marll[j,2]=coef(lpm.lpm)[3]+coef(lpm.lpm)[5]*coef(lpm)[3]
    marll[j,3]=coef(lpm.lpm)[4]+coef(lpm.lpm)[5]*coef(lpm)[4]
  }
  
  temp=rbind(cbind(marpp,"Probit-Probit"),cbind(marpl,"Probit-LPM"),cbind(marlp,"LPM-Probit"),
             cbind(marll,"LPM-LPM"))
  colnames(temp)=c("x1","x2","x3","model")
  temp=data.frame(temp,'sample'=sim.size[i])
  margin[[i]]=temp
}

temp1=rbind(margin[[1]],margin[[2]],margin[[3]])
dim(temp1)
names(temp1)

require(reshape2)
temp2=reshape(temp1,varying = c("x1","x2","x3"),
              v.names = "marginal",
              timevar = "Coefficient",
              times = c("x1","x2","x3"),
              direction = "long")
dim(temp2)
temp2$sample=with(temp2,as.character(sample))
#plotting x1
options(scipen = 99)
require(ggplot2)
p1=ggplot(temp2,aes(x=Coefficient,y=as.numeric(as.character(marginal))))+
  facet_wrap(~sample)+
  theme_bw()+
  geom_boxplot(aes(fill=model))+xlab("")+
  scale_fill_manual(values=c( "black", "grey80","grey50","grey30"))+ylab("Marginal Effect")
p1
  
require(gridExtra)
pdf("LogVsProb.pdf",11,5)
print(p1)
dev.off()


###########################
##########probit
#plotting x1
load("Probit.margin")
margin=margin[1:7,]
temp2=rbind(margin[,1:2],margin[,5:6],margin[,9:10],margin[,13:14])
logMardf2=as.data.frame(temp2)
logMardf2$sample=rep(sim.size,4)
logMardf2$label=c(rep("Prob-Prob",7),rep("Prob-LPM",7),rep("LPM-Prob",7),rep("LPM-LPM",7))
logMardf2$sid=rep(1:7,4)
head(logMardf2)
options(scipen = 99)
# require(ggplot2)
p3=ggplot(logMardf2,aes(x=sid,y=V1,group=label))+
  facet_wrap(~label,ncol = 4)+
  geom_errorbar(aes(ymin=V1-V2,ymax=V1+V2))+
  geom_line()+
  geom_point(size=3, shape=21, fill="white")+
  xlab("Sample Sizes")+
  ylab("Marginal Effect for x1")+
  theme_bw()

p3=p3+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background=element_blank())
p3
#plotting x2
temp3=rbind(margin[,3:4],margin[,7:8],margin[,11:12],margin[,15:16])
logMardf3=as.data.frame(temp3)
logMardf3$sample=rep(sim.size,4)
logMardf3$label=c(rep("Prob-Prob",7),rep("Prob-LPM",7),rep("LPM-Prob",7),rep("LPM-LPM",7))
logMardf3$sid=rep(1:7,4)
head(logMardf3)
# options(scipen = 99)
# require(ggplot2)
p4=ggplot(logMardf3,aes(x=sid,y=V1,group=label))+
  facet_wrap(~label,ncol=4)+
  geom_errorbar(aes(ymin=V1-V2,ymax=V1+V2))+
  geom_line()+
  geom_point(size=3, shape=21, fill="white")+
  xlab("Sample Sizes")+
  ylab("Marginal Effect for x2")+
  theme_bw()

p4=p4+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.background=element_blank())

# require(gridExtra)
pdf("LogVsProb.pdf",17,10)
grid.arrange(p1,p3,p2,p4)
dev.off()



