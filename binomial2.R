#Binomial simulation

binomCase2=function(os.flag,us.flag,case,binary.flag)
{
  n=2e6  #6
  if(case==1)
  {
    b0=.5
    b1=1
    b2=.01
    set.seed(1)
    large.data=data.frame(eps=rnorm(n,mean=0,sd=.01))
    set.seed(2)
    hold.data=data.frame(eps=rnorm(500,mean=0,sd=.01))
  }else
  {
    b0=0.5
    b1=0.1
    b2=0.45
    set.seed(1)
    large.data=data.frame(eps=rnorm(n,mean=0,sd=.1))
    set.seed(2)
    hold.data=data.frame(eps=rnorm(500,mean=0,sd=.1))
  }
  #
  set.seed(3)
  large.data$X=runif(n,-.5,.5)
  set.seed(4)
  hold.data$X=runif(500,-.5,.5)
  #
  set.seed(4)
  large.data$X1=runif(n,-.5,.5)
  set.seed(3)
  hold.data$X1=runif(500,-.5,.5)
  #
  if(os.flag)
  {
    b1=0
    large.data$Y=with(large.data,b0+b1*X+eps)  #+b2*X1
    hold.data$Y=with(hold.data,b0+b1*X+eps)    #+b2*X1
  }else if(us.flag)
  {
    large.data$Y=with(large.data,b0+b1*X+b2*X1+eps)  #+b2*X1
    hold.data$Y=with(hold.data,b0+b1*X+b2*X1+eps)    #+b2*X1
  }else
  {
    large.data$Y=with(large.data,b0+b1*X+eps)  #+b2*X1
    hold.data$Y=with(hold.data,b0+b1*X+eps)    #+b2*X1
  }
  
  #
  if(binary.flag)
  {
    large.data$Y.binary=with(large.data,rbinom(n,1,Y))
    large.data=na.omit(large.data)
    dim(large.data)
    hold.data$Y.binary=with(hold.data,rbinom(500,1,Y))
    hold.data=na.omit(hold.data)
    dim(hold.data)
  }else
  {
    large.data$Y.binary=with(large.data,ifelse(Y>=median(Y),1,0))
    large.data=na.omit(large.data)
    dim(large.data)
    hold.data$Y.binary=with(hold.data,ifelse(Y>=median(Y),1,0))
    hold.data=na.omit(hold.data)
    dim(hold.data)
  }
  
  #  
  
  # sample.data creation
  set.seed(1)
  sample.data.index=sample.int(dim(large.data)[1], 500, replace = FALSE, prob = NULL)
  sample.data=large.data[sample.data.index,]
  # tiny data
  set.seed(2)
  tiny.data.index=sample.int(dim(large.data)[1], 50, replace = FALSE, prob = NULL)
  tiny.data=large.data[tiny.data.index,]  
  
  m1=as.formula(Y~X)
  m2=as.formula(Y.binary~X)
  # m3=as.formula(Y.binary~X-1)
  
  #Linear Reg
  quad.reg1=function(plot.data,hold.data,title,label,case,data.ind)
  {
    if(missing(title)) title=""
    if(missing(label)) label=""
    xl=c(-.5,-.5,.5,.5)     #median(large.data$X)
    yl=c(.85,.65,.15,.15)
    
    #LPM-1
    my.axes=T
    if(case==1)
    {
      y.lab="s"
      y.roc.lim=c(0,.2,.4,.6,.8,1)
    }else
    {
      y.lab="n"
      y.roc.lim=c(0,1)
    }
    if(data.ind==3)
    {
      x.lab="s"
      x.roc.lim=c(0,.2,.4,.6,.8,1)
      
    }else
    {
      x.lab="n"
      x.roc.lim=c(0,1)
    }
    #
    plot(plot.data$X,plot.data$Y,xlab="LPM",ylab="",main="",type="n",cex.axis=0.5,ylim=c(0,1),xlim=c(-1,1), axes = my.axes,xaxt=x.lab,yaxt=y.lab)
    lpm.model=lm(m2,data=plot.data)
    coef1=lpm.model$coefficients
    #####
    require(brglm)
    require(texreg)
    lpm.pred=predict(lm(m2,data=plot.data),hold.data)
    logit.pred=predict(glm(m2,family=binomial(link="logit"),data=plot.data),hold.data,type="response")
    probit.pred=predict(glm(m2,family=binomial(link="probit"),data=plot.data),hold.data,type="response")
    if(data.ind!=3)
    {
      a11=glm(m2,family=binomial(link="logit"),data=plot.data)
      a12=glm(m2,family=binomial(link="probit"),data=plot.data)
      a21=brglm(m2,family=binomial(link="logit"),data=plot.data)
      a22=brglm(m2,family=binomial(link="probit"),data=plot.data)
      cat(capture.output(texreg(list(a11,a21,a12,a22))),file="D:\\Prof.Galit\\LPM\\PaperResults\\PaperLPM\\latex1.txt",sep="\n",append=T)
      #     write(texreg(list(a11,a21,a12,a22)),file="D:\\Prof.Galit\\LPM\\PaperResults\\PaperLPM\\latex.txt",sep="\n")
    }
    
	#####
    sd1=round(summary(lm(m2,data=plot.data))$coefficients[,2],3)
    abline(coef1,col="black",lty=4)
    my.plot.equation(coef1,sd1,"b.ols",xl[1],yl[1],"black")
    
    #WOLS
    t=lm(m2,data=plot.data)$fitted
    plot.data$weight.lpm=1/sqrt(t*(1-t))
    coef2=lm(m2,data=plot.data,weights=weight.lpm)$coefficients
    sd2=round(summary(lm(m2,data=plot.data,weights=weight.lpm))$coefficients[,2],3)
    #####
    lpm.pred.wls=predict(lm(m2,data=plot.data,weights=weight.lpm),hold.data)
    ##
    ## ROC
    lpm.pred1=ifelse(lpm.pred>0.5,1,0)
    logit.pred1=ifelse(logit.pred>0.5,1,0)
    probit.pred1=ifelse(probit.pred>0.5,1,0)
    lpm.pred.wls1=ifelse(lpm.pred.wls>0.5,1,0)
    ##
    dev.set(dev.next())
    require(ROCR)
    roc.legend=function(conf,lab)
    { 
      #     if(sum(dim(conf)==c(2,2))==0)
      #     {
      #       return(paste("$",lab,"(",round(100*sum(conf[1,1]+conf[2,2])/sum(conf),2),"\\%)$",sep=""))
      #     }else
      #       return(paste("$",lab,"(",round(100*conf[1,1]/sum(conf),2),"\\%)$",sep=""))
      return(paste("$",lab,"$"))
    }    
    lpm.roc=roc.legend(xtabs(~lpm.pred1+hold.data$Y.binary),"LPM")
    #   lpm.lab=paste("LPM(",round(100*sum(lpm.conf[1,1]+lpm.conf[2,2])/sum(lpm.conf),2),"%)",sep="")
    plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="black",xaxis.at=x.roc.lim,yaxis.at=y.roc.lim,xaxis.cex.axis=0.5,yaxis.cex.axis=0.5)
    plot(performance(prediction(logit.pred,hold.data$Y.binary),"tpr","fpr"),col="red",add=T,lty=2)
    plot(performance(prediction(probit.pred,hold.data$Y.binary),"tpr","fpr"),col="brown",add=T,lty=3,xaxt=x.lab)
    plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="darkgreen",add=T,lty=4,xaxt=x.lab)
    
    logit.roc=roc.legend(xtabs(~logit.pred1+hold.data$Y.binary),"Logit")
    probit.roc=roc.legend(xtabs(~probit.pred1+hold.data$Y.binary),"Probit")
    wls.roc=roc.legend(xtabs(~lpm.pred.wls1+hold.data$Y.binary),"WLS")
    legend.fill=c("black","red","brown","darkgreen")
    legend(.6,.4,c(lpm.roc,logit.roc,probit.roc,wls.roc),fill=legend.fill,col=legend.fill,cex=0.4)
    
    
    ##
    dev.set(dev.next())
      if(data.ind==3)
      {
        lab1=c("OLS","OLS")
        lab2=c("Log","Log")
        lab3=c("Pro","Pro")
        lab4=c("WLS","WLS")
      }else
      {
        lab1=lab2=lab3=lab4=c("","")
      }
    if(binary.flag) ylim=c(0,1)
    else ylim=c(-1,2)
    boxplot(lpm.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,names=lab1,cex.axis=0.5,ylim=ylim,yaxt=y.lab)
    boxplot(logit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.32,names=lab2,cex.axis=0.5,border="blue")
    boxplot(probit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.16,names=lab3,cex.axis=0.5,border="darkblue")
    boxplot(lpm.pred.wls~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2+.16,names=lab4,cex.axis=0.5,border="red")
      if(data.ind==3)
      {
        mtext(paste("$y=0 \\hspace{3cm} y=1$"),1, cex = 0.5, line = 1.5)
      }
    
    dev.set(dev.prev())
    dev.set(dev.prev())
    
    #####
    abline(coef2,col="red",lty=3)
    my.plot.equation(coef2,sd2,"b.wls",xl[2],yl[2],"red")  
    
    #   #Zero-Intercept
    #   coef3=lm(m3,data=plot.data)$coefficients
    #   sd3=round(summary(lm(m3,data=plot.data))$coefficients[,2],3)
    #   abline(c(0,coef3),col="darkblue",lty=4)
    #   my.plot.equation(coef3,sd3,"b.zero",xl[3],yl[3],"darkblue")
    
    # OLS
    coef4=lm(m1,data=plot.data)$coefficients
    sd4=round(summary(lm(m1,data=plot.data))$coefficients[,2],3)
    abline(coef4,col="darkgreen")
    my.plot.equation(coef4,sd4,"ols",xl[4],yl[4],"darkgreen")
  }
  #   creating plot for each datasets
  quad.reg1(plot.data=tiny.data,hold.data,case=case,data.ind=1)
  quad.reg1(plot.data=sample.data,hold.data,case=case,data.ind=2)
  quad.reg1(plot.data=large.data,hold.data,case=case,data.ind=3)
}


#################################################################################
#################Plotting equation##############################################

#   function for plotting eqautions in the plot
my.plot.equation=function(coef,sd,labtex,x,y,col)
{
  if(length(sd)==2)
  {
    c1=round(coef[1],3);c2=round(coef[2],3)
    s1=round(sd[1],3);s2=round(sd[2],3)
    text(x,y,paste("$y_{",labtex,"}={",c1,"\\atop (",s1,")}+{",c2,"\\atop (",s2,")}x$"),col=col,cex=.7)
  }else if(length(sd)==3)
  {
    c1=round(coef[1],3);c2=round(coef[2],3);c3=round(coef[3],3)
    s1=round(sd[1],3);s2=round(sd[2],3);s3=round(sd[3],3)
    text(x,y,paste("$y_{",labtex,"}={",c1,"\\atop (",s1,")}+{",c2,"\\atop (",s2,")}x+{",c3,"\\atop (",s3,")}z$"),col=col,cex=.7)
  }
}

  


binomCase3=function(os.flag,us.flag,case,binary.flag)
{
  n=2e6  #6
  if(case==1)
{
  b0=.5
  b1=1
  b2=.01
  set.seed(1)
  large.data=data.frame(eps=rnorm(n,mean=0,sd=.01))
  set.seed(2)
  hold.data=data.frame(eps=rnorm(500,mean=0,sd=.01))
}else
{
  b0=0.5
  b1=0.1
  b2=0.45
  set.seed(1)
  large.data=data.frame(eps=rnorm(n,mean=0,sd=.1))
  set.seed(2)
  hold.data=data.frame(eps=rnorm(500,mean=0,sd=.1))
}
#
set.seed(3)
large.data$X=runif(n,-.5,.5)
set.seed(4)
hold.data$X=runif(500,-.5,.5)
#
set.seed(4)
large.data$X1=runif(n,-.5,.5)
set.seed(3)
hold.data$X1=runif(500,-.5,.5)
#
large.data$Y=with(large.data,b0+b1*X+eps)  #+b2*X1
hold.data$Y=with(hold.data,b0+b1*X+eps)    #+b2*X1

#
if(binary.flag)
{
  large.data$Y.binary=with(large.data,rbinom(n,1,Y))
  large.data=na.omit(large.data)
  dim(large.data)
  hold.data$Y.binary=with(hold.data,rbinom(500,1,Y))
  hold.data=na.omit(hold.data)
  dim(hold.data)
}else
{
  large.data$Y.binary=with(large.data,ifelse(Y>=median(Y),1,0))
  large.data=na.omit(large.data)
  dim(large.data)
  hold.data$Y.binary=with(hold.data,ifelse(Y>=median(Y),1,0))
  hold.data=na.omit(hold.data)
  dim(hold.data)
}

#  

# sample.data creation
set.seed(1)
sample.data.index=sample.int(dim(large.data)[1], 500, replace = FALSE, prob = NULL)
sample.data=large.data[sample.data.index,]
# tiny data
set.seed(2)
tiny.data.index=sample.int(dim(large.data)[1], 50, replace = FALSE, prob = NULL)
tiny.data=large.data[tiny.data.index,]  

m1=as.formula(Y~X+X1)
m2=as.formula(Y.binary~X+X1)

  quad.reg2=function(plot.data,hold.data,title,label,case,data.ind)
{
  if(missing(title)) title=""
  if(missing(label)) label=""
  xl=c(2,2,2,2)     #median(large.data$X)
  if(binary.flag) yl=c(1,.50,.50,0)
  else yl=c(2,1.5,.50,0)
  
  #LPM-1
  my.axes=T
  if(case==1)
  {
    y.lab="s"
    y.roc.lim=c(0,.2,.4,.6,.8,1)
  }else
  {
    y.lab="n"
    y.roc.lim=c(0,1)
  }
  if(data.ind==3)
  {
    x.lab="s"
    x.roc.lim=c(0,.2,.4,.6,.8,1)
    
  }else
  {
    x.lab="n"
    x.roc.lim=c(0,1)
  }
#   require(Zelig)
  #
  lpm.pred=predict(lm(m2,data=plot.data),hold.data)
  logit.pred=predict(glm(m2,family=binomial(link="logit"),data=plot.data),hold.data,type="response")
  probit.pred=predict(glm(m2,family=binomial(link="probit"),data=plot.data),hold.data,type="response")
  #####
  coef1=lm(m2,data=plot.data)$coefficients
  sd1=summary(lm(m2,data=plot.data))$coefficients[,2]
  x=1:length(coef1)
  if(binary.flag) ylim=c(-0.25,1.25)
  else ylim=c(-0.25,2.25)
  plot(x,coef1,type="n",cex.axis=0.5,ylim=ylim, axes = my.axes,xaxt="n",yaxt=y.lab,xlim=c(0.5,3.5))
  if(data.ind==3) axis(1,at=c(1,2,3),labels=c(paste("$\\beta_0$"),paste("$\\beta_1$"),paste("$\\beta_2$")),cex.axis=0.5)
  require(scales)
 my.errorbar= function(x,coef,sd,colour,type)
 {
   points(x,coef,col=colour,cex=0.3)
#    lines(coef,col=colour,lty=type)
   segments(x, coef-sd,x, coef+sd,col=colour)
   epsilon = 0.02
   segments(x-epsilon,coef-sd,x+epsilon,coef-sd,col=colour)
   segments(x-epsilon,coef+sd,x+epsilon,coef+sd,col=colour)
 }
  my.errorbar(x,coef1,sd1,"black",4)
  my.plot.equation(coef1,sd1,"b.ols",xl[1],yl[1],alpha("black",0.1))
  
  #WOLS
  t=lm(m2,data=plot.data)$fitted
  plot.data$weight.lpm=1/sqrt(t*(1-t))

  coef2=lm(m2,data=plot.data,weights=weight.lpm)$coefficients
  sd2=summary(lm(m2,data=plot.data,weights=weight.lpm))$coefficients[,2]
  my.errorbar(x+0.1,coef2,sd2,"red",3)
  my.plot.equation(coef2,sd2,"b.wls",xl[2],yl[2],alpha("red",0.1))  
  #####
  lpm.pred.wls=predict(lm(m2,data=plot.data,weights=weight.lpm),hold.data)
  ##
  ## ROC
  lpm.pred1=ifelse(lpm.pred>0.5,1,0)
  logit.pred1=ifelse(logit.pred>0.5,1,0)
  probit.pred1=ifelse(probit.pred>0.5,1,0)
  lpm.pred.wls1=ifelse(lpm.pred.wls>0.5,1,0)
  ##
  dev.set(dev.next())
  require(ROCR)
  roc.legend=function(conf,lab)
  { 
#     if(sum(dim(conf)==c(2,2))==0)
#     {
#       return(paste("$",lab,"(",round(100*sum(conf[1,1]+conf[2,2])/sum(conf),2),"\\%)$",sep=""))
#     }else
#       return(paste("$",lab,"(",round(100*conf[1,1]/sum(conf),2),"\\%)$",sep=""))
    return(paste("$",lab,"$"))
  }
  
  lpm.roc=roc.legend(xtabs(~lpm.pred1+hold.data$Y.binary),"LPM")
  #   lpm.lab=paste("LPM(",round(100*sum(lpm.conf[1,1]+lpm.conf[2,2])/sum(lpm.conf),2),"%)",sep="")
  plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="black",xaxis.at=x.roc.lim,yaxis.at=y.roc.lim,xaxis.cex.axis=0.5,yaxis.cex.axis=0.5)
  plot(performance(prediction(logit.pred,hold.data$Y.binary),"tpr","fpr"),col="red",add=T,lty=2)
  plot(performance(prediction(probit.pred,hold.data$Y.binary),"tpr","fpr"),col="brown",add=T,lty=3,xaxt=x.lab)
  plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="darkgreen",add=T,lty=4,xaxt=x.lab)
  
  logit.roc=roc.legend(xtabs(~logit.pred1+hold.data$Y.binary),"Logit")
  probit.roc=roc.legend(xtabs(~probit.pred1+hold.data$Y.binary),"Probit")
  wls.roc=roc.legend(xtabs(~lpm.pred.wls1+hold.data$Y.binary),"WLS")
  legend.fill=c("black","red","brown","darkgreen")
  legend(.6,.4,c(lpm.roc,logit.roc,probit.roc,wls.roc),fill=legend.fill,col=legend.fill,cex=0.4)
  
  
  ##
  dev.set(dev.next())
  if(data.ind==3)
  {
    lab1=c("OLS","OLS")
    lab2=c("Log","Log")
    lab3=c("Pro","Pro")
    lab4=c("WLS","WLS")
  }else
  {
    lab1=lab2=lab3=lab4=c("","")
  }
  if(binary.flag) ylim=c(0,1)
  else ylim=c(-1,2)
  boxplot(lpm.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,names=lab1,cex.axis=0.5,ylim=ylim,yaxt=y.lab)
  boxplot(logit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.32,names=lab2,cex.axis=0.5,border="blue")
  boxplot(probit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.16,names=lab3,cex.axis=0.5,border="darkblue")
  boxplot(lpm.pred.wls~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2+.16,names=lab4,cex.axis=0.5,border="red")
  if(data.ind==3)
  {
    mtext(paste("$y=0 \\hspace{3cm} y=1$"),1, cex = 0.5, line = 1.5)
  }
  
  dev.set(dev.prev())
  dev.set(dev.prev())
  
  #####
  
  
  #   #Zero-Intercept
  #   coef3=lm(m3,data=plot.data)$coefficients
  #   sd3=round(summary(lm(m3,data=plot.data))$coefficients[,2],3)
  #   abline(c(0,coef3),col="darkblue",lty=4)
  #   my.plot.equation(coef3,sd3,"b.zero",xl[3],yl[3],"darkblue")
  
  # OLS
  ols.model=lm(m1,data=plot.data)
  coef4=lm(m1,data=plot.data)$coefficients
  sd4=summary(lm(m1,data=plot.data))$coefficients[,2]
  my.errorbar(x-0.1,coef4,sd4,"darkgreen",1)
  my.plot.equation(coef4,sd4,"ols",xl[4],yl[4],alpha("darkgreen",0.1))
 }
  
#   creating plot for each datasets
  quad.reg2(plot.data=tiny.data,hold.data,case=case,data.ind=1)
  quad.reg2(plot.data=sample.data,hold.data,case=case,data.ind=2)
  quad.reg2(plot.data=large.data,hold.data,case=case,data.ind=3)

}

  
