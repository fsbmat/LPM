# Data generation

my.simulation=function(n,b0,b1,cond)
{
#   source("plotReg.R")
  #   iterating sigma
  if (cond %in% c(2,4)){sd=1}else sd=0.5
  set.seed(1)
  large.data=data.frame(eps=rnorm(n,mean=0,sd=sd))
  set.seed(5)
  hold.data=data.frame(eps=rnorm(500,mean=0,sd=sd))
# generating X
  if (cond %in% c(3,4)) 
    {
    set.seed(2)
    large.data$X=rnorm(n,0.5, 1/sqrt(12))
    set.seed(5)
    hold.data$X=rnorm(500,0.5, 1/sqrt(12))
    set.seed(3)
    large.data$X1=rnorm(n,0.5, 1/sqrt(12))
  }else 
    {
      set.seed(2)
      large.data$X=runif(n,0,1)
      set.seed(5)
      hold.data$X=runif(500,0,1)
      set.seed(3)
      large.data$X1=runif(n,0,1)
    }
  
  # generating Y  
  b2=1
  large.data$Y=with(large.data,b0+b1*X+eps)    #+b1*X+b2*X1
  hold.data$Y=with(hold.data,b0+b1*X+eps)
#   Median Split
  large.data$Y.binary=with(large.data,ifelse(Y>=median(Y),1,-1))
  hold.data$Y.binary=with(hold.data,ifelse(Y>=median(Y),1,-1))
  table(large.data$Y.binary)
  
  
  # sample.data creation
  set.seed(2)
    sample.data.index=sample.int(dim(large.data)[1], 500, replace = FALSE, prob = NULL)
  sample.data=large.data[sample.data.index,]
  dim(sample.data)
  # tiny data
  set.seed(3)
  tiny.data.index=sample.int(dim(large.data)[1], 50, replace = FALSE, prob = NULL)
  tiny.data=large.data[tiny.data.index,]  #47,3
  dim(tiny.data)
  
  #
#   large.data$X.m=with(large.data,X-mean(X))
#   sample.data$X.m=with(sample.data,X-mean(X))
#   tiny.data$X.m=with(tiny.data,X-mean(X))
  
  #
  # Regression models
  m1=as.formula(Y~X)
  m2=as.formula(Y.binary~X)
  m3=as.formula(Y.binary~X-1)
  
#Linear Reg
  quad.reg=function(plot.data,title,label,cond,data.ind)
  {
    if(missing(title)) title=""
    if(missing(label)) label=""
    xl=c(-2,2,-2,2)     #median(large.data$X)
    yl=c(3.5,3,-3,-3)
  
    #LPM-1
    my.axes=T
    if(data.ind==3)
      { x.lab="s"
        x.roc.lim=c(0,0.2,0.4,0.6,0.8,1)
    }else 
      { x.lab="n"
        x.roc.lim=c(0,1)
      }
    
    if(cond==1)
    { y.lab="s"
      y.roc.lim=c(0,0.2,0.4,0.6,0.8,1)
    }else 
    { y.lab="n"
      y.roc.lim=c(0,1)
    }
    plot(large.data$X,large.data$Y,xlab="LPM",ylab="",main="",type="n",cex.axis=0.8,ylim=c(-4,4),xlim=c(-4,4), axes = my.axes,xaxt=x.lab,yaxt=y.lab)
    lpm.model=lm(m2,data=plot.data)
    coef1=lpm.model$coefficients
      #####
      lpm.pred=predict(lm(m2,data=plot.data),hold.data)
      logit.pred=predict(glm(m2,family=binomial(link="logit"),data=plot.data),hold.data,type="response")
      probit.pred=predict(glm(m2,family=binomial(link="probit"),data=plot.data),hold.data,type="response")
      #####
    sd1=round(summary(lm(m2,data=plot.data))$coefficients[,2],3)
    abline(coef1,col="black")
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
      if(sum(dim(conf)==c(2,2))==0)
      {
        return(paste("$",lab,"(",round(100*sum(conf[1,1]+conf[2,2])/sum(conf),2),"\\%)$",sep=""))
      }else
        return(paste("$",lab,"(",round(100*conf[1,1]/sum(conf),2),"\\%)$",sep=""))
      }
    
    lpm.roc=roc.legend(xtabs(~lpm.pred1+hold.data$Y.binary),"LPM")
    #   lpm.lab=paste("LPM(",round(100*sum(lpm.conf[1,1]+lpm.conf[2,2])/sum(lpm.conf),2),"%)",sep="")
    plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="black",xaxis.at=x.roc.lim,y.axis.at=y.roc.lim)
    plot(performance(prediction(logit.pred,hold.data$Y.binary),"tpr","fpr"),col="red",add=T,lty=2)
    plot(performance(prediction(probit.pred,hold.data$Y.binary),"tpr","fpr"),col="brown",add=T,lty=3,xaxt=x.lab)
    plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="darkgreen",add=T,lty=4,xaxt=x.lab)
    
    logit.roc=roc.legend(xtabs(~logit.pred1+hold.data$Y.binary),"Logit")
    probit.roc=roc.legend(xtabs(~probit.pred1+hold.data$Y.binary),"Probit")
    wls.roc=roc.legend(xtabs(~lpm.pred.wls1+hold.data$Y.binary),"WLS")
    legend.fill=c("black","red","brown","darkgreen")
    legend(.6,.4,c(lpm.roc,logit.roc,probit.roc,wls.roc),fill=legend.fill,col=legend.fill,cex=0.8)
    
    
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
    boxplot(lpm.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,names=lab1,cex.axis=0.4,ylim=c(0,1))
    boxplot(logit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.32,names=lab2,cex.axis=0.4)
    boxplot(probit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.16,names=lab3,cex.axis=0.4)
    boxplot(lpm.pred.wls~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2+.16,names=lab4,cex.axis=0.4)
    if(data.ind==3)
    {
      mtext(paste("$y=0 \\hspace{4cm} y=1$"),1, cex = 0.5, line = 1.5)
    }
            
    dev.set(dev.prev())
    dev.set(dev.prev())
    
    #####
    abline(coef2,col="red",lty=3)
    my.plot.equation(coef2,sd2,"b.wls",xl[2],yl[2],"red")  
    
    #Zero-Intercept
    coef3=lm(m3,data=plot.data)$coefficients
    sd3=round(summary(lm(m3,data=plot.data))$coefficients[,2],3)
    abline(c(0,coef3),col="darkblue",lty=4)
    my.plot.equation(coef3,sd3,"b.zero",xl[3],yl[3],"darkblue")
    
    # OLS
    coef4=lm(m1,data=plot.data)$coefficients
    sd4=round(summary(lm(m1,data=plot.data))$coefficients[,2],3)
    abline(coef4,col="darkgreen")
    my.plot.equation(coef4,sd4,"ols",xl[4],yl[4],"darkgreen")
    
  }

#   my.plot.equation=function(coef,sd,labtex,x,y,col)
#   {
#     if(length(sd)==2)
#     {
#       c1=round(coef[1],3);c2=round(coef[2],3)
#       s1=round(sd[1],3);s2=round(sd[2],3)
#       text(x,y,paste("$y={",c1,"\\atop (",s1,")}+{",c2,"\\atop (",s2,")}x$"),cex=0.7,col=col)
#     }else
#     {
#       c1=round(coef[1],3)
#       s1=round(sd[1],3)
#       text(x,y,paste("$y={",c1,"\\atop (",s1,")}x$"),cex=0.7,col=col)
#     }
#   }
  
#   creating plot for each datasets
  quad.reg(plot.data=tiny.data,cond=cond,data.ind=1)
  quad.reg(plot.data=sample.data,cond=cond,data.ind=2)
  quad.reg(plot.data=large.data,cond=cond,data.ind=3)
}  

# my.simulation(n,b0,b1,1)


##################################################################################
#################Binomial#########################################################

#Linear Reg
quad.reg1=function(plot.data,hold.data,title,label,cond,data.ind)
{
  if(missing(title)) title=""
  if(missing(label)) label=""
  xl=c(-.5,.5,-.5,.5)     #median(large.data$X)
  yl=c(.85,.85,.15,.15)
  
  #LPM-1
  my.axes=T
  x.lab="s"
  plot(plot.data$X,plot.data$Y,xlab="LPM",ylab="",main="",type="n",cex.axis=0.8,ylim=c(0,1),xlim=c(-1,1), axes = my.axes,xaxt=x.lab)
  lpm.model=lm(m2,data=plot.data)
  coef1=lpm.model$coefficients
  #####
  lpm.pred=predict(lm(m2,data=plot.data),hold.data)
  logit.pred=predict(glm(m2,family=binomial(link="logit"),data=plot.data),hold.data,type="response")
  probit.pred=predict(glm(m2,family=binomial(link="probit"),data=plot.data),hold.data,type="response")
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
    if(sum(dim(conf)==c(2,2))==0)
    {
      return(paste("$",lab,"(",round(100*sum(conf[1,1]+conf[2,2])/sum(conf),2),"\\%)$",sep=""))
    }else
      return(paste("$",lab,"(",round(100*conf[1,1]/sum(conf),2),"\\%)$",sep=""))
  }
  
  lpm.roc=roc.legend(xtabs(~lpm.pred1+hold.data$Y.binary),"LPM")
  #   lpm.lab=paste("LPM(",round(100*sum(lpm.conf[1,1]+lpm.conf[2,2])/sum(lpm.conf),2),"%)",sep="")
  plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="black")
  plot(performance(prediction(logit.pred,hold.data$Y.binary),"tpr","fpr"),col="red",add=T,lty=2)
  plot(performance(prediction(probit.pred,hold.data$Y.binary),"tpr","fpr"),col="brown",add=T,lty=3,xaxt=x.lab)
  plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="darkgreen",add=T,lty=4,xaxt=x.lab)
  
  logit.roc=roc.legend(xtabs(~logit.pred1+hold.data$Y.binary),"Logit")
  probit.roc=roc.legend(xtabs(~probit.pred1+hold.data$Y.binary),"Probit")
  wls.roc=roc.legend(xtabs(~lpm.pred.wls1+hold.data$Y.binary),"WLS")
  legend.fill=c("black","red","brown","darkgreen")
  legend(.6,.4,c(lpm.roc,logit.roc,probit.roc,wls.roc),fill=legend.fill,col=legend.fill,cex=0.8)
  
  
  ##
  dev.set(dev.next())
#   if(data.ind==3)
#   {
    lab1=c("OLS","OLS")
    lab2=c("Log","Log")
    lab3=c("Pro","Pro")
    lab4=c("WLS","WLS")
#   }else
#   {
#     lab1=lab2=lab3=lab4=c("","")
#   }
  boxplot(lpm.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,names=lab1,cex.axis=0.4,ylim=c(0,1))
  boxplot(logit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.32,names=lab2,cex.axis=0.4)
  boxplot(probit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.16,names=lab3,cex.axis=0.4)
  boxplot(lpm.pred.wls~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2+.16,names=lab4,cex.axis=0.4)
#   if(data.ind==3)
#   {
    mtext(paste("$y=0 \\hspace{4cm} y=1$"),1, cex = 0.5, line = 1.5)
#   }
  
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


#################################################################################
#################Plotting equation##############################################

#   function for plotting eqautions in the plot
my.plot.equation=function(coef,sd,labtex,x,y,col)
{
  if(length(sd)==2)
  {
    c1=round(coef[1],3);c2=round(coef[2],3)
    s1=round(sd[1],3);s2=round(sd[2],3)
    text(x,y,paste("$y_{",labtex,"}={",c1,"\\atop (",s1,")}+{",c2,"\\atop (",s2,")}x$"),col=col,cex=.9)
  }else
  {
    c1=round(coef[1],3)
    s1=round(sd[1],3)
    text(x,y,paste("$y_{",labtex,"}={",c1,"\\atop (",s1,")}x$"),col=col,cex=.9)
  }
}
