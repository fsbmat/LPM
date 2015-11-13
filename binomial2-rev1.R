#Binomial simulation

binomCase2=function(os.flag,us.flag,case,binary.flag,error.dist)
{
  n=2e6  #6
  #
  
  error.sim=function(n,error.dist,case)
  {
    if(error.dist==1)
    {
      if (case==1)
      {
        set.seed(n)
        x=rnorm(n,0,.1) #runif(n,-2,2)
        
      }else
      {
        set.seed(n)
        x=rnorm(n,0,1) #runif(n,-2,2)
        
      }
        
    }
    if(error.dist==2)
    {
      if(case==1)
      {
        set.seed(n)
        x=rlogis(n,0,sqrt(3)/(10*pi)) #runif(n,-2,2)
        
      }else
      {
        set.seed(n)
        x=rlogis(n,0,sqrt(3)/pi) #runif(n,-2,2)
      }
            
    }
    if(error.dist==3)
    {
      if(case==1)
      {
        set.seed(n)
        x=runif(n,min=-sqrt(3)/10,max=sqrt(3)/10) #runif(n,-2,2)
        }else
        {
          set.seed(n)
          x=runif(n,min=-sqrt(3),max=sqrt(3)) #runif(n,-2,2)
        }
            
    }
    
    return(x)
  }
  
  ###
  # epsilon
  large.data=data.frame(eps=error.sim(n,error.dist,case))
  hold.data=data.frame(eps=error.sim(500,error.dist,case))
#     
  if(case==1)
  {
    
    b0=0.5
    b1=1
    b2=-1
    #
    set.seed(3)
    large.data$X=rnorm(n,0,1)   #rnorm(n,0,1.5)
    set.seed(4)
    hold.data$X=rnorm(500,0,1)  #rnorm(500,0,1.5)
    #
    set.seed(4)
    large.data$X1=rnorm(n,0,1)
    set.seed(3)
    hold.data$X1=rnorm(500,0,1)
    
#     set.seed(1)
#     large.data=data.frame(eps=rnorm(n,mean=0,sd=1))
#     set.seed(2)
#     hold.data=data.frame(eps=rnorm(500,mean=0,sd=1))
  }else
  {
    
    b0=0.5
    b1=.1
    b2=-.1
    #
    set.seed(3)
    large.data$X=rnorm(n,0,1)  #rnorm(n,0,.5)
    set.seed(4)
    hold.data$X=rnorm(500,0,1)   #rnorm(500,0,.5)
    #
    set.seed(4)
    large.data$X1=rnorm(n,0,1)
    set.seed(3)
    hold.data$X1=rnorm(500,0,1)
    
#     set.seed(1)
#     large.data=data.frame(eps=rnorm(n,mean=0,sd=1))
#     set.seed(2)
#     hold.data=data.frame(eps=rnorm(500,mean=0,sd=1))
  }
  
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
#   #
#   set.seed(3)
#   large.data$X=runif(n,-.5,.5)
#   set.seed(4)
#   hold.data$X=runif(500,-.5,.5)
#   #
#   set.seed(4)
#   large.data$X1=runif(n,-.5,.5)
#   set.seed(3)
#   hold.data$X1=runif(500,-.5,.5)
#   #
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
    large.data$Y.binary=with(large.data,ifelse(Y>= 0.5,1,0)) #median(Y)
    large.data=na.omit(large.data)
    dim(large.data)
    hold.data$Y.binary=with(hold.data,ifelse(Y>=0.5,1,0))
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
  
#   test.ols=lm(m1,large.data)
#   test.lpm=lm(m2,large.data)
#   m=model.matrix(test.lpm)
#   bias= solve(t(m)%*%m)%*%t(m)%*%rep(1.5,dim(large.data)[1])
#     # b1=(m%*%c(0.5,1)+rep(1.5,dim(large.data)[1]))/4
#     # solve(t(m)%*%m)%*%t(m)%*%b1
#     (coef(test.ols)+bias)/4
#   
#   ## Normal errors
#   .798*sd(large.data$Y.binary)/sd(large.data$Y)
#   ### Logistic 
#   .7624619*sd(large.data$Y.binary)/sd(large.data$Y)
  ##
  #Linear Reg
  quad.reg1=function(plot.data,hold.data,title,label,case,data.ind)
  {
    if(missing(title)) title=""
    if(missing(label)) label=""
    xl=c(-1.5,-1.5,1.5,1.5)     #median(large.data$X)
    yl=c(2,-2,-2,-2)
    
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
    plot(plot.data$X,plot.data$Y,xlab="LPM",ylab="",main="",type="n",cex.axis=0.5,ylim=c(-3,3),xlim=c(-3,3), axes = my.axes,xaxt=x.lab,yaxt=y.lab)
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
    abline(coef1,col="black",lty=1)
    my.plot.equation(coef1,sd1,"b.ols",xl[1],yl[1],"black")
    
    
    ### bias correction
    if(error.dist==1)
    {
      ## Normal errors
      bias=.798*sd(plot.data$Y.binary)/sd(plot.data$eps)
      coef3=coef1
      coef3[2:length(coef3)]=coef1[2:length(coef3)]/bias
    }
    if(error.dist==2)
    {
      ## Logistic 
      bias=.762*sd(plot.data$Y.binary)/sd(plot.data$eps)
      coef3=coef1
      coef3[2:length(coef3)]=coef1[2:length(coef3)]/bias
    }
    if(error.dist==3)
    {
#       if(case==1) s=.1
#       else s=1
#       bias=sqrt(3)/2*sd(plot.data$Y.binary)/sd(plot.data$Y)
#       coef3=coef1
#       coef3[2:length(coef3)]=coef1[2:length(coef3)]/bias
        m=model.matrix(lpm.model)
      if(case==1){
        a=-sqrt(3)/10;b=sqrt(3)/10
      }else
        {
        a=-sqrt(3);b=sqrt(3)
        }
        flag=which(a<=0.5-(plot.data$Y-plot.data$eps)& 0.5-(plot.data$Y-plot.data$eps)<b)
        m1=m[flag,]
        bias= solve(t(m1)%*%m1)%*%t(m1)%*%rep((b-0.5)/(b-a),length(flag))
        coef3=coef(lm(m2,plot.data[flag,]))
        coef3= (b-a)*coef3-bias
#         
    }
    
    #####
        abline(coef3,lty=3)
        my.plot.equation(coef3,sd1,"b.cor",xl[2],yl[2],"black")  
    #   
    
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
    roc.legend=function(auc,lab)
    { 
      #     if(sum(dim(conf)==c(2,2))==0)
      #     {
      #       return(paste("$",lab,"(",round(100*sum(conf[1,1]+conf[2,2])/sum(conf),2),"\\%)$",sep=""))
      #     }else
      #       return(paste("$",lab,"(",round(100*conf[1,1]/sum(conf),2),"\\%)$",sep=""))
      return(paste("$",lab,"=",round(auc,2),"$"))
    }    
#     lpm.roc=roc.legend(xtabs(~lpm.pred1+hold.data$Y.binary),"LPM")
#     #   lpm.lab=paste("LPM(",round(100*sum(lpm.conf[1,1]+lpm.conf[2,2])/sum(lpm.conf),2),"%)",sep="")
    plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="black",xaxis.at=x.roc.lim,yaxis.at=y.roc.lim,xaxis.cex.axis=0.5,yaxis.cex.axis=0.5)
    lpm.auc=as.numeric(performance(prediction(lpm.pred,hold.data$Y.binary),"auc")@y.values)
    plot(performance(prediction(logit.pred,hold.data$Y.binary),"tpr","fpr"),col="red",add=T,lty=2)
    logit.auc=as.numeric(performance(prediction(logit.pred,hold.data$Y.binary),"auc")@y.values)
    plot(performance(prediction(probit.pred,hold.data$Y.binary),"tpr","fpr"),col="brown",add=T,lty=3,xaxt=x.lab)
    probit.auc=as.numeric(performance(prediction(probit.pred,hold.data$Y.binary),"auc")@y.values)
    plot(performance(prediction(lpm.pred.wls,hold.data$Y.binary),"tpr","fpr"),col="darkgreen",add=T,lty=4,xaxt=x.lab)
    wls.auc=as.numeric(performance(prediction(lpm.pred.wls,hold.data$Y.binary),"auc")@y.values)
    
    lpm.roc=roc.legend(lpm.auc,"LPM")
    logit.roc=roc.legend(logit.auc,"Logit")
    probit.roc=roc.legend(probit.auc,"Probit")
    wls.roc=roc.legend(wls.auc,"WLS")
    legend.fill=c("black","red","brown","darkgreen")
    legend(.6,.6,c(lpm.roc,logit.roc,probit.roc,wls.roc),fill=legend.fill,
           col=legend.fill,cex=0.5,title="AUC")
    
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
#     abline(coef2,col="red",lty=3)
#     my.plot.equation(coef2,sd2,"b.wls",xl[2],yl[2],"red")  
#   
    
    #   #Zero-Intercept
    #   coef3=lm(m3,data=plot.data)$coefficients
    #   sd3=round(summary(lm(m3,data=plot.data))$coefficients[,2],3)
    #   abline(c(0,coef3),col="darkblue",lty=4)
    #   my.plot.equation(coef3,sd3,"b.zero",xl[3],yl[3],"darkblue")
    
    # OLS
    coef4=lm(m1,data=plot.data)$coefficients
    sd4=round(summary(lm(m1,data=plot.data))$coefficients[,2],3)
    abline(coef4,col="gray")
    my.plot.equation(coef4,sd4,"ols",xl[4],yl[4],"black")
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
    c1=round(coef[1],4);c2=round(coef[2],4)
    s1=round(sd[1],4);s2=round(sd[2],4)
    text(x,y,paste("$y_{",labtex,"}={",c1,"\\atop (",s1,")}+{",c2,"\\atop (",s2,")}x$"),col=col,cex=.7)
  }else if(length(sd)==3)
  {
    c1=round(coef[1],4);c2=round(coef[2],4);c3=round(coef[3],4)
    s1=round(sd[1],4);s2=round(sd[2],4);s3=round(sd[3],4)
    text(x,y,paste("$y_{",labtex,"}={",c1,"\\atop (",s1,")}+{",c2,"\\atop (",s2,")}x+{",c3,"\\atop (",s3,")}z$"),col=col,cex=.7)
  }
}




binomCase3=function(os.flag,us.flag,case,binary.flag,error.dist)
{
  n=2e6  #6
  #
  if(error.dist==1)
  {
    set.seed(1)
    large.data=data.frame(eps=rnorm(n,0,1)) #runif(n,-2,2)
    set.seed(2)
    hold.data=data.frame(eps=rnorm(500,0,1))  #runif(500,-2,2)
    
  }
  if(error.dist==2)
  {
    set.seed(1)
    large.data=data.frame(eps=rlogis(n,0,sqrt(3)/pi)) #runif(n,-2,2)
    set.seed(2)
    hold.data=data.frame(eps=rlogis(500,0,sqrt(3)/pi))  #runif(500,-2,2)
  }
  if(error.dist==3)
  {
    set.seed(1)
    large.data=data.frame(eps=runif(n,-2,2)) #runif(n,-2,2)
    set.seed(2)
    hold.data=data.frame(eps=runif(500,-2,2))  #runif(500,-2,2)
    
  }
  
  if(case==1)
  {
    b0=0.5
    b1=1
    b2=-1
    #
    set.seed(3)
    large.data$X=rnorm(n,0,1.5)
    set.seed(4)
    hold.data$X=rnorm(500,0,1.5)
    #
    set.seed(4)
    large.data$X1=rnorm(n,0,1.5)
    set.seed(3)
    hold.data$X1=rnorm(500,0,1.5)
    
    #     set.seed(1)
    #     large.data=data.frame(eps=rnorm(n,mean=0,sd=1))
    #     set.seed(2)
    #     hold.data=data.frame(eps=rnorm(500,mean=0,sd=1))
  }else
  {
    b0=0.5
    b1=1
    b2=-1
    #
    set.seed(3)
    large.data$X=rnorm(n,0,.5)
    set.seed(4)
    hold.data$X=rnorm(500,0,.5)
    #
    set.seed(4)
    large.data$X1=rnorm(n,0,.5)
    set.seed(3)
    hold.data$X1=rnorm(500,0,.5)
    
    #     set.seed(1)
    #     large.data=data.frame(eps=rnorm(n,mean=0,sd=1))
    #     set.seed(2)
    #     hold.data=data.frame(eps=rnorm(500,mean=0,sd=1))
  }
  
  #   #
  #   set.seed(3)
  #   large.data$X=runif(n,-.5,.5)
  #   set.seed(4)
  #   hold.data$X=runif(500,-.5,.5)
  #   #
  #   set.seed(4)
  #   large.data$X1=runif(n,-.5,.5)
  #   set.seed(3)
  #   hold.data$X1=runif(500,-.5,.5)
  #   #
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
    large.data$Y.binary=with(large.data,ifelse(Y>= median(Y),1,0)) #median(Y)
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
  # m3=as.formula(Y.binary~X-1)
  
  #   test.ols=lm(m1,large.data)
  #   test.lpm=lm(m2,large.data)
  #   m=model.matrix(test.lpm)
  #   bias= solve(t(m)%*%m)%*%t(m)%*%rep(1.5,dim(large.data)[1])
  #     # b1=(m%*%c(0.5,1)+rep(1.5,dim(large.data)[1]))/4
  #     # solve(t(m)%*%m)%*%t(m)%*%b1
  #     (coef(test.ols)+bias)/4
  #   
  #   ## Normal errors
  #   .798*sd(large.data$Y.binary)/sd(large.data$Y)
  #   ### Logistic 
  #   .7624619*sd(large.data$Y.binary)/sd(large.data$Y)
  
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
    # my.plot.equation(coef1,sd1,"b.ols",xl[1],yl[1],alpha("black",0.1))
    ### bias correction
    if(error.dist==1)
    {
      ## Normal errors
      bias=.798*sd(plot.data$Y.binary)/sd(plot.data$Y)
      coef3=coef1
      coef3[2:length(coef3)]=coef1[2:length(coef3)]/bias
    }
    if(error.dist==2)
    {
      ## Logistic 
      bias=.762*sd(plot.data$Y.binary)/sd(plot.data$Y)
      coef3=coef1
      coef3[2:length(coef3)]=coef1[2:length(coef3)]/bias
    }
    if(error.dist==3)
    {
      m=model.matrix(lpm.model)
      bias= solve(t(m)%*%m)%*%t(m)%*%rep(1.5,dim(plot.data)[1])
      coef3= 4*coef1-bias
      
    }
    
    my.errorbar(x+0.1,coef3,sd1,"black",4)
    #WOLS
    t=lm(m2,data=plot.data)$fitted
    plot.data$weight.lpm=1/sqrt(t*(1-t))
    
    coef2=lm(m2,data=plot.data,weights=weight.lpm)$coefficients
    sd2=summary(lm(m2,data=plot.data,weights=weight.lpm))$coefficients[,2]
    # my.errorbar(x+0.1,coef2,sd2,"black",3)
    # my.plot.equation(coef2,sd2,"b.wls",xl[2],yl[2],alpha("red",0.1))  
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
    roc.legend=function(auc,lab)
    { 
      #     if(sum(dim(conf)==c(2,2))==0)
      #     {
      #       return(paste("$",lab,"(",round(100*sum(conf[1,1]+conf[2,2])/sum(conf),2),"\\%)$",sep=""))
      #     }else
      #       return(paste("$",lab,"(",round(100*conf[1,1]/sum(conf),2),"\\%)$",sep=""))
      return(paste("$",lab,"=",round(auc,2),"$"))
    }    
    #     lpm.roc=roc.legend(xtabs(~lpm.pred1+hold.data$Y.binary),"LPM")
    #     #   lpm.lab=paste("LPM(",round(100*sum(lpm.conf[1,1]+lpm.conf[2,2])/sum(lpm.conf),2),"%)",sep="")
    plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="black",xaxis.at=x.roc.lim,yaxis.at=y.roc.lim,xaxis.cex.axis=0.5,yaxis.cex.axis=0.5)
    lpm.auc=as.numeric(performance(prediction(lpm.pred,hold.data$Y.binary),"auc")@y.values)
    plot(performance(prediction(logit.pred,hold.data$Y.binary),"tpr","fpr"),col="red",add=T,lty=2)
    logit.auc=as.numeric(performance(prediction(logit.pred,hold.data$Y.binary),"auc")@y.values)
    plot(performance(prediction(probit.pred,hold.data$Y.binary),"tpr","fpr"),col="brown",add=T,lty=3,xaxt=x.lab)
    probit.auc=as.numeric(performance(prediction(probit.pred,hold.data$Y.binary),"auc")@y.values)
    plot(performance(prediction(lpm.pred.wls,hold.data$Y.binary),"tpr","fpr"),col="darkgreen",add=T,lty=4,xaxt=x.lab)
    wls.auc=as.numeric(performance(prediction(lpm.pred.wls,hold.data$Y.binary),"auc")@y.values)
    
    lpm.roc=roc.legend(lpm.auc,"LPM")
    logit.roc=roc.legend(logit.auc,"Logit")
    probit.roc=roc.legend(probit.auc,"Probit")
    wls.roc=roc.legend(wls.auc,"WLS")
    legend.fill=c("black","red","brown","darkgreen")
    legend(.6,.6,c(lpm.roc,logit.roc,probit.roc,wls.roc),fill=legend.fill,
           col=legend.fill,cex=0.5,title="AUC")
    
    
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
    my.errorbar(x-0.1,coef4,sd4,"black",1)
    # my.plot.equation(coef4,sd4,"ols",xl[4],yl[4],alpha("darkgreen",0.1))
  }
  
  #   creating plot for each datasets
  quad.reg2(plot.data=tiny.data,hold.data,case=case,data.ind=1)
  quad.reg2(plot.data=sample.data,hold.data,case=case,data.ind=2)
  quad.reg2(plot.data=large.data,hold.data,case=case,data.ind=3)
  
}


##################################################
# comparison Unif, normal, logistic corrections
##################################################

compare=function(case,error.dist)
{
  n=2e6  #6
  #
  error.sim=function(n,error.dist,case)
  {
    if(error.dist==1)
    {
      if (case==1)
      {
        set.seed(n)
        x=rnorm(n,0,.1) #runif(n,-2,2)
        
      }else
      {
        set.seed(n)
        x=rnorm(n,0,1) #runif(n,-2,2)
        
      }
              
    }
    if(error.dist==2)
    {
      if(case==1)
      {
        set.seed(n)
        x=rlogis(n,0,sqrt(3)/(10*pi)) #runif(n,-2,2)
        
      }else
      {
        set.seed(n)
        x=rlogis(n,0,sqrt(3)/pi) #runif(n,-2,2)
        
      }
              
    }
    if(error.dist==3)
    {
      if(case==1)
      {
        set.seed(n)
        x=runif(n,-sqrt(12)/20,sqrt(12)/20) #runif(n,-2,2)
      }else
      {
        set.seed(n)
        x=runif(n,-sqrt(12)/2,sqrt(12)/2) #runif(n,-2,2)
        
      }
              
    }
    
    return(x)
  }
  
  ###
  # epsilon
  large.data=data.frame(eps=error.sim(n,error.dist,case))
  # hold.data=data.frame(eps=error.sim(500,error.dist,case))
  
  if(case==1)
  {
    
    b0=0.5
    b1=1
    b2=-1
    #
    set.seed(3)
    large.data$X=runif(n,0,1) #rnorm(n,0,1.5)
#     set.seed(4)
#     hold.data$X=rnorm(500,0,1)  #rnorm(500,0,1.5)
    #
    set.seed(4)
    large.data$X1=runif(n,0,1)
#     set.seed(3)
#     hold.data$X1=rnorm(500,0,1)
    
    #     set.seed(1)
    #     large.data=data.frame(eps=rnorm(n,mean=0,sd=1))
    #     set.seed(2)
    #     hold.data=data.frame(eps=rnorm(500,mean=0,sd=1))
  }else{
    
    b0=0.5
    b1=.1
    b2=-.1
    #
    set.seed(3)
    large.data$X=runif(n,0,1)  #rnorm(n,0,.5)
#     set.seed(4)
#     hold.data$X=rnorm(500,0,1)   #rnorm(500,0,.5)
#     #
    set.seed(4)
    large.data$X1=runif(n,0,1)
#     set.seed(3)
#     hold.data$X1=rnorm(500,0,1)
#     
    #     set.seed(1)
    #     large.data=data.frame(eps=rnorm(n,mean=0,sd=1))
    #     set.seed(2)
    #     hold.data=data.frame(eps=rnorm(500,mean=0,sd=1))
  }  #   #
  #   set.seed(3)
  #   large.data$X=runif(n,-.5,.5)
  #   set.seed(4)
  #   hold.data$X=runif(500,-.5,.5)
  #   #
  #   set.seed(4)
  #   large.data$X1=runif(n,-.5,.5)
  #   set.seed(3)
  #   hold.data$X1=runif(500,-.5,.5)
  #   #
#   if(os.flag)
#   {
#     b1=0
#     large.data$Y=with(large.data,b0+b1*X+eps)  #+b2*X1
#     # hold.data$Y=with(hold.data,b0+b1*X+eps)    #+b2*X1
#   }else if(us.flag)
#   {
#     large.data$Y=with(large.data,b0+b1*X+b2*X1+eps)  #+b2*X1
#     # hold.data$Y=with(hold.data,b0+b1*X+b2*X1+eps)    #+b2*X1
#   }else
#   {
  
    
    
    # hold.data$Y=with(hold.data,b0+b1*X+eps)    #+b2*X1
  # }
  
  #
#   if(binary.flag)
#   {
#     large.data$Y.binary=with(large.data,rbinom(n,1,Y))
#     large.data=na.omit(large.data)
#     dim(large.data)
#     hold.data$Y.binary=with(hold.data,rbinom(500,1,Y))
#     hold.data=na.omit(hold.data)
#     dim(hold.data)
#   }else
#   {
  
  large.data$Y=with(large.data,b0+b1*X+eps)  #+b2*X1
    large.data$Y.binary=with(large.data,ifelse(Y>= 0.5,1,0)) #median(Y)
    large.data=na.omit(large.data)
    dim(large.data)
#     hold.data$Y.binary=with(hold.data,ifelse(Y>=median(Y),1,0))
#     hold.data=na.omit(hold.data)
#     dim(hold.data)
#   # }
  
  #  
  
  # sample.data creation
  set.seed(1)
  sample.data.index=sample.int(dim(large.data)[1], 500, replace = FALSE, prob = NULL)
  sample.data=large.data[sample.data.index,]
  # tiny data
  set.seed(2)
  tiny.data.index=sample.int(dim(large.data)[1], 50, replace = FALSE, prob = NULL)
  tiny.data=large.data[tiny.data.index,]  
  
  
  # m3=as.formula(Y.binary~X-1)
  
  #   test.ols=lm(m1,large.data)
  #   test.lpm=lm(m2,large.data)
  #   m=model.matrix(test.lpm)
  #   bias= solve(t(m)%*%m)%*%t(m)%*%rep(1.5,dim(large.data)[1])
  #     # b1=(m%*%c(0.5,1)+rep(1.5,dim(large.data)[1]))/4
  #     # solve(t(m)%*%m)%*%t(m)%*%b1
  #     (coef(test.ols)+bias)/4
  #   
  #   ## Normal errors
  #   .798*sd(large.data$Y.binary)/sd(large.data$Y)
  #   ### Logistic 
  #   .7624619*sd(large.data$Y.binary)/sd(large.data$Y)
  ##
  #Linear Reg
  compare.reg1=function(plot.data,title,label,case,data.ind)
  {
    if(missing(title)) title=""
    if(missing(label)) label=""
    xl=c(-1.5,-1.5,1.5,1.5)     #median(large.data$X)
    yl=c(2,-2,2,-2)    
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
    m1=as.formula(Y~X)
    m2=as.formula(Y.binary~X)
    plot(plot.data$X,plot.data$Y,xlab="LPM",ylab="",main="",type="n",cex.axis=0.5,ylim=c(-3,3),xlim=c(-3,3), axes = my.axes,xaxt=x.lab,yaxt=y.lab)
    lpm.model=lm(m2,data=plot.data)
    coef1=lpm.model$coefficients
    #####
#     require(brglm)
#     require(texreg)
#     lpm.pred=predict(lm(m2,data=plot.data),hold.data)
#     logit.pred=predict(glm(m2,family=binomial(link="logit"),data=plot.data),hold.data,type="response")
#     probit.pred=predict(glm(m2,family=binomial(link="probit"),data=plot.data),hold.data,type="response")
#     if(data.ind!=3)
#     {
#       a11=glm(m2,family=binomial(link="logit"),data=plot.data)
#       a12=glm(m2,family=binomial(link="probit"),data=plot.data)
#       a21=brglm(m2,family=binomial(link="logit"),data=plot.data)
#       a22=brglm(m2,family=binomial(link="probit"),data=plot.data)
#       cat(capture.output(texreg(list(a11,a21,a12,a22))),file="D:\\Prof.Galit\\LPM\\PaperResults\\PaperLPM\\latex1.txt",sep="\n",append=T)
#       #     write(texreg(list(a11,a21,a12,a22)),file="D:\\Prof.Galit\\LPM\\PaperResults\\PaperLPM\\latex.txt",sep="\n")
#     }
#     
    #####
    sd1=round(summary(lm(m2,data=plot.data))$coefficients[,2],3)
#     abline(coef1,col="black",lty=1)
#     my.plot.equation(coef1,sd1,"b.ols",xl[1],yl[1],"black")
#     
    
    ### bias correction
#     if(error.dist==1)
#     {
#       ## Normal errors
      bias.n=.798*sd(plot.data$Y.binary)/sd(plot.data$Y)
      coef3.n=coef1
      coef3.n[2:length(coef3.n)]=coef1[2:length(coef3.n)]/bias.n
#     }
#     if(error.dist==2)
#     {
      ## Logistic 
      bias.l=.762*sd(plot.data$Y.binary)/sd(plot.data$Y)
      coef3.l=coef1
      coef3.l[2:length(coef3.l)]=coef1[2:length(coef3.l)]/bias.l
#     }
#     if(error.dist==3)
#     {
      # m=model.matrix(lpm.model)
#       bias.u= solve(t(m)%*%m)%*%t(m)%*%rep(1.5,dim(plot.data)[1])
#       coef3.u=coef1
#       coef3.u[2:length(coef3.l)]= sqrt(12)*coef1[2:length(coef3.l)]-bias.u[2:length(coef3.l)]
#       
      
#       a=-sqrt(12)/2;b=sqrt(12)/2
#       bias.u= solve(t(m)%*%m)%*%t(m)%*%rep(median(plot.data$Y)+a,dim(plot.data)[1])
#       coef3.u=coef1
#       coef3.u[2:length(coef3.u)]= (b-a)*coef1[2:length(coef3.u)]+bias.u[2:length(coef3.u)]
    # }
      m.mat=model.matrix(lpm.model)
      if(case==1){
        a=-sqrt(3)/10;b=sqrt(3)/10
      }else
      {
        a=-sqrt(3);b=sqrt(3)
      }
      flag=which(a<=0.5-(plot.data$Y-plot.data$eps)& 0.5-(plot.data$Y-plot.data$eps)<b)
      m.mat1=m.mat[flag,]
      bias.u= solve(t(m.mat1)%*%m.mat1)%*%t(m.mat1)%*%rep((b-0.5)/(b-a),length(flag))
      coef3.u=cbind(coef1[1],coef(lm(m2,plot.data[flag,]))[2:length(coef1)])
      coef3.u[2:length(coef3.u)]= (b-a)*coef3.u[2:length(coef3.u)]-bias.u[[2:length(coef3.u)]]
      
      
    
    #####
    abline(coef3.n,lty=2)
    my.plot.equation(coef3.n,sd1,"b.nor",xl[1],yl[1],"black")  
    #   
    abline(coef3.l,lty=3)
    my.plot.equation(coef3.l,sd1,"b.log",xl[2],yl[2],"black")
    #
    abline(coef3.u,lty=4)
    my.plot.equation(coef3.u,sd1,"b.uni",xl[3],yl[3],"black")  
    
    
    
    
#     #WOLS
#     t=lm(m2,data=plot.data)$fitted
#     plot.data$weight.lpm=1/sqrt(t*(1-t))
#     coef2=lm(m2,data=plot.data,weights=weight.lpm)$coefficients
#     sd2=round(summary(lm(m2,data=plot.data,weights=weight.lpm))$coefficients[,2],3)
#     #####
#     lpm.pred.wls=predict(lm(m2,data=plot.data,weights=weight.lpm),hold.data)
#     ##
#     ## ROC
#     lpm.pred1=ifelse(lpm.pred>0.5,1,0)
#     logit.pred1=ifelse(logit.pred>0.5,1,0)
#     probit.pred1=ifelse(probit.pred>0.5,1,0)
#     lpm.pred.wls1=ifelse(lpm.pred.wls>0.5,1,0)
#     ##
#     dev.set(dev.next())
#     require(ROCR)
#     roc.legend=function(auc,lab)
#     { 
#       #     if(sum(dim(conf)==c(2,2))==0)
#       #     {
#       #       return(paste("$",lab,"(",round(100*sum(conf[1,1]+conf[2,2])/sum(conf),2),"\\%)$",sep=""))
#       #     }else
#       #       return(paste("$",lab,"(",round(100*conf[1,1]/sum(conf),2),"\\%)$",sep=""))
#       return(paste("$",lab,"=",round(auc,2),"$"))
#     }    
#     #     lpm.roc=roc.legend(xtabs(~lpm.pred1+hold.data$Y.binary),"LPM")
#     #     #   lpm.lab=paste("LPM(",round(100*sum(lpm.conf[1,1]+lpm.conf[2,2])/sum(lpm.conf),2),"%)",sep="")
#     plot(performance(prediction(lpm.pred,hold.data$Y.binary),"tpr","fpr"),col="black",xaxis.at=x.roc.lim,yaxis.at=y.roc.lim,xaxis.cex.axis=0.5,yaxis.cex.axis=0.5)
#     lpm.auc=as.numeric(performance(prediction(lpm.pred,hold.data$Y.binary),"auc")@y.values)
#     plot(performance(prediction(logit.pred,hold.data$Y.binary),"tpr","fpr"),col="red",add=T,lty=2)
#     logit.auc=as.numeric(performance(prediction(logit.pred,hold.data$Y.binary),"auc")@y.values)
#     plot(performance(prediction(probit.pred,hold.data$Y.binary),"tpr","fpr"),col="brown",add=T,lty=3,xaxt=x.lab)
#     probit.auc=as.numeric(performance(prediction(probit.pred,hold.data$Y.binary),"auc")@y.values)
#     plot(performance(prediction(lpm.pred.wls,hold.data$Y.binary),"tpr","fpr"),col="darkgreen",add=T,lty=4,xaxt=x.lab)
#     wls.auc=as.numeric(performance(prediction(lpm.pred.wls,hold.data$Y.binary),"auc")@y.values)
#     
#     lpm.roc=roc.legend(lpm.auc,"LPM")
#     logit.roc=roc.legend(logit.auc,"Logit")
#     probit.roc=roc.legend(probit.auc,"Probit")
#     wls.roc=roc.legend(wls.auc,"WLS")
#     legend.fill=c("black","red","brown","darkgreen")
#     legend(.6,.6,c(lpm.roc,logit.roc,probit.roc,wls.roc),fill=legend.fill,
#            col=legend.fill,cex=0.5,title="AUC")
#     
#     ##
#     dev.set(dev.next())
#     if(data.ind==3)
#     {
#       lab1=c("OLS","OLS")
#       lab2=c("Log","Log")
#       lab3=c("Pro","Pro")
#       lab4=c("WLS","WLS")
#     }else
#     {
#       lab1=lab2=lab3=lab4=c("","")
#     }
#     if(binary.flag) ylim=c(0,1)
#     else ylim=c(-1,2)
#     boxplot(lpm.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,names=lab1,cex.axis=0.5,ylim=ylim,yaxt=y.lab)
#     boxplot(logit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.32,names=lab2,cex.axis=0.5,border="blue")
#     boxplot(probit.pred~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2-.16,names=lab3,cex.axis=0.5,border="darkblue")
#     boxplot(lpm.pred.wls~hold.data$Y.binary,boxwex=.15,medlwd=0.05,add=T,at=1:2+.16,names=lab4,cex.axis=0.5,border="red")
#     if(data.ind==3)
#     {
#       mtext(paste("$y=0 \\hspace{3cm} y=1$"),1, cex = 0.5, line = 1.5)
#     }
#     
#     dev.set(dev.prev())
#     dev.set(dev.prev())
#     
    #####
    #     abline(coef2,col="red",lty=3)
    #     my.plot.equation(coef2,sd2,"b.wls",xl[2],yl[2],"red")  
    #   
    
    #   #Zero-Intercept
    #   coef3=lm(m3,data=plot.data)$coefficients
    #   sd3=round(summary(lm(m3,data=plot.data))$coefficients[,2],3)
    #   abline(c(0,coef3),col="darkblue",lty=4)
    #   my.plot.equation(coef3,sd3,"b.zero",xl[3],yl[3],"darkblue")
    
    # OLS
    coef4=lm(m1,data=plot.data)$coefficients
    sd4=round(summary(lm(m1,data=plot.data))$coefficients[,2],3)
    abline(coef4,col="gray")
    my.plot.equation(coef4,sd4,"ols",xl[4],yl[4],"black")
  }
  #   creating plot for each datasets
  compare.reg1(plot.data=tiny.data,case=case,data.ind=1)
  compare.reg1(plot.data=sample.data,case=case,data.ind=2)
  compare.reg1(plot.data=large.data,case=case,data.ind=3)
}

