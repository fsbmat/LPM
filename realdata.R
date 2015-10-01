
remove(list=ls())
###########################################################################
################## Data Preparation #######################################
## libraries
require(sqldf)
## options
options(scipen=999)
options(digits=2)
###

cameras <- file("analysiswheads")
raw.data <- sqldf("select * from cameras",
                  dbname=tempfile(),
                  file.format=list(header=TRUE,sep="|",eol="\n"))


cols <- scan('colnames',what='character',sep=',')
colnames(raw.data) <- cols

large.data=raw.data
large.data$auction.starts <- strptime(raw.data[,'Auction Start Date'],format="%d-%b-%y %H:%M:%S")
large.data$auction.ends <- strptime(raw.data[,'Auction End Date'],format="%d-%b-%y %H:%M:%S")
large.data$duration <- difftime(large.data$auction.ends , large.data$auction.starts,units='days')
large.data$camera.type <- factor(raw.data[,'Camera Type Value ID.'],
                                 levels=c(-99,-6,25566,25567,31694,37116),
                                 labels=c('Unknown','Other','SLR','PointnShoot','Analog','Digital'))
large.data$brand <- factor(raw.data[,'Brand Value ID.'],
                           levels=c(-99, -6, 1753, 1758, 1762, 1764, 1765, 2221, 25480, 25481, 25482, 25483, 25484, 25485, 25486, 25487, 25488, 25489, 25490, 25637, 26044, 26045, 26046, 26452, 30319, 31708, 41854, 76339),
                           labels=c('Unknown', 'Other', 'Casio', 'HP', 'Sharp', 'Sony', 'Toshiba', 'Polaroid', 'Canon', 'Fujifilm', 'Kodak', 'Konika', 'Minolta', 'Nikon', 'Olympus', 'Panasonic', 'Pentax', 'Samsung', 'Vivitar', 'Sanyo', 'Bell & Howell', 'Mustek', 'Logitech', 'Gateway', 'Fuji', 'Aiptek', 'Opteka','SVP'))
large.data$productline <- factor(raw.data[,'Product Line Value ID.'],
                                 levels=c(-99, -6, 30274, 30277, 30280, 30286, 31084, 31086, 31087, 31089, 31090, 31091, 31092, 31093, 31094, 31095, 31096, 31097, 31098, 31099, 31108, 31109, 31111, 31113, 31114, 31115, 31119, 31123, 31124, 31125, 31127, 31130, 31131, 31132, 31135, 31138, 31139, 31140, 31141, 31142, 31143, 31144, 31145, 31146, 31147, 31149, 31155, 31161, 31164, 31165, 31166, 33018, 33965, 33968),
                                 labels=c('Unknown', 'Other', 'Canon PowerShot','Canon Digital IXUS','Canon EOS','Canon ZR','Casio EXILIM','Casio QV','HP PhotoSmart','Fujifilm DS','Fujifilm DX','Fujifilm FinePix','Fujifilm MX','Kodak DC','Kodak DCS','Kodak EasyShare CX','Kodak EasyShare DX','Kodak EasyShare LS','Kodak EZ','Konica Digital Revio','Logitech Pocket Digital','Logitech QuickCam','Minolta DiMAGE','Mustek DV','Mustek GSmart','Mustek MDC','Nikon Coolpix','Olympus CAMEDIA C','Olympus CAMEDIA D','Olympus CAMEDIA E','Olympus Stylus','Panasonic CoolShot','Panasonic D-Snap','Panasonic Lumix','Panasonic PalmCam','Panasonic iPalm','Pentax EI','Pentax *ist','Pentax Optio','Pentax OptioS','Polaroid PDC','Polaroid PhotoMAX','Polaroid i-Zone','Polaroid iON','Samsung Digimax','Samsung SCD','Sony Cyber-Shot','Sony Mavica','Toshiba PDR','Vivitar DSC','Vivitar ViviCam','Nikon D','Sharp Viewcam','Sony Handycam DCR'))
large.data$condition <- factor(raw.data[,'Condition Value ID.'],
                               levels=c(-99,10425,10426,22656,24269,26588),
                               labels=c('Unknown','New','Used','New','New','Refurbished'))
large.data$condition=factor(large.data$condition)
large.data$reserve.price=factor(raw.data[,'Reserve Price Flag'],
                                levels=c("N","Y"),
                                labels=c("No","Yes"))
large.data$minimum.bid=raw.data[,'Start Price USD']
large.data$price=raw.data[,'Final Unit Price']
large.data$Seller.Feedback=large.data[,'Seller Feedback']
# ###
# large.data=large.data[which(large.data$'Reserve Price Flag'=="N"),]
# #outliers
# large.data=large.data[which(large.data$price<=200),]
# #
# large.data=large.data[which(large.data$duration<=200),]
# ###
# summary(large.data$price)
# hist(large.data$price.b.s)
# hist(large.data$price.s)
# write.csv(large.data,"camera.csv",quote=F)

large.data$lprice.s=with(large.data,scale(log(price)))
large.data$lprice.b=with(large.data,ifelse(log(price)>median(log(price)),1,0))
#large.data$price.s=with(large.data,scale(price))
large.data$lprice.b.s=with(large.data,scale(lprice.b))
dim(large.data)


save(large.data,file="large.data.saved")
remove(raw.data)
##########################################################################
##########################################################################
set.seed(1)
hold.size=5000  #round(dim(large.data)[1]*.03)
hold.data.index=sample.int(dim(large.data)[1], hold.size, replace = FALSE, prob = NULL)
hold.data=large.data[hold.data.index,]
large.data1=large.data[-hold.data.index,]
dim(hold.data)
dim(large.data1)

#+reserve.price
m1=as.formula(lprice.s~minimum.bid+Seller.Feedback+duration+reserve.price)
m2=as.formula(lprice.b.s~minimum.bid+Seller.Feedback+duration+reserve.price)
m3=as.formula(lprice.b~minimum.bid+Seller.Feedback+duration+reserve.price)
options(scipen=2)
ols.model=lm(m1,data=large.data1)
lpm.model=lm(m2,data=large.data1)
lpm.model1=lm(m3,data=large.data1)
require(texreg)
options(digits=4)
screenreg(list(ols.model,lpm.model),digits=4)

require(brglm)
logit.model=glm(m3,data=large.data1,family=binomial(link="logit"))
logit.model1=brglm(m3,data=large.data1,family=binomial(link="logit"))

probit.model=glm(m3,data=large.data1,family=binomial(link="probit"))
probit.model1=brglm(m3,data=large.data1,family=binomial(link="probit"))
screenreg(list(logit.model,probit.model),digits=4)

texreg(list(ols.model,lpm.model),digits=4)

screenreg(list(ols.model,lpm.model,lpm.model1,logit.model,probit.model),digits=4)


lpm.pred=predict(lpm.model,hold.data)
lpm.pred1=predict(lpm.model1,hold.data)
logit.pred=predict(logit.model,hold.data,type="response")
probit.pred=predict(probit.model,hold.data,type="response")


################# Plotting predictions for holdout sample
graphics.off()
require(tikzDevice)
tikz("camera_pred.tex",standAlone = TRUE, width = 4,height = 2,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfcol = c(1, 2),
    cex=0.6,
    mar = c(0, 0, 0, 3), 
    oma = c(4, 4, 2, 0),
    tcl = -0.25,
    mgp = c(2, 0.6, 0))

lab1=c("LPM","LPM")
lab2=c("Log","Log")
lab3=c("Pro","Pro")


boxplot(logit.pred~hold.data$lprice.b,boxwex=.15,medlwd=0.05,ylim=c(0,1.7),names=lab2,cex.axis=0.5,border="blue")
boxplot(lpm.pred1~hold.data$lprice.b,boxwex=.15,medlwd=0.05,add=T,at=1:2+.16,cex.axis=0.5,names=lab1)
boxplot(probit.pred~hold.data$lprice.b,boxwex=.15,medlwd=0.05,add=T,at=1:2-.16,cex.axis=0.5,border="red",names=lab3)
mtext(paste("$Price.b=0 \\hspace{2.5cm} Price.b=1$"),1, cex = 0.4, line = 1.5)
# mtext(paste("$p_{b}$"),1,line = 2.2,cex=0.6)
mtext(paste("$\\hat{Price.b}$"),2,line = 2,cex=0.5)

require(ROCR)
plot(performance(prediction(lpm.pred1,hold.data$lprice.b),"tpr","fpr"),xaxis.cex.axis=0.5,yaxis.cex.axis=0.5,lwd=0.5)
plot(performance(prediction(logit.pred,hold.data$lprice.b),"tpr","fpr"),col="blue",add=T,lty=3,lwd=0.5)
plot(performance(prediction(probit.pred,hold.data$lprice.b),"tpr","fpr"),col="red",add=T,lty=3,lwd=0.5)
legend(.6,.4,c("$LPM$","$Logit$","$Probit$"),fill=c("black","blue","red"),col=c("black","blue","red"),border=c("black","blue","red"),cex=0.5)
mtext(paste("$FPR$"),1,line = 2.2,cex=0.4)
mtext(paste("$TPR$"),4,line = 0.75,las=2,cex=0.4)
dev.off()

tools::texi2pdf("camera_pred.tex")


#################### skewness example
tikz("price_skew.tex",standAlone = TRUE, width = 4,height = 2,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfcol = c(1, 2),
    cex=0.6,
    mar = c(0, 0, 0, 0), 
    oma = c(4, 4, 2, 1),
    tcl = -0.25,
    mgp = c(2, 0.6, 0))

hist(large.data$price,xlab="$Price$",main="",cex.axis=0.5)
mtext(paste("$Price$"),1, cex = 0.5, line = 2)
mtext(paste("$frequency$"),2, cex = 0.5, line = 1.5)
hist(log(large.data$price),xlab="$ln.Price$",main="",ylab="",cex.axis=0.5,yaxt="n")
mtext(paste("$lnPrice$"),1, cex = 0.5, line = 2)
dev.off()

tools::texi2pdf("price_skew.tex")



##########################################################################


large.data$res1=factor(large.data$reserve.price,levels=c("No","Yes"),labels=c("0","1"))

mean(as.numeric(as.character(large.data$res1)))
