setwd("~/Suneel/Prof.Galit/LPM/PaperResults/Revision1")
#binomial 2 run
remove(list=ls())
options(scipen = 999)
source('~/Suneel/Prof.Galit/LPM/PaperResults/Revision1/binomial2-rev1.R')

###input
pdf1="LPMvsOLS_new_U.tex"  #LPMvsOLS_new_L
pdf2="LPMvsOLS_Median_roc_U.tex" #LPMvsOLS_Median_roc_L
pdf3="LPMvsOLS_Median_Pred_U.tex" #LPMvsOLS_Median_Pred_L
#
main.lab=paste("$y_{b}= I(y \\textgreater M_y )$")
data.lab=paste("$ n=2M \\hspace{1.5cm} n=500 \\hspace{1.5cm} n=50$")
parm.lab=paste("$ \\beta_0=0.5,\\beta_1=1, \\sigma_y=0.1 \\hspace{2cm} \\beta_0=0.5,\\beta_1=0.1 ,\\sigma_y=1)$")

#Logis(0,\\sqrt{3}/\\pi)
# N(0,1)
# U(-\\sqrt(12)/2,\\sqrt(12)/2)
#
#         par(mfcol = c(3, 2),
#            cex=0.6,
#            mar = c(0, 0, 0, 0), 
#            oma = c(4, 4, 4, 4)+0.5,
#            tcl = -0.25,
#            mgp = c(2, 0.6, 0))
########
require(tikzDevice)
require(grid)
graphics.off()
tikz(pdf1,standAlone = TRUE, width = 4,height = 2.5,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfcol = c(3, 2),
    cex=0.6,
    mar = c(0, 0, 0, 0), 
    oma = c(4, 4, 4, 4)+0.5,
    tcl = -0.25,
    mgp = c(2, 0.6, 0))
# x11()
# device for ROC
tikz(pdf2,standAlone = TRUE, width = 4,height = 2.5,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfcol = c(3, 2),
    cex=0.6,
    mar = c(0, 0, 0, 0), 
    oma = c(4, 4, 4, 4)+0.5,
    tcl = -0.25,
    mgp = c(2, 0.6, 0))
# x11()
# device for pred box
tikz(pdf3,standAlone = TRUE, width = 4,height = 2.5,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfcol = c(3, 2),
    cex=0.6,
    mar = c(0, 0, 0, 0), 
    oma = c(4, 4, 4, 4)+0.5,
    tcl = -0.25,
    mgp = c(2, 0.6, 0))
# x11()

#
dev.list()
dev.set(dev.prev())
dev.set(dev.prev())

#case1 with b0=.5,b1=1,eps=.01
binomCase2(F,F,1,F,3)
#case2 with b0=.5,b1=.1,eps=.1
binomCase2(F,F,2,F,3)
# labelling
mtext(main.lab, side = 3, outer = TRUE, cex = 0.6, line = 2)
mtext(data.lab, side = 2, outer = TRUE, cex = 0.5, line =1.2)
mtext("$ x \\sim N(0,1),  y|x \\sim U(\\beta_0+\\beta_1x-\\sqrt{3}\\sigma_y,\\beta_0+\\beta_1x+\\sqrt{3}\\sigma_y) $", side = 1, outer = TRUE, line = 2.2,cex=0.6)
mtext("$y$", side = 2,las=2, outer = TRUE, line = 2.2,cex=0.6)
mtext(parm.lab, side = 3, outer = TRUE, line = 0.5,cex=0.5)
dev.off()


# mtext(main.lab, side = 3, outer = TRUE, cex = 0.6, line = 2)
mtext(data.lab, side = 2, outer = TRUE, cex = 0.5, line =1.2)
mtext("FPR", side = 1, outer = TRUE,  line =2,cex=0.6)
mtext("TPR", side = 2,las=2, outer = TRUE, line = 1.7,cex=0.6)
# mtext(parm.lab, side = 3, outer = TRUE, line = 0.5,cex=0.5)
dev.off()

mtext(main.lab, side = 3, outer = TRUE, cex = 0.6, line = 2)
mtext(data.lab, side = 2, outer = TRUE, cex = 0.5, line =1.2)
mtext("$y_b$", side = 1, outer = TRUE, line = 1.7,cex=0.6)
mtext("$\\hat{y_b}$", side = 2,las=2, outer = TRUE, line = 2,cex=0.6)
mtext(parm.lab, side = 3, outer = TRUE, line = 0.5,cex=0.5)
dev.off()

tools::texi2pdf(pdf1)
tools::texi2pdf(pdf2)
tools::texi2pdf(pdf3)





### scatter
require(tikzDevice)
graphics.off()
tikz("scatter.tex",standAlone = TRUE, width = 4,height = 2,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfrow = c(1, 2))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4)+0.5)
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

plot(large.data$X,large.data$Y,xlab="",ylab="")
coef=lm(large.data$Y~large.data$X)$coefficients
abline(coef,col="darkgreen")
sd=round(summary(lm(large.data$Y~large.data$X))$coefficients[,2],3)
my.plot.equation(coef,sd,"ols",-0.2,0.8,"darkgreen")

plot(large.data$X,large.data$Y.binary,xlab="",ylab="",yaxt="n")
coef=lm(large.data$Y.binary~large.data$X)$coefficients
abline(coef,col="red")
sd=round(summary(lm(large.data$Y.binary~large.data$X))$coefficients[,2],3)
my.plot.equation(coef,sd,"b.ols",-0.2,0.8,"red")

mtext(paste("$\\beta_0=0.5,\\beta_1=1,\\epsilon \\sim N(0,0.01)$"), side = 3, outer = TRUE, cex = 0.9, line = 1)
mtext("$x\\sim U(-0.5,0.5)$", side = 1, outer = TRUE, line = 2.2)
mtext("$y$", side = 2, outer = TRUE, line = 2.2,las=2)
mtext("$y_b$", side = 4, outer = TRUE, line = 1,las=2)
dev.off()

tools::texi2pdf("scatter.tex")
#################




########################################
#binomial 2 run
remove(list=ls())
setwd("D:/Prof.Galit/LPM/PaperResults/PaperLPM")
source('D:/Prof.Galit/LPM/PaperResults/PaperLPM/binomial2.R')
source('D:/Prof.Galit/LPM/PaperResults/PaperLPM/plotreg.R')

require(tikzDevice)
require(grid)
graphics.off()
tikz("LPM-Simulation-Over-Spec-Binomial-epsnorm1.tex",standAlone = TRUE, width = 5,height = 6,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
# x11()
pushViewport(viewport(layout=grid.layout(3, 2, widths=unit(2.5, "inches"),
                                         heights=unit(0.45, "npc"),
                                         just=just)))

# device for ROC
# x11()
tikz("LPM-Simulation-Over-Spec-Binomial-roc1.tex",standAlone = TRUE, width = 5,height = 6,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfcol = c(3, 2))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4)+0.5)
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
# device for pred box
# x11()
tikz("LPM-Simulation-Over-Spec-Binomial-predhold1.tex",standAlone = TRUE, width = 5,height = 6,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfcol = c(3, 2))
par(cex = 0.6)
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4)+0.5) 
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
dev.list()
dev.set(dev.prev())
dev.set(dev.prev())

#case1 with b0=.5,b1=1,eps=.01
binomCase3(F,F,1,T)
#case2 with b0=.5,b1=.1,eps=.1
binomCase3(F,F,2,T)
# labelling
mtext(paste("$y_{b} \\sim B(n,\\beta_0+\\beta_1x+\\epsilon )$"), side = 3, outer = TRUE, cex = 0.9, line = 2)
mtext(paste("$ n=2M \\hspace{5.5cm} n=500 \\hspace{5.5cm} n=50$"), side = 2, outer = TRUE, cex = 0.6, line =1.2)
mtext("$x,z\\sim U(-0.5,0.5)$", side = 1, outer = TRUE, line = 2.2)
mtext("$y$", side = 2,las=2, outer = TRUE, line = 2.2)
mtext(paste("$  \\beta_0=0.5,\\beta_1=1,\\epsilon \\sim N(0,0.01^2) \\hspace{4cm} \\beta_0=0.5,\\beta_1=0.1,\\epsilon \\sim N(0,0.1^2)$"), side = 3, outer = TRUE, line = 0.5,cex=0.6)
dev.off()

mtext(paste("$y_{b} \\sim B(n,\\beta_0+\\beta_1x+\\epsilon )$"), side = 3, outer = TRUE, cex = 0.9, line = 2)
mtext(paste("$ n=2M \\hspace{5.5cm} n=500 \\hspace{5.5cm} n=50$"), side = 2, outer = TRUE, cex = 0.6, line =1.2)
mtext("FPR", side = 1, outer = TRUE, cex = 0.8, line =2)
mtext("TPR", side = 2,las=2, outer = TRUE, cex = 0.8, line = 1.7)
mtext(paste("$  \\beta_0=0.5,\\beta_1=1,\\epsilon \\sim N(0,0.01^2) \\hspace{4cm} \\beta_0=0.5,\\beta_1=0.1,\\epsilon \\sim N(0,0.1^2)$"), side = 3, outer = TRUE, line = 0.5,cex=0.6)
dev.off()

mtext(paste("$y_{b} \\sim B(n,\\beta_0+\\beta_1x+\\epsilon )$"), side = 3, outer = TRUE, cex = 0.9, line = 2)
mtext(paste("$ n=2M \\hspace{5.5cm} n=500 \\hspace{5.5cm} n=50$"), side = 2, outer = TRUE, cex = 0.6, line =1.2)
mtext("$y_b$", side = 1, outer = TRUE, line = 1.7)
mtext("$\\hat{y_b}$", side = 2,las=2, outer = TRUE, line = 2)
mtext(paste("$  \\beta_0=0.5,\\beta_1=1,\\epsilon \\sim N(0,0.01^2) \\hspace{4cm} \\beta_0=0.5,\\beta_1=0.1,\\epsilon \\sim N(0,0.1^2)$"), side = 3, outer = TRUE, line = 0.5,cex=0.6)
# mtext(paste("$  \\beta_0=0.5,\\epsilon \\sim N(0,0.1^2) \\hspace{3cm} \\beta_0=0.5,\\epsilon \\sim N(0,0.01^2)$"), side = 2, outer = TRUE, line = 1.5,cex=0.6)

dev.off()

tools::texi2pdf("LPM-Simulation-Over-Spec-Binomial-epsnorm1.tex")
tools::texi2pdf("LPM-Simulation-Over-Spec-Binomial-roc1.tex")
tools::texi2pdf("LPM-Simulation-Over-Spec-Binomial-predhold1.tex")


###########################################################
###################################################

setwd("~/Suneel/Prof.Galit/LPM/PaperResults/Revision1")
#binomial 2 run
remove(list=ls())
options(scipen = 999)
source('~/Suneel/Prof.Galit/LPM/PaperResults/Revision1/binomial2-rev1.R')

###input
pdf1="LPMCorvsOLS_new_N_U.tex"  #LPMvsOLS_new_L
# pdf2="LPMvsOLS_Median_roc_U.tex" #LPMvsOLS_Median_roc_L
# pdf3="LPMvsOLS_Median_Pred_U.tex" #LPMvsOLS_Median_Pred_L
#
main.lab=paste("$y_{b}= I(\\beta_0+\\beta_1x+\\epsilon \\textgreater 0.5 )$")
data.lab=paste("$ n=2M \\hspace{1.5cm} n=500 \\hspace{1.5cm} n=50$")
parm.lab=paste("$ \\beta_0=0.5,\\beta_1=1, \\epsilon \\sim N(0,0.1^2) \\hspace{2cm} \\beta_0=0.5,\\beta_1=0.1 ,\\epsilon \\sim N(0,1) $")

#Logis(0,\\sqrt{3}/\\pi)
# N(0,1)
# U(-\\sqrt(12)/2,\\sqrt(12)/2)
#
#         par(mfcol = c(3, 2),
#            cex=0.6,
#            mar = c(0, 0, 0, 0), 
#            oma = c(4, 4, 4, 4)+0.5,
#            tcl = -0.25,
#            mgp = c(2, 0.6, 0))
########
require(tikzDevice)
require(grid)
graphics.off()
tikz(pdf1,standAlone = TRUE, width = 4,height = 2.5,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
par(mfcol = c(3, 2),
    cex=0.6,
    mar = c(0, 0, 0, 0), 
    oma = c(4, 4, 4, 4)+0.5,
    tcl = -0.25,
    mgp = c(2, 0.6, 0))
# x11()
# device for ROC
# tikz(pdf2,standAlone = TRUE, width = 4,height = 2.5,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
# par(mfcol = c(3, 2),
#     cex=0.6,
#     mar = c(0, 0, 0, 0), 
#     oma = c(4, 4, 4, 4)+0.5,
#     tcl = -0.25,
#     mgp = c(2, 0.6, 0))
# # x11()
# # device for pred box
# tikz(pdf3,standAlone = TRUE, width = 4,height = 2.5,packages=c(options()$tikzLatexPackages, "\\usepackage{amsfonts,amsmath,textcomp}"))
# par(mfcol = c(3, 2),
#     cex=0.6,
#     mar = c(0, 0, 0, 0), 
#     oma = c(4, 4, 4, 4)+0.5,
#     tcl = -0.25,
#     mgp = c(2, 0.6, 0))
# # x11()
# 
# #
# dev.list()
# dev.set(dev.prev())
# dev.set(dev.prev())

#case1 with b0=.5,b1=1,eps=.01
compare(1,1)
#case2 with b0=.5,b1=.1,eps=.1
compare(2,1)
# labelling
mtext(main.lab, side = 3, outer = TRUE, cex = 0.6, line = 2)
mtext(data.lab, side = 2, outer = TRUE, cex = 0.5, line =1.2)
mtext("$  x \\sim U(0,1) $", side = 1, outer = TRUE, line = 2.2,cex=0.6)
mtext("$y$", side = 2,las=2, outer = TRUE, line = 2.2,cex=0.6)
mtext(parm.lab, side = 3, outer = TRUE, line = 0.5,cex=0.5)
dev.off()


# # mtext(main.lab, side = 3, outer = TRUE, cex = 0.6, line = 2)
# mtext(data.lab, side = 2, outer = TRUE, cex = 0.5, line =1.2)
# mtext("FPR", side = 1, outer = TRUE,  line =2,cex=0.6)
# mtext("TPR", side = 2,las=2, outer = TRUE, line = 1.7,cex=0.6)
# # mtext(parm.lab, side = 3, outer = TRUE, line = 0.5,cex=0.5)
# dev.off()
# 
# mtext(main.lab, side = 3, outer = TRUE, cex = 0.6, line = 2)
# mtext(data.lab, side = 2, outer = TRUE, cex = 0.5, line =1.2)
# mtext("$y_b$", side = 1, outer = TRUE, line = 1.7,cex=0.6)
# mtext("$\\hat{y_b}$", side = 2,las=2, outer = TRUE, line = 2,cex=0.6)
# mtext(parm.lab, side = 3, outer = TRUE, line = 0.5,cex=0.5)
# dev.off()

tools::texi2pdf(pdf1)
# tools::texi2pdf(pdf2)
# tools::texi2pdf(pdf3)



###########################################################

system.time(lm(m2,data=large.data))
user  system elapsed 
6.85    0.19    7.03 
system.time(lm(m2,data=sample.data))
user  system elapsed 
0.00    0.00    0.02 
> system.time(lm(m2,data=tiny.data))
user  system elapsed 
0       0       0 
system.time(glm(m2,data=tiny.data,family=binomial(link="logit")))
user  system elapsed 
0       0       0 
system.time(glm(m2,data=tiny.data,family=binomial(link="probit")))
user  system elapsed 
0       0       0 
system.time(glm(m2,data=sample.data,family=binomial(link="logit")))
user  system elapsed 
0       0       0 
system.time(glm(m2,data=sample.data,family=binomial(link="probit")))
user  system elapsed 
0       0       0 
system.time(glm(m2,data=large.data,family=binomial(link="logit")))
user  system elapsed 
27.53    2.15   32.04 
system.time(lm(m2,data=large.data))
user  system elapsed 
2.68    0.14    2.87 
> system.time(lm(m2,data=sample.data))
user  system elapsed 
0.01    0.00    0.01 
> system.time(lm(m2,data=tiny.data))
user  system elapsed 
0       0       0 
> system.time(glm(m2,data=large.data,family=binomial(link="logit")))
user  system elapsed 
23.77    1.54   25.45 
> system.time(glm(m2,data=sample.data,family=binomial(link="logit")))
user  system elapsed 
0       0       0 
> system.time(glm(m2,data=tiny.data,family=binomial(link="logit")))
user  system elapsed 
0.01    0.00    0.02 
system.time(glm(m2,data=large.data,family=binomial(link="probit")))
user  system elapsed 
25.30    1.56   26.90 
system.time(glm(m2,data=sample.data,family=binomial(link="probit")))
user  system elapsed 
0       0       0 
system.time(brglm(m2,data=large.data,family=binomial(link="probit")))
user  system elapsed 
605.33   87.55  699.51 
Warning message:
  In fit.proc(x = X, y = Y, weights = weights, start = start, etastart = etastart,  :
                Iteration limit reached
              system.time(brglm(m2,data=sample.data,family=binomial(link="probit")))
              user  system elapsed 
              0.04    0.00    0.04 
              system.time(brglm(m2,data=tiny.data,family=binomial(link="probit")))
              user  system elapsed 
              0       0       0 
              system.time(brglm(m2,data=large.data,family=binomial(link="logit")))
              user  system elapsed 
              59.17    6.63   65.88 
              system.time(brglm(m2,data=sample.data,family=binomial(link="logit")))
              user  system elapsed 
              0.01    0.00    0.02 
              system.time(brglm(m2,data=tiny.data,family=binomial(link="logit")))
              user  system elapsed 
              0.02    0.00    0.02 
              
              polyserial <-function(x, y, ML=FALSE, control=list(), std.err=FALSE, maxcor=.9999, bins=4){
                if (!is.numeric(x)) stop("x must be numeric")
                valid <- complete.cases(x, y)
                x <- x[valid]
                y <- y[valid]
                #z <- scale(x)
                tab <- table(y)
                prop=tab/sum(tab)
                #n <- sum(tab)
                M1=mean(x[which(y!=1)])
                M0=mean(x[which(y!=0)])
                s <- length(tab)
                if (s < 2) {
                  warning("y has fewer than 2 levels")
                  return(NA)
                }
                if (sum(0 != tab) < 2){
                  warning("y has fewer than 2 levels with data")
                  return(NA)
                }
                rho=(M1-M0)*sqrt(prop[1]*prop[2])/sd(x)
                return(rho)
              }
              
              
              
  
              
              
                          
              