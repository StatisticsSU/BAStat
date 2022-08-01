#' Compute p-values and take decisions under a t-distribution or a normal distribution
#' 
#' This function helps in the learning process: when the null hypothesis can be rejected based on the p-value. It is implemented for one and two tailed t-tests. It is for the test of hypothesis for the mean of one or two populations as well as a support for the estimated parameters of a regression model.
#' @param Crit critical value
#' @param Obs observed value already as a Student or standardized Normal value
#' @param degree.freed it is the number of degree of freedom
#' @param alternative `two.sided` if the alternative hypothesis is on both sides; `greater` if the alternative hypothesis is on the right side; `less` if the alternative hypothesis is on the left side.
#' @param method `areas` in this case the comparison and the decision is based on the areas and the result of the p-values is also presented. If method is equal to `values` than the decision is based on the values of the distribution and the values from the observations.
#' @return plot with the comparison between the theoretical area for the critical region and the obtained p-value. On the top of the plot is reported the answer: if it has to be rejected of not the null hypothesis. On the left side of the plot is reported the computed p-value and the computed area for the theoretical value. If the method is with the values then the plot will be with the critical point and the observed point
#' @export 
#' @examples
#' library(BAStat)
#' 
#' Crit =qnorm(0.95)
#' CSPvalues(Crit = 1.65,Obs= 3,alternative="greater",method="areas")
#' CSPvalues(Crit = 1.65,Obs= 3,alternative="greater",method="values")
#' 
#' Crit =qnorm(0.05)
#' CSPvalues(Crit = -1.65,Obs= -3,alternative="less",method="areas")
#' CSPvalues(Crit = -1.65,Obs= -3,alternative="less",method="values")
#' 
#' Crit =qnorm(0.025)
#' CSPvalues(Crit = -1.96,Obs= -3,alternative="two.sided",method="areas")
#' CSPvalues(Crit = -1.96,Obs= -3,alternative="two.sided",method="values")
#' 
#' Crit =qnorm(0.975)
#' CSPvalues(Crit = 1.96,Obs= 3,alternative="two.sided",method="areas")
#' CSPvalues(Crit = 1.96,Obs= 3,alternative="two.sided",method="values")
#' 
#' Crit =qt(0.95,18)
#' CSPvalues(cs="t",Crit = 1.7341,Obs= 3.0867,degree.freed=18,alternative="greater",method="areas")
#' CSPvalues(cs="t",Crit = 1.7341,Obs= 3.0867,degree.freed=18,alternative="greater",method="values")
#' 
#' Crit =qt(0.05,18)
#' CSPvalues(cs="t",Crit = -1.7341,Obs= -3.0867,degree.freed=18,alternative="less",method="areas")
#' CSPvalues(cs="t",Crit = -1.7341,Obs= -3.0867,degree.freed=18,alternative="less",method="values")

#' Crit =qt(0.025,18)
#' CSPvalues(cs="t",Crit = -2.1009,Obs= -3.0867,degree.freed=18,alternative="two.sided",method="areas")
#' CSPvalues(cs="t",Crit = -2.1009,Obs= -3.0867,degree.freed=18,alternative="two.sided",method="values")
#' Crit =qt(0.975,18)
#' CSPvalues(cs="t",Crit = 2.1009,Obs= 3.0867,degree.freed=18,alternative="two.sided",method="areas")
#' CSPvalues(cs="t",Crit = 2.1009,Obs= 3.0867,degree.freed=18,alternative="two.sided",method="values")



CSPvalues<-function(cs="Standard Normal",Crit, Obs,alternative,method,degree.freed){
  if(cs=="Standard Normal"){
    Grid= seq(-4,4,by = 0.01)
    
    plot(Grid, dnorm(Grid), type = "l", xlab = "z", ylab = "Standardized Normal density",lwd=1)
    
    Plot = function(x){dnorm(x)}  
    
    
    if((alternative=="less") & (method=="areas")){
      
      legend(x = "topleft", inset=.05, legend = c(c("alpha:", round(pnorm(Crit),4),"P-value:",round(pnorm(Obs),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
      
      x1 = seq(from = min(Grid), to = Crit, length.out = 1000) 
      y1 = Plot(x1)
      x1 = c(Crit,min(Grid), x1, Crit) 
      
      y1 = c(y1,0, 0, 0)
      polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
      
      
      xp1 = seq(from = min(Grid), to = Obs, length.out = 1000) 
      yp1 = Plot(xp1)
      xp1 = c(Obs,min(Grid), xp1, Obs) 
      
      yp1 = c(yp1,0, 0, 0)
      lines(xp1,yp1, lwd=4,  col = "red")
      
      
      if(max(yp1)<=max(y1)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(max(yp1)>max(y1)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    if((alternative=="less")&(method=="values")){
      
      
      points(Crit,0, col = "blue", pch = 19)
      points(Obs,0, col = "red", pch = 19)
      legend(x = "topleft", inset=.05, legend = c(c("Crit",Crit), c("Obs",Obs)), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n")
      
      
      if(Obs<=Crit){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(Obs>Crit){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    
    
    if((alternative=="greater") & (method=="areas")){
      legend(x = "topleft", inset=.05, legend = c(c("alpha:", round(1-pnorm(Crit),4),"P-value:",round(1-pnorm(Obs),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
      
      
      x = seq(from = Crit, to = max(Grid), length.out = 1000) 
      y = Plot(x)
      
      x = c(Crit, x, max(Grid), Crit) 
      y = c(0, y, 0, 0)
      
      polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      
      xp = seq(from = Obs, to = max(Grid), length.out = 1000) 
      yp = Plot(xp)
      xp = c(Obs, xp, 10, Obs) 
      yp = c(0, yp, 0, 0)
      lines(xp,yp, lwd=4,  col = "red")
      
      
      if(max(yp)<=max(y)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      if(max(yp)>max(y)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    if((alternative=="greater")&(method=="values")){
      
      
      points(Crit,0, col = "blue", pch = 19)
      points(Obs,0, col = "red", pch = 19)
      legend(x = "topleft", inset=.05, legend = c(c("Crit",Crit), c("Obs",Obs)), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n")
      
      
      if(Obs>=Crit){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(Obs<Crit){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    
    
    if((alternative=="two.sided")&(method=="areas")){
      
      
      Crits<-abs(Crit)
      Obsa<-abs(Obs)
      
      legend(x = "topleft", inset=.05, legend = c(c("alpha/2:", round(1-pnorm(Crits),4),"P-value/2:",round(1-pnorm(Obsa),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
      
      x = seq(from = Crits, to = max(Grid), length.out = 1000) 
      y = Plot(x)
      
      x = c(Crits, x, max(Grid), Crits) 
      y = c(0, y, 0, 0)
      
      polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      
      xp = seq(from = Obsa, to = max(Grid), length.out = 1000) 
      yp = Plot(xp)
      xp = c(Obsa, xp, 10, Obsa) 
      yp = c(0, yp, 0, 0)
      lines(xp,yp, lwd=4,  col = "red")
      
      
      legend(x = "topleft", inset=.05, legend = c(c("alpha/2:", round(pnorm(-Crits),4),"P-value/2:",round(pnorm(-Obsa),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
      
      if(max(yp)<=max(y)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      
      if(max(yp)>max(y)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
      #################
      
      
      xp1 = seq(from = min(Grid), to = -Obsa, length.out = 1000) 
      yp1 = Plot(xp1)
      xp1 = c(-Obsa,min(Grid), xp1, -Obsa) 
      
      yp1 = c(yp1,0, 0, 0)
      lines(xp1,yp1, lwd=4,  col = "red")
      
      x1 = seq(from = min(Grid), to = -Crits, length.out = 1000) 
      y1 = Plot(x1)
      x1 = c(-Crits,min(Grid), x1, -Crits) 
      
      y1 = c(y1,0, 0, 0)
      polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
      
      
      if(max(yp1)<=max(y1)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      if(max(yp1)>max(y1)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
      
    }
    
    if((alternative=="two.sided")&(method=="values")){
      Crits<-abs(Crit)
      Obsa<-abs(Obs)
      
      points(Crit,0, col = "blue", pch = 19)
      points(Obs,0, col = "red", pch = 19)
      legend(x = "topleft", inset=.05, legend = c(c("Crit",Crit), c("Obs",Obs)), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n")
      
      
      if(Obsa>=Crits){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(Obsa<Crits){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
  }
  
  if(cs=="t"){
    
    
    
    tGrid= seq(-10,10,by = 0.01)
    
    
    plot(tGrid, (dt(tGrid,df=degree.freed)), type = "l", xlab = "X", ylab = "t-Student density",lwd=1)
    
    tPlot = function(x,df=degree.freed){dt(x,df=degree.freed)}  
    
    
    if((alternative=="less") & (method=="areas")){
      
      legend(x = "topleft", inset=.05, legend = c(c("alpha:", round(pt(Crit,degree.freed),4),"P-value:",round(pt(Obs,degree.freed),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
      
      x1 = seq(from = min(tGrid), to = Crit, length.out = 1000) 
      y1 = tPlot(x1,df=degree.freed)
      x1 = c(Crit,min(tGrid), x1, Crit) 
      
      y1 = c(y1,0, 0, 0)
      polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
      
      
      xp1 = seq(from = min(tGrid), to = Obs, length.out = 1000) 
      yp1 = tPlot(xp1,df=degree.freed)
      xp1 = c(Obs,min(tGrid), xp1, Obs) 
      
      yp1 = c(yp1,0, 0, 0)
      lines(xp1,yp1, lwd=4,  col = "red")
      
      
      if(max(yp1)<=max(y1)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(max(yp1)>max(y1)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    if((alternative=="less")&(method=="values")){
      
      
      points(Crit,0, col = "blue", pch = 19)
      points(Obs,0, col = "red", pch = 19)
      legend(x = "topleft", inset=.05, legend = c(c("Crit",Crit), c("Obs",Obs)), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n")
      
      
      if(Obs<=Crit){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(Obs>Crit){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    
    
    if((alternative=="greater") & (method=="areas")){
      legend(x = "topleft", inset=.05, legend = c(c("alpha:", round(1-pt(Crit,degree.freed),4),"P-value:",round(1-pt(Obs,degree.freed),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
      
      
      x = seq(from = Crit, to = max(tGrid), length.out = 1000) 
      y = tPlot(x,df=degree.freed)
      
      x = c(Crit, x, max(tGrid), Crit) 
      y = c(0, y, 0, 0)
      
      polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      
      xp = seq(from = Obs, to = max(tGrid), length.out = 1000) 
      yp = tPlot(xp,df=degree.freed)
      xp = c(Obs, xp, 10, Obs) 
      yp = c(0, yp, 0, 0)
      lines(xp,yp, lwd=4,  col = "red")
      
      
      if(max(yp)<=max(y)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      if(max(yp)>max(y)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    if((alternative=="greater")&(method=="values")){
      
      
      points(Crit,0, col = "blue", pch = 19)
      points(Obs,0, col = "red", pch = 19)
      legend(x = "topleft", inset=.05, legend = c(c("Crit",Crit), c("Obs",Obs)), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n")
      
      
      if(Obs>=Crit){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(Obs<Crit){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    
    
    if((alternative=="two.sided")&(method=="areas")){
      
      
      Crits<-abs(Crit)
      Obsa<-abs(Obs)
      
      legend(x = "topleft", inset=.05, legend = c(c("alpha/2:", round(1-pt(Crits,degree.freed),4),"P-value/2:",round(1-pt(Obsa,degree.freed),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
      
      x = seq(from = Crits, to = max(tGrid), length.out = 1000) 
      y = tPlot(x,df=degree.freed)
      
      x = c(Crits, x, max(tGrid), Crits) 
      y = c(0, y, 0, 0)
      
      polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      
      xp = seq(from = Obsa, to = max(tGrid), length.out = 1000) 
      yp = tPlot(xp,df=degree.freed)
      xp = c(Obsa, xp, 10, Obsa) 
      yp = c(0, yp, 0, 0)
      lines(xp,yp, lwd=4,  col = "red")
      
      
      legend(x = "topleft", inset=.05, legend = c(c("alpha/2:", round(pt(-Crits,degree.freed),4),"P-value/2:",round(pt(-Obsa,degree.freed),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
      
      if(max(yp)<=max(y)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      #if(1-pt(Obs,degree.freed) >= 1-pt(Crit,degree.freed))
      if(max(yp)>max(y)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
      #################
      
      
      xp1 = seq(from = min(tGrid), to = -Obsa, length.out = 1000) 
      yp1 = tPlot(xp1,df=degree.freed)
      xp1 = c(-Obsa,min(tGrid), xp1, -Obsa) 
      
      yp1 = c(yp1,0, 0, 0)
      lines(xp1,yp1, lwd=4,  col = "red")
      
      x1 = seq(from = min(tGrid), to = -Crits, length.out = 1000) 
      y1 = tPlot(x1,df=degree.freed)
      x1 = c(-Crits,min(tGrid), x1, -Crits) 
      
      y1 = c(y1,0, 0, 0)
      polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
      
      
      if(max(yp1)<=max(y1)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      if(max(yp1)>max(y1)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
      # if(pt(Obs,degree.freed)>=pt(Crit,degree.freed))
    }
    
    if((alternative=="two.sided")&(method=="values")){
      Crits<-abs(Crit)
      Obsa<-abs(Obs)
      
      points(Crit,0, col = "blue", pch = 19)
      points(Obs,0, col = "red", pch = 19)
      legend(x = "topleft", inset=.05, legend =c(c("Crit",Crit), c("Obs",Obs)), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n")
      
      
      if(Obsa>=Crits){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(Obsa<Crits){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
    }
    
    
    
  }
  
}




