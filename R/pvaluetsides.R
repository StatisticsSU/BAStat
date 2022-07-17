#' Compute p-values and take decisions under a t-distribution
#' 
#' This function helps in the learning process: when the null hypothesis can be rejected based on the p-value. It is implemented for one and two tailed t-tests. It is for the test of hypothesis for the mean of one or two populations as well as a support for the estimated parameters of a regression model.
#' @param tCrit critical value
#' @param tObs observed value already studentised
#' @param degree.freed it is the number of degree of freedom
#' @param alternative `two.sided` if the alternative hypothesis is on both sides; `greater` if the alternative hypothesis is on the right side; `less` if the alternative hypothesis is on the left side
#' @return plot with the comparison between the theoretical area for the critical region and the obtained p-value. On the top of the plot is reported the answer: if it has to be rejected of not the null hypothesis. On the left side of the plot is reported the computed p-value and the computed area for the theoretical t-value.
#' @export 
#' @examples
#' library(BAStat)
#' tCrit =qt(0.95,18)
#' tPvalues(tCrit = 1.734064,tObs= 3.0866537,degree.freed=18,alternative="greater")
#' tCrit =qt(0.05,18)
#' tPvalues(tCrit = -1.734064,tObs= -3.0866537,degree.freed=18,alternative="less")
#' tCrit =qt(0.025,18)
#' tPvalues(tCrit = -2.100922,tObs= -3.0866537,degree.freed=18,alternative="two.sided")
#' tCrit =qt(0.975,18)
#' tPvalues(tCrit = 2.100922,tObs= 3.0866537,degree.freed=18,alternative="two.sided")

tPvalues <- function(tCrit, tObs, degree.freed, alternative){
  
  tGrid= seq(-10,10,by = 0.01)
  
  
  plot(tGrid, (dt(tGrid,df=degree.freed)), type = "l", xlab = "X", ylab = "t-Student density",lwd=1)
  
  tPlot = function(x,df=degree.freed){dt(x,df=degree.freed)}  
  
  
  if(alternative=="less"){
    
    legend(x = "topleft", inset=.05, legend = c(c("alpha:", round(pt(tCrit,degree.freed),4),"P-value:",round(pt(tObs,degree.freed),4))), pch = c(19,19),
           cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
    
    x1 = seq(from = min(tGrid), to = tCrit, length.out = 1000) 
    y1 = tPlot(x1,df=degree.freed)
    x1 = c(tCrit,min(tGrid), x1, tCrit) 
    
    y1 = c(y1,0, 0, 0)
    polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
    
    
    xp1 = seq(from = min(tGrid), to = tObs, length.out = 1000) 
    yp1 = tPlot(xp1,df=degree.freed)
    xp1 = c(tObs,min(tGrid), xp1, tObs) 
    
    yp1 = c(yp1,0, 0, 0)
    lines(xp1,yp1, lwd=4,  col = "red")
    
    
    if(max(yp1)<=max(y1)){
      mtext(~italic("Reject null hypothesis"), side=3)
    }
    
    if(max(yp1)>max(y1)){
      mtext(~italic("No Reject null hypothesis"), side=3)
    }
    
  }
  
  
  if(alternative=="greater"){
    legend(x = "topleft", inset=.05, legend = c(c("alpha:", round(1-pt(tCrit,degree.freed),4),"P-value:",round(1-pt(tObs,degree.freed),4))), pch = c(19,19),
           cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
    
    
    x = seq(from = tCrit, to = max(tGrid), length.out = 1000) 
    y = tPlot(x,df=degree.freed)
    
    x = c(tCrit, x, max(tGrid), tCrit) 
    y = c(0, y, 0, 0)
    
    polygon(x,y, lty = 3, border = NULL, col = "Light blue")
    
    xp = seq(from = tObs, to = max(tGrid), length.out = 1000) 
    yp = tPlot(xp,df=degree.freed)
    xp = c(tObs, xp, 10, tObs) 
    yp = c(0, yp, 0, 0)
    lines(xp,yp, lwd=4,  col = "red")
    
    
    if(max(yp)<=max(y)){
      mtext(~italic("Reject null hypothesis"), side=3)
    }
    if(max(yp)>max(y)){
      mtext(~italic("No Reject null hypothesis"), side=3)
    }
    
    
    
  }
  
  if(alternative=="two.sided"){
    
    
    tCrits<-abs(tCrit)
    tObsa<-abs(tObs)
    
    legend(x = "topleft", inset=.05, legend = c(c("alpha/2:", round(1-pt(tCrits,degree.freed),4),"P-value/2:",round(1-pt(tObsa,degree.freed),4))), pch = c(19,19),
           cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
    
    x = seq(from = tCrits, to = max(tGrid), length.out = 1000) 
    y = tPlot(x,df=degree.freed)
    
    x = c(tCrits, x, max(tGrid), tCrits) 
    y = c(0, y, 0, 0)
    
    polygon(x,y, lty = 3, border = NULL, col = "Light blue")
    
    xp = seq(from = tObsa, to = max(tGrid), length.out = 1000) 
    yp = tPlot(xp,df=degree.freed)
    xp = c(tObsa, xp, 10, tObsa) 
    yp = c(0, yp, 0, 0)
    lines(xp,yp, lwd=4,  col = "red")
    
    
    legend(x = "topleft", inset=.05, legend = c(c("alpha/2:", round(pt(-tCrits,degree.freed),4),"P-value/2:",round(pt(-tObsa,degree.freed),4))), pch = c(19,19),
           cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
    
    if(max(yp)<=max(y)){
      mtext(~italic("Reject null hypothesis"), side=3)
    }
    
    #if(1-pt(tObs,degree.freed) >= 1-pt(tCrit,degree.freed))
    if(max(yp)>max(y)){
      mtext(~italic("No Reject null hypothesis"), side=3)
    }
    
    
    xp1 = seq(from = min(tGrid), to = -tObsa, length.out = 1000) 
    yp1 = tPlot(xp1,df=degree.freed)
    xp1 = c(-tObsa,min(tGrid), xp1, -tObsa) 
    
    yp1 = c(yp1,0, 0, 0)
    lines(xp1,yp1, lwd=4,  col = "red")
    
    x1 = seq(from = min(tGrid), to = -tCrits, length.out = 1000) 
    y1 = tPlot(x1,df=degree.freed)
    x1 = c(-tCrits,min(tGrid), x1, -tCrits) 
    
    y1 = c(y1,0, 0, 0)
    polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
    
    
    
    if(max(yp1)<=max(y1)){
      mtext(~italic("Reject null hypothesis"), side=3)
    }
    if(max(yp1)>max(y1)){
      mtext(~italic("No Reject null hypothesis"), side=3)
    }
    
    
  }
}