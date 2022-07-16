#' Compute p-values and take decisions under a t-distribution
#'
#' @param tCrit critical value
#' @param tObs observed value already studentised
#' @param degree.freed it is the number of degree of freedom
#' @param alternative `two.sided` if the alternative hypothesis is on both sides; `greater` if the alternative hypothesis is on the right side; `less` if the alternative hypothesis is on the left side
#' @return plot with the critical region under the t-Student hypothesis and the p-value
#' @export 
#' @examples
#' library(BAStat)
#' tCrit =qt(0.95,18)
#' tCrit =qt(0.975,18)
#' tCrit =qt(0.025,18)
#' pt(3.0866537,18)
#' qt(0.003181154,18)


#' t.pvalues(tCrit = 1.734064,tObs= 3.0866537,degree.freed=18,alternative="greater")
#' t.pvalues(tCrit = -1.734064,tObs= -3.0866537,degree.freed=18,alternative="less")

#' t.pvalues(tCrit = -2.100922,tObs= -3.0866537,degree.freed=18,alternative="two.sided")
#' t.pvalues(tCrit = 2.100922,tObs= 3.0866537,degree.freed=18,alternative="two.sided")

 t.pvalues <- function(tCrit, tObs, degree.freed, alternative){
  
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
    
    
    legend(x = "topleft", inset=.05, legend = c(c("alpha/2:", round(1-pt(tCrit,degree.freed),4),"P-value/2:",round(1-pt(tObs,degree.freed),4))), pch = c(19,19),
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
    
    
    legend(x = "topleft", inset=.05, legend = c(c("alpha/2:", round(pt(-tCrit,degree.freed),4),"P-value/2:",round(pt(-tObs,degree.freed),4))), pch = c(19,19),
           cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
    if(max(yp)<=max(y)){
      mtext(~italic("Reject null hypothesis"), side=3)
    }
    
    #if(1-pt(tObs,degree.freed) >= 1-pt(tCrit,degree.freed))
    if(max(yp)>max(y)){
      mtext(~italic("No Reject null hypothesis"), side=3)
    }
    
    #################
    
    
    
    xp1 = seq(from = min(tGrid), to = - tObs, length.out = 1000) 
    yp1 = tPlot(xp1,df=degree.freed)
    xp1 = c(-tObs,min(tGrid), xp1, -tObs) 
    
    yp1 = c(yp1,0, 0, 0)
    lines(xp1,yp1, lwd=4,  col = "red")
    
    x1 = seq(from = min(tGrid), to = -tCrit, length.out = 1000) 
    y1 = tPlot(x1,df=degree.freed)
    x1 = c(-tCrit,min(tGrid), x1, -tCrit) 
    
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





