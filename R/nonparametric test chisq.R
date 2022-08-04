#' plot and decide for nonparametric test based on chi-squared distribution with p-values and area in comparison
#' 
#' @param Crit is the quantile on the right side of the chi-squared distribution (it can be known or computed right away)
#' @param Obs observed value from chi-square computation
#' @param df degree of freedom
#' @return two plots: on the left side the chi-squared distribution and the comparison between the theoretical and observed value; on the right side it is computed the same chi-squared but focusing on the areas. In this plot on the right side there is the p-value and the theoretical alpha-value and on top there is the decision: if it can be rejected or not the null hypothesis.  
#' @export
#' @examples
#' library(BAStat)
#'
#' df<-3
#' alpha<-0.025
#' Crit<-qchisq(1-alpha,df)
#' Obs<-5
#' alpha<-0.05
#' alpha<-0.10
#' 
#' CAnonparam(Crit=7.81,Obs=10,df=3)
#' CAnonparam(Crit=qchisq(1-alpha,df),Obs=10,df=3)
#' CAnonparam(Crit=qchisq(1-alpha,df),Obs=18,df=3)
#' CAnonparam(Crit=qchisq(1-alpha,df),Obs=8,df=3)
#' CAnonparam(Crit=qchisq(1-alpha,df),Obs=7,df=3)
#' 
#' #let's combine with a test
#' observations<- rbind(c(15,25,20),c(25,5,10))             
#' chisq.test(observations)
#' alpha=0.05 
#' CAnonparam(Crit=qchisq(1-alpha,df=2),Obs=2.5,df=2)



CAnonparam<-function(Crit,Obs,df){
  
  dch <- function(x,df=df)dchisq(x,df=df)
  
  pCH <- function(q,df)integrate(dch,q,Inf,df=df)$value
  
  max.x<-max(Crit,Obs)+5
  min.x<-min(Crit,Obs)-5
  par(mfrow=c(1,2))
  max.x<-max(Crit,Obs)+5
  Grid= seq(0,max.x,by = 0.01)
  plot(Grid, dch(Grid,df=df), type = "l", xlab = expression(chi^2), ylab = "Chi-squared density",main="Nonparametric test",lwd=1,xaxt="n")
  axis(1,seq(0,max.x,by=1))
  legend(x = "topright", inset=.05, legend = c(c("Crit:", round(Crit,4),"Obs:",round(Obs,4))), pch = c(19,19),
         cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n")
  
  points(Crit,0, col = "blue", pch = 19)
  points(Obs,0, col = "red", pch = 19)
  
  x = seq(from = Crit, to = max(Grid), length.out = 1000) 
  y = dch(x,df=df)
  
  plot(x, y, type = "l", xlab = expression(chi^2), ylab = "Zoom right tail",lwd=1,xaxt="n")
  axis(1,seq(Crit,max(Grid),by=1))
  x = c(Crit, x, max(Grid), Crit) 
  y = c(0, y, 0, 0)
  legend(x = "topright", inset=.05, legend = c(c("alpha:", round(pCH(Crit,df),4),"P-value:",round(pCH(Obs,df),6))), pch = c(19,19),
         cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n")
  
  polygon(x,y, lty = 3, border = NULL, col = "Light blue")
  
  xp = seq(from = Obs, to = max(Grid), length.out = 1000) 
  yp = dch(xp,df=df)
  xp = c(Obs, xp, 30, Obs) 
  yp = c(0, yp, 0, 0)
  polygon(xp,yp,  col = "red")
  
  
  if(max(yp)<=max(y)){
    mtext(~italic("Reject null hypothesis"), side=3)
  }
  if(max(yp)>max(y)){
    mtext(~italic("No Reject null hypothesis"), side=3)
  }
  
  
}





