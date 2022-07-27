#' Compute the density function if you have the cumulative distribution function for a continuous variable 
#' 
#' @param Fx the cumulative distribution function (cdf) or the generic function to be derivated.
#' @param lower the numerical lower bound of the function.
#' @param upper the numerical upper bound of the function. 
#' @param lower.x it can be equal to lower or a different value.  
#' @param upper.x it can be equal to upper or a different value.
#' @return On R Studio's Console: the numerical result of cdf in the bounds and the density function producted by your initial cdf 
#' and on the plots area the plot of the region representing the computed area.
#' @export 
#' @examples
#' 
#' library(BAStat)
#' 
#' cdfContinous(Fx=function(x)1/4 * x^2,lower = 0,upper=2,lower.x = 1,upper.x = 2)
#' cdfContinous(Fx=function(x)1-exp(-x^2),lower = 0,upper=10,lower.x = 0,upper.x = 10)#10 is a fictive limit
#' cdfContinous(Fx=function(x)x-1/2,lower=0,upper=2,lower.x = 1,upper.x = 1.5)



cdfContinous<-function(Fx,lower,upper,lower.x,upper.x){
  step <- (upper.x - lower.x) / 100
  cord.x <- c(lower.x,seq(lower.x,upper.x,step),upper.x)
  cord.y <- c(0,Fx(seq(lower.x,upper.x,step)),0)
  curve(Fx,xlim=c(lower,upper),xlab = "",ylab = "",bty="n") 
  polygon(cord.x,cord.y,col=2,density = 15)
  deriv<-(call_args(fn_body(Fx)))[[1]]
  cat("\nP(X<=xupp) or F(X=xupp)\n------------------------------------------------\n");
  print(Fx(upper.x), digits = 5, na.print = "")
  cat("\nP(X<=xlow) or F(X=xlow)\n------------------------------------------------\n");
  print(Fx(lower.x), digits = 5, na.print = "")
  cat("\nP(xlow<=X<=xupp) or F(X=xupp)-F(X=xlow)\n------------------------------------------------\n");
  print(Fx(upper.x)-Fx(lower.x), digits = 5, na.print = "")
  cat("\ndensity function\n------------------------------------------------\n");
  print(D(deriv,"x"))
  
}