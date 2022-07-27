#' Compute defined integral and draw the area that is referred to
#' 
#' The goal of this function is to compute defined integral and to show the corresponding area. 
#' Note that this function can be adapted also for indefinite integrals (integrals between -Inf and Inf) but a tolerance equal to 1e-06 is introduced.
#' @param fx the integrand, the mathematical function to be integrated.
#' @param lower the numerical lower bound of the integral.
#' @param upper the numerical upper bound of the integral. 
#' @return On R Studio's Console: the numerical result of the integral and on the plots area the plot of the region representing the computed area.
#' @export 
#' @examples
#' 
#' library(BAStat)
#' 
#' integrand<-function(x){3*x^4}
#' integPlot(fx=integrand,lower = 0,upper = 2)
#' integrand<-function(x){x*exp(x)}
#' integPlot(fx=integrand,lower = 0,upper = 1)
#'
#' integrand <- function(x) {1/sqrt(2*pi)*exp(-x^2/2)} #this is the Normal distribution
#' integPlot(fx=integrand,lower = -1.96,upper = 1.96)
#' 
#' integPlot(fx=function(x)1/4 * x^2,lower = 0,upper = 2)
#' integPlot(fx=function(x)1/4 * x^2,lower = -Inf,upper = Inf)# indefinite integral
#' integPlot(fx=function(x)1/4 * x^2,lower = -Inf,upper = 2) # improper integral: one of the bounds is infinity
#' integPlot(fx=function(x)1/4 * x^2,lower = 0,upper = Inf)# improper integral 


integPlot<-function(fx,lower,upper){
  
  if(lower==-Inf){lower<--1e-06}
  if(upper==Inf){upper<-1e-06}
  
  x<-seq(lower,upper,by=0.1)
  
  plot(x,fx(x),type="l")
  x3 <- c(lower, x, upper)
  y2<-fx(x)
  y3 <- c(0, y2, 0)
  
  
  polygon(x3,
          y3,density = 15, angle = 45,
          col = "darkgreen")
  return(integrate(Vectorize(fx),lower,upper))
}

