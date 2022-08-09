#' This small function is for standardizing the observed value before the test for one parameter
#' 
#' There are several functions in R for those computations and in particular for the proportion there are many formulas for the correction to normality and decentralizing the attention to the the initial formula and the power of the central limit theorem.
#'
#' @param x the numerical variable to be standardized
#' @param parameter the mean or the proportion
#' @param binom if it is TRUE than the observed value will be the proportion from the observations
#' @param n number of observations; it would be needed only if it is supposed to refer to the Binomial distribution
#' @return the standardized observed value for the test of one parameter
#' @export
#' @examples
#' library(BAStat)
#' 
#' ex<-c(2970,3020,3005,2900,2940,2925)
#' observed(ex,parameter=3000)
#' 
#' x<-10
#' n<-160
#' parameter<-0.08
#' observed(x=10,parameter=0.08,binom=T,n=160)

observed<-function(x,parameter,binom=F,n=NULL){
  
  num<-mean(x)-parameter
  nn<-length(x)
  denom<-sqrt(var(x)/nn)
  value<-num/denom
  
  if(binom){
    phat<-(x/n)
    value<-(phat-parameter)/sqrt((parameter*(1-parameter))/n)
    
  }
  return(value)
  
}


