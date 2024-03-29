#' Draw the area under a standardized Normal density curve
#'
#' @param cs by default is for standard Normal distribution; if it is *cs="t"* than it is referring to a t-Student
#' @param lower.x the lower bound for a standardized Normal distribution, by default it is equal to -4
#' @param upper.x the upper bound for a standardized Normal distribution, by default it is equal to 4
#' @param mu the population mean, by default is equal to zero
#' @param sigma the standarddeviation of the Normal; by default it is equal to 1
#' @param col it is the color of the area under the curve,  by default it is light blue: it can be changed
#' @param titles it the title on the top of the plot
#' @param density draw the kernel density function from the data
#' @return the area under the standardized Normal distribution
#' @export
#' @examples
#' library(BAStat)
#' #P(-1<Z<1)
#' area(lower.x=-1, upper.x=1, mu=0, sigma=1,col="light blue",titles="Area")
#' #P(Z<1)
#' area(upper.x=1)
#' #P(Z>1)
#' area(lower.x = 1)


area <- function(lower.x=-4, upper.x=4, mu=0, sigma=1,col="light blue",density=NULL,titles="Area"){
  step <- (upper.x - lower.x) / 100
  bounds <- c(mu-3*sigma, mu+3*sigma)
  cord.x <- c(lower.x,seq(lower.x,upper.x,step),upper.x)
  cord.y <- c(0,dnorm(seq(lower.x,upper.x,step),mu,sigma),0)
  curve(dnorm(x,mu,sigma),xlim=bounds,xlab = "",ylab = "",bty="n",main=titles) 
  polygon(cord.x,cord.y,col=col,density = density)
}
