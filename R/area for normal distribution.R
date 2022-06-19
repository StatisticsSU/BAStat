#' Draw the area under the Normal density curve
#'
#' @param lower.x the lower bound for a Normal distribution, by default it is equal to -4
#' @param upper.x the upper bound for a Normal distribution, by default it is equal to 4
#' @param mu the population mean, by default is equal to zero
#' @param sigma the standarddeviation of the Normal; by default it is equal to 1
#' @param col it is the color of the area under the curve,  by default it is light blue: it can be changed
#' @param titles it the title on the top of the plot
#' @return the area under the standardized Normal distibution
#' @export
#' @examples
#' library(BAStat)
#' #P(-4<Z<4)
#' area(lower.x=-4, upper.x=4, mu=0, sigma=1,col="light blue",titles="Area")
#' #P(Z<2)
#' area(upper.x=2)
#' #P(Z>3)
#' area(lower.x=3)
#' #P(X>1) and X is a Normal distribution with mean equal to 3 and variance equal to 2
#' area(lower.x=1,mu=3,sigma=sqrt(2))

area <- function(lower.x=-4, upper.x=4, mu=0, sigma=1,col="light blue",density=NULL,titles="Area"){
  step <- (upper.x - lower.x) / 100
  bounds <- c(mu-3*sigma, mu+3*sigma)
  cord.x <- c(lower.x,seq(lower.x,upper.x,step),upper.x)
  cord.y <- c(0,dnorm(seq(lower.x,upper.x,step),mu,sigma),0)
  curve(dnorm(x,mu,sigma),xlim=bounds,xlab = "",ylab = "",bty="n",main=titles) 
  polygon(cord.x,cord.y,col=col,density = density)
}
