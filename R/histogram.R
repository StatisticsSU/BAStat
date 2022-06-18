#' Histogram for one variable 
#'
#' @param x quantitative variable (continous or discrete)
#' @param x_lab specify the name of the variable on x-axis
#' @param palette_x define the color of your histogram or by default is "light blue"
#' @param breaks_x define the number of bins; by default there is the suggested number of bins from hist function
#' @param box if it is true it can produce a boxplot as well
#' As a result the histogram is with the Normal density curve and the kernel density from the observations 
#' @export 
#' @examples
#' library(BAStat)
#' x<-c(25,25,26,26,27,27,28,28,29,30,30,31)
#' histog(x)






histog<-function(x,x_lab="",title_x="",palette_x="light blue",breaks_x=ceiling(log2(length(x)) + 1),box=FALSE){
  
  
  x2 <- seq(min(x), max(x), length = length(x))
  
  # Normal curve
  fun <- dnorm(x2, mean = mean(x), sd = sd(x))
  
  # Histogram
  
  p_hist = hist(x, plot = F)
  hist(x, prob = TRUE, col = palette_x,xlab=x_lab,main=title_x,breaks=breaks_x,
       ylim = c(0, max(p_hist$density, fun)))
  
  lines(x2, fun, col = "red", lwd = 2)# Density
  lines(density(x), col = "blue", lwd = 2) #normal
  legend("bottomright", c("Histogram", "Normal", "Density"), xpd=TRUE, inset=c(0,1), cex=0.7,bty="n",
         pch=20, col = c("black", "red", "blue"))
  if(box==TRUE){
par(mar=c(1,1,1,1))
    layout(matrix(1:2, nrow = 2))
    hist(x, prob = TRUE, col = palette_x, xlab=x_lab,main=title_x,breaks=breaks_x,
         ylim = c(0, max(p_hist$density, fun)))
    
    lines(x2, fun, col = "red", lwd = 2)# Density
    lines(density(x), col = "blue", lwd = 2) #normal
    legend("bottomright", c("Histogram", "Normal", "Density"), xpd=TRUE, inset=c(0,1), cex=0.7,bty="n",
           pch=20, col = c("black", "red", "blue"))
    
    boxplot(x,  xlab = "", main = "",horizontal = TRUE, col = palette_x)  
    
  }
  
  
  
}  



