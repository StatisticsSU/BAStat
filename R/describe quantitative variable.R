#' The aim of this function is to show how to represent a quantitative (discrete or continuous) variable numerically and graphically at the same time.
#' @param x the data as a numerical vector
#' @param plot histogram as default diagram. It is possible to show a boxplot with the mean and the median on the plot. 
#' @param place is for the legend to placed on top left, top right, bottom right or bottom left.
#' @param title_x is for the title of the plot
#' @param palette_x is for the color of the plot. Default color is light blue but it can be changed
#' @param breaks_x is for the histogram: in order to choose the number of groups in it.
#' @return Plot and table for one quantitative variable.
#' @export
#' @examples
#' library(SUdatasets)
#' str(smoking)
#' describeQT(smoking$Age,plot="boxplot",title_x = "Age")
#' describeQT(smoking$Age,plot="histogram",breaks=5,title_x = "Age")

describeQT<-function(x,x_lab="",title_x="",palette_x="light blue",breaks_x=ceiling(log2(length(x)) + 1),plot="histogram"){
  
  
  x2 <- seq(min(x), max(x), length = length(x))
  
  # Normal curve
  fun <- dnorm(x2, mean = mean(x), sd = sd(x))
  
  # Histogram
  if(plot=="histogram"){
    
    
    p_hist = hist(x, plot = F)
    hist(x, prob = TRUE, col = palette_x,xlab=x_lab,main=title_x,breaks=breaks_x,las=1,
         ylim = c(0, max(p_hist$density, fun)))
    
    lines(x2, fun, col = "red", lwd = 2)# Density
    lines(density(x), col = "blue", lwd = 2) #normal
    legend("bottomright", c("Histogram", "Normal", "Density"), xpd=TRUE, inset=c(0,1), cex=0.7,bty="n",
           pch=20, col = c("black", "red", "blue"))
    
  }
  if(plot=="boxplot"){
    #   layout(matrix(1:2, nrow = 2))
    #    hist(x, prob = TRUE, col = palette_x, xlab=x_lab,main=title_x,breaks=breaks_x,
    #         ylim = c(0, max(p_hist$density, fun)))
    
    #    lines(x2, fun, col = "red", lwd = 2)# Density
    #    lines(density(x), col = "blue", lwd = 2) #normal
    #    legend("bottomright", c("Histogram", "Normal", "Density"), xpd=TRUE, inset=c(0,1), cex=0.7,bty="n",
    #           pch=20, col = c("black", "red", "blue"))
    
    boxplot(x,  xlab = x_lab, main = title_x, col = palette_x,las=1)  
    points(mean(x),pch=20,col="red")
    
  }
  
  
  
  
  #if(box=="only"){
  #  boxplot(x,  xlab = "", main = "",horizontal = TRUE, col = palette_x)  
  #   }
  
  
  
  n    =length(x)
  quants = round(quantile(x, na.rm = TRUE), 4)
  mean =  round(mean(x, na.rm = TRUE), 4)
  sd   = round(sd(x, na.rm = TRUE), 4)
  se   = round(sd / sqrt(n), 4)
  
  
  tab<-c(n,quants,mean,sd,se)
  names(tab)<-c("n","min","1st Q.","Median","3rd Q.","Max","Mean","sd","se")
  # txx<-t(xx) 
  
  
  cat("\nDescriptive statistics for quantitative variable\n------------------------------------------------\n")
  
  
  
  return(tab)
  
  
}
