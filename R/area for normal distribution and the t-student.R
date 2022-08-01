#' Draw the area under the standardized Normal density curve or the t-Student's curve. Both those functions are continous and symmetric, this is why the function is called "CS"
#'
#' @param cs by default is intended to draw the Standardized Normal distribution, otherwise specify *cs="t"* and it will for the t distribution
#' @param lower.x the lower bound of the symmetric and continuous distribution;
#' @param upper.x the upper bound of the symmetric and continuous distribution;
#' @param side it can be `left`, if the area is on the left side of the area; `right` if the area is on the right; `between` if the area is between two values
#' @param deg.fred is the degree of freedom and it would be required if it is the t-Student (cs="t")
#' @param cdf if it is true standard normal distribution is drawn with CDF
#' @return the area under the standardized Normal distribution or the CDF for the normal or the area for the t-Student
#' @export
#' @examples
#' library(BAStat)
#' 
#' CSareas(upper.x=1,side="left")#P(z<1)
#' CSareas(upper.x=1,side="left",cdf=TRUE)#P(z<1)
#' CSareas(upper.x=-1,side="left")#p(z<-1)
#' CSareas(upper.x=-1,side="left",cdf=TRUE)#p(z<-1)
#'
#' CSareas(upper.x=-1,side="between")#wrong!
#' CSareas(upper.x=-1,side="right")#wrong!
#'
#' CSareas(lower.x=-1,side="right")#P(z>-1)
#' CSareas(lower.x=-1,side="right",cdf=TRUE)#F(-1)
#' CSareas(lower.x=1,side="right")#P(z>1)
#' CSareas(lower.x=1,side="right",cdf=TRUE)#F(1)

#' CSareas(lower.x=1,upper.x=2,side="between")#P(1<Z<2)
#' CSareas(lower.x=1,upper.x=2,side="between",cdf=TRUE)#F(2)-F(1)
#' CSareas(lower.x=-1,upper.x=2,side="between")#P(-1<Z<2)
#' CSareas(lower.x=-1,upper.x=2,side="between",cdf=TRUE)#F(2)-F(-1)
#' CSareas(lower.x=-2,upper.x=-1,side="between")#P(-2<Z<-1)
#' CSareas(lower.x=-2,upper.x=-1,side="between",cdf=TRUE)#F(-2)-F(-1)
#' 
#' CSareas(lower.x=2,upper.x=-1,side="between")#wrong!
#' CSareas(lower.x=-1,upper.x=-2,side="between")#wrong!

####
#t-Student

#' CSareas(cs="t",upper.x=2.49,deg.fred = 23,side="left")#P(t<2.49)
#' CSareas(cs="t",upper.x=-2.49,deg.fred = 23,side="left")#p(t<-2.49)
#' 
#' CSareas(cs="t",upper.x=-2.49,deg.fred = 23,side="between")#wrong!
#' CSareas(cs="t",upper.x=-2.49,deg.fred = 23,side="right")#wrong!
#' 
#' CSareas(cs="t",lower.x=-2.49,deg.fred = 23,side="right")#P(t>-2.49)
#' CSareas(cs="t",lower.x=2.49,deg.fred = 23,side="right")#P(t>2.49)
#' CSareas(cs="t",lower.x=-1.714,upper.x=1.714,deg.fred = 23,side="between") #P(-1.714<t<1.714)
#' CSareas(cs="t",lower.x=-2.49,upper.x=3,deg.fred = 23,side="between") #P(-2.49<t<3)
#' CSareas(cs="t",lower.x=-3,upper.x=-2.49,deg.fred = 23,side="between")#P(-3<t<-2.49)
#'
#' CSareas(cs="t",lower.x=2,upper.x=-1,deg.fred = 23,side="between")#wrong!
#' CSareas(cs="t",lower.x=-1,upper.x=-2,deg.fred = 23,side="between")#wrong!

#different areas with same bounds if it is normal or t-student we get -numerically-different areas
#' CSareas(lower.x=1,upper.x=2,side="between")
#' CSareas(cs="t",lower.x=1,upper.x=2,deg.fred=23,side="between")



CSareas<-function(cs="Standard Normal",lower.x=NULL, upper.x=NULL, side,deg.fred, cdf=FALSE){
  
  if(cs=="Standard Normal"){
    Grid= seq(-4,4,by = 0.01)
    plot(Grid, dnorm(Grid), type = "l", xlab = "z", ylab = "Standardized Normal density",lwd=1,xaxt="n")
    axis(1,seq(-4,4,by=1))
    normPlot = function(x){dnorm(x)}  
    
    if(side=="right"){
      if((lower.x>-4)&(lower.x<0)){
        upper.x<-4
        mtext(paste("area:", round(1-pnorm(lower.x),4)), side = 3)
        
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = normPlot(x)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      }
    }
    
    
    if(side=="right"){
      upper.x<-4
      if((lower.x>-4)&(lower.x>=0)){
        mtext(paste("area:", round(1-pnorm(lower.x),4)), side = 3)
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = normPlot(x)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
      }
    }
    
    if(side=="left"){
      lower.x<--4
      if((upper.x<4)&(upper.x<0)){
        
        x1 = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y1 = normPlot(x1)
        x1 = c(upper.x,min(Grid), x1, upper.x) 
        
        y1 = c(y1,0, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
        
        mtext(paste("area:", round(pnorm(upper.x),4)), side = 3)
        
      }
    }
    
    if(side=="left"){
      if((upper.x<4)&(upper.x>=0)){
        lower.x<--4
        mtext(paste("area:", round(pnorm(upper.x),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = normPlot(x)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
      }
    }
    
    
    if(side=="between"){
      if((lower.x>=0) & (upper.x>=0)){
        
        mtext(paste("area:", round((pnorm(upper.x)-pnorm(lower.x)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = normPlot(x)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = normPlot(x1)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
      
      if((lower.x<0) & (upper.x>=0)){
        
        mtext(paste("area:", round((pnorm(upper.x)-pnorm(lower.x)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = normPlot(x)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = normPlot(x1)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
        
      }
      
      
      if((lower.x<0) & (upper.x<0)&(lower.x<upper.x)){
        mtext(paste("area:", round((pnorm(upper.x)-pnorm(lower.x)),4)), side = 3)
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = normPlot(x)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = normPlot(x1)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
      
    }
    
    if(cdf==TRUE){
      
      x<-seq(from=-4,to=4,by=0.05)
      cumulative<-pnorm(x)
      plot(x, cumulative, col="darkorange", xlab="z",xaxt="n",
           ylab="Cumulative Probability",type="l",lwd=2, cex=2,
           main="Standard Normal CDF", cex.axis=.8)
      
      if(side=="right"){
        
        if((lower.x>-4)&(lower.x<0)){
          upper.x<-4
          mtext(paste("area:", round(1-pnorm(lower.x),4)), side = 3)
          
          p <- c(upper.x, pnorm(upper.x))#pnorm(1, mean = 0 , sd=1)
          segments(x0 = upper.x, y0 = lower.x, x1 = upper.x, y1 = pnorm(upper.x), col = "darkgreen",lty=2) 
          segments(x0 = upper.x, y0 = pnorm(upper.x), x1 = -4, y1 = pnorm(upper.x), 
                   col = "darkgreen",lty=2) 
          points(t(p), col="blue", pch=16)
          
          text(t(p), paste(round(pnorm(upper.x),4)), adj=-0.2)
          
          p2 <- c(lower.x, pnorm(lower.x))#pnorm(1, mean = 0 , sd=1)
          points(t(p2), col="blue", pch=16)
          text(t(p2), paste(round(pnorm(lower.x),4)), adj=-0.2)
          
          segments(x0 = lower.x, y0 = lower.x, x1 = lower.x, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          segments(x0 = lower.x, y0 = pnorm(lower.x), x1 = -4, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          axis(1,at=c(-4,-3,-2,-1,0,1,2,3,4))
          x1<-ifelse(x<=upper.x&x>=lower.x,x,NA)
          cum1<-ifelse(cumulative<=pnorm(upper.x)&cumulative>=pnorm(lower.x),cumulative,NA)
          p1<-cbind(x1,cum1)
          points(p1, col="blue", pch=16)
        }
      }
      
      if(side=="right"){
        upper.x<-4
        if((lower.x>-4)&(lower.x>=0)){
          
          mtext(paste("area:", round(1-pnorm(lower.x),4)), side = 3)
          
          p <- c(upper.x, pnorm(upper.x))#pnorm(1, mean = 0 , sd=1)
          segments(x0 = upper.x, y0 = lower.x, x1 = upper.x, y1 = pnorm(upper.x), col = "darkgreen",lty=2) 
          segments(x0 = upper.x, y0 = pnorm(upper.x), x1 = -4, y1 = pnorm(upper.x), 
                   col = "darkgreen",lty=2) 
          points(t(p), col="blue", pch=16)
          
          text(t(p), paste(round(pnorm(upper.x),4)), adj=-0.2)
          
          p2 <- c(lower.x, pnorm(lower.x))#pnorm(1, mean = 0 , sd=1)
          points(t(p2), col="blue", pch=16)
          text(t(p2), paste(round(pnorm(lower.x),4)), adj=-0.2)
          
          segments(x0 = lower.x, y0 = lower.x, x1 = lower.x, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          segments(x0 = lower.x, y0 = pnorm(lower.x), x1 = -4, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          axis(1,at=c(-4,-3,-2,-1,0,1,2,3,4))
          x1<-ifelse(x<=upper.x&x>=lower.x,x,NA)
          cum1<-ifelse(cumulative<=pnorm(upper.x)&cumulative>=pnorm(lower.x),cumulative,NA)
          p1<-cbind(x1,cum1)
          points(p1, col="blue", pch=16)
        }
      }
      
      if(side=="left"){
        
        if((upper.x<4)&(upper.x<0)){
          lower.x<--4
          mtext(paste("area:", round(pnorm(upper.x),4)), side = 3)
          
          p <- c(upper.x, pnorm(upper.x))#pnorm(1, mean = 0 , sd=1)
          segments(x0 = upper.x, y0 = lower.x, x1 = upper.x, y1 = pnorm(upper.x), col = "darkgreen",lty=2) 
          segments(x0 = upper.x, y0 = pnorm(upper.x), x1 = -4, y1 = pnorm(upper.x), 
                   col = "darkgreen",lty=2) 
          points(t(p), col="blue", pch=16)
          
          text(t(p), paste(round(pnorm(upper.x),4)), adj=-0.2)
          
          p2 <- c(lower.x, pnorm(lower.x))#pnorm(1, mean = 0 , sd=1)
          points(t(p2), col="blue", pch=16)
          text(t(p2), paste(round(pnorm(lower.x),4)), adj=-0.2)
          
          segments(x0 = lower.x, y0 = lower.x, x1 = lower.x, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          segments(x0 = lower.x, y0 = pnorm(lower.x), x1 = -4, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          axis(1,at=c(-4,-3,-2,-1,0,1,2,3,4))
          x1<-ifelse(x<=upper.x&x>=lower.x,x,NA)
          cum1<-ifelse(cumulative<=pnorm(upper.x)&cumulative>=pnorm(lower.x),cumulative,NA)
          p1<-cbind(x1,cum1)
          points(p1, col="blue", pch=16)
          
        }
      }
      
      if(side=="left"){
        if((upper.x<4)&(upper.x>=0)){
          lower.x<--4
          mtext(paste("area:", round((pnorm(upper.x)-pnorm(lower.x)),4)), side = 3)
          
          p <- c(upper.x, pnorm(upper.x))#pnorm(1, mean = 0 , sd=1)
          segments(x0 = upper.x, y0 = lower.x, x1 = upper.x, y1 = pnorm(upper.x), col = "darkgreen",lty=2) 
          segments(x0 = upper.x, y0 = pnorm(upper.x), x1 = -4, y1 = pnorm(upper.x), 
                   col = "darkgreen",lty=2) 
          points(t(p), col="blue", pch=16)
          
          text(t(p), paste(round(pnorm(upper.x),4)), adj=-0.2)
          
          p2 <- c(lower.x, pnorm(lower.x))#pnorm(1, mean = 0 , sd=1)
          points(t(p2), col="blue", pch=16)
          text(t(p2), paste(round(pnorm(lower.x),4)), adj=-0.2)
          
          segments(x0 = lower.x, y0 = lower.x, x1 = lower.x, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          segments(x0 = lower.x, y0 = pnorm(lower.x), x1 = -4, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          axis(1,at=c(-4,-3,-2,-1,0,1,2,3,4))
          x1<-ifelse(x<=upper.x&x>=lower.x,x,NA)
          cum1<-ifelse(cumulative<=pnorm(upper.x)&cumulative>=pnorm(lower.x),cumulative,NA)
          p1<-cbind(x1,cum1)
          points(p1, col="blue", pch=16)
          
        }
      }
      
      if(side=="between"){
        if((lower.x>=0) & (upper.x>=0)){
          
          mtext(paste("area:", round((pnorm(upper.x)-pnorm(lower.x)),4)), side = 3)
          
          p <- c(upper.x, pnorm(upper.x))#pnorm(1, mean = 0 , sd=1)
          segments(x0 = upper.x, y0 = lower.x, x1 = upper.x, y1 = pnorm(upper.x), col = "darkgreen",lty=2) 
          segments(x0 = upper.x, y0 = pnorm(upper.x), x1 = -4, y1 = pnorm(upper.x), 
                   col = "darkgreen",lty=2) 
          points(t(p), col="blue", pch=16)
          
          text(t(p), paste(round(pnorm(upper.x),4)), adj=-0.2)
          
          p2 <- c(lower.x, pnorm(lower.x))#pnorm(1, mean = 0 , sd=1)
          points(t(p2), col="blue", pch=16)
          text(t(p2), paste(round(pnorm(lower.x),4)), adj=-0.2)
          
          segments(x0 = lower.x, y0 = lower.x, x1 = lower.x, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          segments(x0 = lower.x, y0 = pnorm(lower.x), x1 = -4, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          axis(1,at=c(-4,-3,-2,-1,0,1,2,3,4))
          x1<-ifelse(x<=upper.x&x>=lower.x,x,NA)
          cum1<-ifelse(cumulative<=pnorm(upper.x)&cumulative>=pnorm(lower.x),cumulative,NA)
          p1<-cbind(x1,cum1)
          points(p1, col="blue", pch=16)
        }
        
        if((lower.x<0) & (upper.x>=0)){
          
          mtext(paste("area:", round((pnorm(upper.x)-pnorm(lower.x)),4)), side = 3)
          
          p <- c(upper.x, pnorm(upper.x))
          segments(x0 = upper.x, y0 = lower.x, x1 = upper.x, y1 = pnorm(upper.x), col = "darkgreen",lty=2) 
          segments(x0 = upper.x, y0 = pnorm(upper.x), x1 = -4, y1 = pnorm(upper.x), 
                   col = "darkgreen",lty=2) 
          points(t(p), col="blue", pch=16)
          text(t(p), paste(round(pnorm(upper.x),4)), adj=-0.2)
          p2 <- c(lower.x, pnorm(lower.x))
          points(t(p2), col="blue", pch=16)
          text(t(p2), paste(round(pnorm(lower.x),4)), adj=-0.2)
          segments(x0 = lower.x, y0 = lower.x, x1 = lower.x, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          segments(x0 = lower.x, y0 = pnorm(lower.x), x1 = -4, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          axis(1,at=c(-4,-3,-2,-1,0,1,2,3,4))
          x1<-ifelse(x<=upper.x&x>=lower.x,x,NA)
          cum1<-ifelse(cumulative<=pnorm(upper.x)&cumulative>=pnorm(lower.x),cumulative,NA)
          p1<-cbind(x1,cum1)
          points(p1, col="blue", pch=16)
          
        }
        
        
        
        if((lower.x<0) & (upper.x<0)&(lower.x<upper.x)){
          
          mtext(paste("area:", round((pnorm(upper.x)-pnorm(lower.x)),4)), side = 3)
          
          p <- c(upper.x, pnorm(upper.x))#pnorm(1, mean = 0 , sd=1)
          segments(x0 = upper.x, y0 = lower.x, x1 = upper.x, y1 = pnorm(upper.x), col = "darkgreen",lty=2) 
          segments(x0 = upper.x, y0 = pnorm(upper.x), x1 = -4, y1 = pnorm(upper.x), 
                   col = "darkgreen",lty=2) 
          points(t(p), col="blue", pch=16)
          text(t(p), paste(round(pnorm(upper.x),4)), adj=-0.2)
          p2 <- c(lower.x, pnorm(lower.x))#pnorm(1, mean = 0 , sd=1)
          points(t(p2), col="blue", pch=16)
          text(t(p2), paste(round(pnorm(lower.x),4)), adj=-0.2)
          segments(x0 = lower.x, y0 = lower.x, x1 = lower.x, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          segments(x0 = lower.x, y0 = pnorm(lower.x), x1 = -4, y1 = pnorm(lower.x), col = "darkgreen",lty=2) 
          axis(1,at=c(-4,-3,-2,-1,0,1,2,3,4))
          x1<-ifelse(x<=upper.x&x>=lower.x,x,NA)
          cum1<-ifelse(cumulative<=pnorm(upper.x)&cumulative>=pnorm(lower.x),cumulative,NA)
          p1<-cbind(x1,cum1)
          points(p1, col="blue", pch=16)
          
        }
        
        
      }
      
    } 
    
    
    
    
    
    
  }
  
  
  if(cs=="t"){
    
    
    
    Grid= seq(-4,4,by = 0.01)
    
    plot(Grid, dt(Grid,df=deg.fred), type = "l", xlab = "t", ylab = "t-Student density",lwd=1,xaxt="n")
    axis(1,seq(-4,4,by=1))
    normPlot = function(x,df){dt(x,df=deg.fred)}  
    
    if(side=="right"){
      if((lower.x>-4)&(lower.x<0)){
        upper.x<-4
        mtext(paste("area:", round(1-pt(lower.x,df=deg.fred),4)), side = 3)
        
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = normPlot(x)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      }
    }
    
    
    if(side=="right"){
      upper.x<-4
      if((lower.x>-4)&(lower.x>=0)){
        mtext(paste("area:", round(1-pt(lower.x,df=deg.fred),4)), side = 3)
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = normPlot(x)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
      }
    }
    
    if(side=="left"){
      lower.x<--4
      if((upper.x<4)&(upper.x<0)){
        
        x1 = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y1 = normPlot(x1)
        x1 = c(upper.x,min(Grid), x1, upper.x) 
        
        y1 = c(y1,0, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
        
        mtext(paste("area:", round(pt(upper.x,df=deg.fred),4)), side = 3)
        
      }
    }
    
    if(side=="left"){
      if((upper.x<4)&(upper.x>=0)){
        lower.x<--4
        mtext(paste("area:", round(pt(upper.x,df=deg.fred),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = normPlot(x)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
      }
    }
    
    
    if(side=="between"){
      if((lower.x>=0) & (upper.x>=0)){
        
        mtext(paste("area:", round((pt(upper.x,df=deg.fred)-pt(lower.x,df=deg.fred)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = normPlot(x)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = normPlot(x1)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
      
      if((lower.x<0) & (upper.x>=0)){
        
        mtext(paste("area:", round((pt(upper.x,df=deg.fred)-pt(lower.x,df=deg.fred)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = normPlot(x)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = normPlot(x1)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
        
        
        
      }
      
      
      if((lower.x<0) & (upper.x<0)&(lower.x<upper.x)){
        
        mtext(paste("area:", round((pt(upper.x,df=deg.fred)-pt(lower.x,df=deg.fred)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = normPlot(x)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = normPlot(x1)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
    }
    
  }
  
  
  
}
