#' Draw the area for continuous distributions: in this function is included the Standard Normal distribution and its CDF; t-Student, chi-squared and F-Fisher density on demand.
#'
#' @param distr by default is intended to draw the Standardized Normal distribution, otherwise specify *distr="t"* and it will be the t-distribution, but it has to be specified the degrees of freedom. 
#' If *distr="chi.sq"* it is the chi-squared between 0 and 30 be drawn. Also in this case it has to be specified the number of degrees of freedom. If *distr="Fisher"* it will be density of the F-Fisher and in that case it would be needed to specify two degrees of freedom `deg.fred1` and `deg.fred2` 
#' @param lower.x the lower bound of the symmetric and continuous distribution;
#' @param upper.x the upper bound of the symmetric and continuous distribution;
#' @param side it can be `left`, if the area is on the left side of the area; `right` if the area is on the right; `between` if the area is between two values
#' @param deg.fred is the degree of freedom and it would be required if it is the t-Student (distr="t") or chi-squared distribution (distr="chi.sq")
#' @param cdf if it is true, then the CDF of a Standard Normal distribution will be drawn.
#' @param value.x it is for one side chi-squared or F-Fisher distribution
#' @param deg.fred1 and deg.fred2 have to be specified only for the Fisher
#' @return the area under the standardized Normal distribution or the CDF for the normal or the area for the t-Student, chi-squared or F-Fisher density
#' @export
#' @examples
#' library(BAStat)
#' 
#' CdistrAreas(upper.x=1,side="left")#P(z<1)
#' CdistrAreas(upper.x=-1,side="left")#p(z<-1)
#' 
#' CdistrAreas(upper.x=1,side="left",cdf=T)#P(z<1)
#' CdistrAreas(upper.x=-1,side="left",cdf=T)#p(z<-1)
#' 
#' CdistrAreas(upper.x=-1,side="between")#wrong!
#' CdistrAreas(upper.x=-1,side="right")#wrong!
#' 
#' CdistrAreas(lower.x=-1,side="right")#P(z>-1)
#' CdistrAreas(lower.x=-1,side="right",cdf=T)#P(z>-1)
#' CdistrAreas(lower.x=1,side="right")#P(z>1)
#' CdistrAreas(lower.x=1,side="right",cdf=T)#P(z>1)
#' 
#' CdistrAreas(lower.x=1,upper.x=2,side="between")
#' CdistrAreas(lower.x=-1,upper.x=2,side="between")
#' CdistrAreas(lower.x=-2,upper.x=-1,side="between")
#' 
#' #cdf standard Normal distribution
#' CdistrAreas(lower.x=1,upper.x=2,side="between",cdf=T)
#' CdistrAreas(lower.x=-1,upper.x=2,side="between",cdf=T)
#' CdistrAreas(lower.x=-2,upper.x=-1,side="between",cdf=T)
#' 
#' CdistrAreas(lower.x=2,upper.x=-1,side="between")#wrong!
#' CdistrAreas(lower.x=-1,upper.x=-2,side="between")#wrong!

#' t-Student
#' 
#' CdistrAreas(distr="t",upper.x=2.49,deg.fred = 23,side="left")#P(t<2.49)
#' CdistrAreas(distr="t",upper.x=-2.49,deg.fred = 23,side="left")#p(t<-2.49)
#' CdistrAreas(distr="t",upper.x=-2.49,deg.fred = 23,side="between")#wrong!
#' CdistrAreas(distr="t",upper.x=-2.49,deg.fred = 23,side="right")#wrong!
#' 
#' CdistrAreas(distr="t",lower.x=-2.49,deg.fred = 23,side="right")#P(t>-2.49)
#' CdistrAreas(distr="t",lower.x=2.49,deg.fred = 23,side="right")#P(t>2.49)
#' 
#' 
#' CdistrAreas(distr="t",lower.x=-1.714,upper.x=1.714,deg.fred = 23,side="between") #P(-1.714<t<1.714)
#' CdistrAreas(distr="t",lower.x=-2.49,upper.x=3,deg.fred = 23,side="between") #P(-2.49<t<3)
#' CdistrAreas(distr="t",lower.x=-3,upper.x=-2.49,deg.fred = 23,side="between")#P(-3<t<-2.49)
#' 
#' CdistrAreas(distr="t",lower.x=2,upper.x=-1,deg.fred = 23,side="between")#wrong!
#' CdistrAreas(distr="t",lower.x=-1,upper.x=-2,deg.fred = 23,side="between")#wrong!
#' 
#' #different areas with same bounds if it is normal or t-student distribution, we get -numerically-different areas
#' CdistrAreas(lower.x=1,upper.x=2,side="between")
#' CdistrAreas(distr="t",lower.x=1,upper.x=2,deg.fred=23,side="between")
#' 
#' #Chisq-different plots are produced in order to see the tail on the right hand side
#' CdistrAreas(distr="chi.sq",upper.x=40,deg.fred = 23,side="left")#P(x2<40)
#' CdistrAreas(distr="chi.sq",upper.x=20,deg.fred = 12,side="left")#p(x2<-20)
#'
#' CdistrAreas(distr="chi.sq",lower.x=40,deg.fred = 23,side="right")#P(x2>40)
#' CdistrAreas(distr="chi.sq",lower.x=20,deg.fred = 12,side="right")#P(x2>20)
#' 
#' CdistrAreas(distr="chi.sq",lower.x=12,upper.x=52,deg.fred = 10,side="between") #P(12<x2<52)
#' CdistrAreas(distr="chi.sq",lower.x=12,upper.x=15,deg.fred = 10,side="between") #P(12<x2<15)
#' CdistrAreas(distr="chi.sq",lower.x=52,upper.x=53,deg.fred = 20,side="between")#P(52<x2<53)
#' 
#' ###Fisher
#' CdistrAreas(distr="Fisher",value.x =2,deg.fred1=5,deg.fred2=10,side="left")
#' CdistrAreas(distr="Fisher",value.x=2.5,deg.fred1=5,deg.fred2=10,side="right")
#' CdistrAreas(distr="Fisher",lower.x=2,upper.x=3,deg.fred1=5,deg.fred2=10,side="between")




CdistrAreas<-function(distr="Standard Normal",lower.x=NULL, upper.x=NULL, side,deg.fred, value.x=NULL,deg.fred1,deg.fred2,cdf=FALSE){
  
  
  if(distr=="Standard Normal"){
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
      if((lower.x>-4)&(lower.x>=0)){
        upper.x<-4
        mtext(paste("area:", round(1-pnorm(lower.x),4)), side = 3)
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = normPlot(x)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      }
    }
    
    if(side=="left"){
      if((upper.x<4)&(upper.x<0)){
        lower.x<--4 
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
        if((lower.x>-4)&(lower.x>=0)){
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
  #### 
  
  if(distr=="t"){
    
    Grid= seq(-4,4,by = 0.01)
    plot(Grid, dt(Grid,df=deg.fred), type = "l", xlab = "t", ylab = "t-Student density",lwd=1,xaxt="n")
    axis(1,seq(-4,4,by=1))
    tPlot = function(x,df){dt(x,df=deg.fred)}  
    
    if(side=="right"){
      if((lower.x>-4)&(lower.x<0)){
        upper.x<-4
        mtext(paste("area:", round(1-pt(lower.x,df=deg.fred),4)), side = 3)
        
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = tPlot(x,df=deg.fred)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      }
    }
    
    
    if(side=="right"){
      
      if((lower.x>-4)&(lower.x>=0)){
        upper.x<-4
        mtext(paste("area:", round(1-pt(lower.x,df=deg.fred),4)), side = 3)
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = tPlot(x,df=deg.fred)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
      }
    }
    
    if(side=="left"){
      lower.x<--4
      if((upper.x<4)&(upper.x<0)){
        
        x1 = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y1 = tPlot(x1,df=deg.fred)
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
        y = tPlot(x,df=deg.fred)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
      }
    }
    
    
    if(side=="between"){
      if((lower.x>=0) & (upper.x>=0)){
        
        mtext(paste("area:", round((pt(upper.x,df=deg.fred)-pt(lower.x,df=deg.fred)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = tPlot(x,df=deg.fred)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = tPlot(x1,df=deg.fred)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
      
      if((lower.x<0) & (upper.x>=0)){
        
        mtext(paste("area:", round((pt(upper.x,df=deg.fred)-pt(lower.x,df=deg.fred)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = tPlot(x,df=deg.fred)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = tPlot(x1,df=deg.fred)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
      
      
      if((lower.x<0) & (upper.x<0)&(lower.x<upper.x)){
        
        mtext(paste("area:", round((pt(upper.x,df=deg.fred)-pt(lower.x,df=deg.fred)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = tPlot(x,df=deg.fred)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = tPlot(x1,df=deg.fred)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
      
      
    }
    
    
  }
  #####chisq
  
  
  
  if(distr=="chi.sq"){
    
    Grid= seq(0,30,by = 0.01)
    plot(Grid, dchisq(Grid,df=deg.fred), type = "l", xlab = expression(chi^2), ylab = "Chi-squared density",lwd=1,xaxt="n")
    axis(1,seq(0,30,by=1))
    chiPlot = function(x,df){dchisq(x,df=deg.fred)}  
    
    if(side=="right"){
      
      
      if((lower.x<=30)){
        upper.x<-30
        mtext(paste("area:", round(1-pchisq(lower.x,df=deg.fred),4)), side = 3)
        
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = chiPlot(x,df=deg.fred)
        x = c(max(Grid), x, lower.x) 
        y = c(min(y),min(y), y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      }
    }
    
    
    if(side=="right"){
      
      if((lower.x>30)){
        max.x<-lower.x+20
        upper.x<-max.x
        
        Grid= seq(30,max.x,by = 0.01)
        plot(Grid, dchisq(Grid,df=deg.fred), type = "l", xlab = expression(chi^2), ylab = "Chi-squared density",lwd=1,xaxt="n")
        axis(1,seq(30,max.x,by=1))
        chiPlot = function(x,df){dchisq(x,df=deg.fred)}  
        
        
        mtext(paste("area:", round(1-pchisq(lower.x,df=deg.fred),6)), side = 3)
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = chiPlot(x,df=deg.fred)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue",lwd=2)
        
      }
    }
    
    if(side=="left"){
      lower.x<-0
      if((upper.x<=30)){
        
        x1 = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y1 = chiPlot(x1,df=deg.fred)
        x1 = c(upper.x,min(Grid), x1, upper.x) 
        
        y1 = c(y1,0, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
        
        mtext(paste("area:", round(pchisq(upper.x,df=deg.fred),4)), side = 3)
        
      }
    }
    
    if(side=="left"){
      if((upper.x<500)&(upper.x>30)){
        max.x<-upper.x+20
        Grid= seq(0,max.x,by = 0.01)
        plot(Grid, dchisq(Grid,df=deg.fred), type = "l", xlab = expression(chi^2), ylab = "Chi-squared density",lwd=1,xaxt="n")
        axis(1,seq(0,max.x,by=1))
        chiPlot = function(x,df){dchisq(x,df=deg.fred)}  
        
        mtext(paste("area:", round(pchisq(upper.x,df=deg.fred),6)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = chiPlot(x,df=deg.fred)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
      }
    }
    
    
    if(side=="between"){
      if((lower.x>30) & (upper.x>30)){
        
        min.x<-lower.x-10
        max.x<-upper.x+10
        
        
        step <- (max.x - min.x) / 20
        
        Grid= seq(min.x,max.x,step)
        plot(Grid, dchisq(Grid,df=deg.fred), type = "l", xlab = expression(chi^2), ylab = "Chi-squared density",lwd=1,xaxt="n")
        axis(1,seq(min.x,max.x,step))
        chiPlot = function(x,df){dchisq(x,df=deg.fred)} 
        
        mtext(paste("area:", round((pchisq(upper.x,df=deg.fred)-pchisq(lower.x,df=deg.fred)),6)), side = 3)
        
        
        
        
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = chiPlot(x,df=deg.fred)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        
        
        
        x1 = seq(from = upper.x, to = max(Grid), length.out = 1000) 
        y1 = chiPlot(x1,df=deg.fred)
        
        x1 = c(upper.x, max(Grid), x1, upper.x) 
        y1 = c(0,0,0, y1)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
        
        
      }
      
      if((lower.x<=30) & (upper.x>30)){
        
        min.x<-lower.x-10
        max.x<-upper.x+10
        
        
        Grid= seq(min.x,max.x,by=0.1)
        plot(Grid, dchisq(Grid,df=deg.fred), type = "l", xlab = expression(chi^2), ylab = "Chi-squared density",lwd=1,xaxt="n")
        axis(1,at=seq(min.x,max.x,by=1))
        chiPlot = function(x,df){dchisq(x,df=deg.fred)} 
        
        
        mtext(paste("area:", round((pchisq(upper.x,df=deg.fred)-pchisq(lower.x,df=deg.fred)),6)), side = 3)
        
        
        x = seq(from = lower.x, to = max(Grid), length.out = 1000) 
        y = chiPlot(x,df=deg.fred)
        
        x = c(lower.x, max(Grid), x, lower.x) 
        y = c(0,0,0, y)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        
        
        
        x1 = seq(from = upper.x, to = max(Grid), length.out = 1000) 
        y1 = chiPlot(x1,df=deg.fred)
        
        x1 = c(upper.x, max(Grid), x1, upper.x) 
        y1 = c(0,0,0, y1)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
        
        
      }
      
      
      if((lower.x<=30) & (upper.x<=30)&(lower.x<upper.x)){
        
        
        
        mtext(paste("area:", round((pchisq(upper.x,df=deg.fred)-pchisq(lower.x,df=deg.fred)),4)), side = 3)
        
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = chiPlot(x,df=deg.fred)
        
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = chiPlot(x1,df=deg.fred)
        
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
      
      
    }
    
  }
  
  ####Fisher
  
  if(distr=="Fisher"){
    
    Grid= seq(0,30,by = 0.01)
    plot(Grid, df(Grid,df1=deg.fred1,df2=deg.fred2), type = "l", xlab = "F", ylab = "Fisher density",lwd=1,xaxt="n")
    axis(1,seq(0,30,by=1))
    FPlot = function(x,df1,df2){df(x,df1=deg.fred1,df2=deg.fred2)}  
    
    if(side=="right"){
      
      upper.x<-30
      mtext(paste("area:", round(1-pf(value.x,df1=deg.fred1,df2=deg.fred2),4)), side = 3)
      
      x = seq(from = value.x, to = max(Grid), length.out = 1000) 
      y = FPlot(x,df1=deg.fred1,df2=deg.fred2)
      
      x = c(value.x, max(Grid), x, value.x) 
      y = c(0,0,0, y)
      polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      
    }
    
    # }
    
    if(side=="left"){
      lower.x<-0
      
      x1 = seq(from = min(Grid), to = value.x, length.out = 1000) 
      y1 = FPlot(x1,df1=deg.fred1,df2=deg.fred2)
      x1 = c(value.x,min(Grid), x1, value.x) 
      
      y1 = c(y1,0, 0, 0)
      polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
      
      mtext(paste("area:", round(pf(value.x,df1=deg.fred1,df2=deg.fred2),4)), side = 3)
      
    }
    
    
    
    
    if(side=="between"){
      if((lower.x>=0) & (upper.x>=0)){
        
        mtext(paste("area:", round((pf(upper.x,df1=deg.fred1,df2=deg.fred2)-pf(lower.x,df1=deg.fred1,df2=deg.fred2)),4)), side = 3)
        x = seq(from = min(Grid), to = upper.x, length.out = 1000) 
        y = FPlot(x,df1=deg.fred1,df2=deg.fred2)
        x = c(upper.x, min(Grid), x, upper.x) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "Light blue")
        
        
        x1 = seq(from = min(Grid), to = lower.x, length.out = 1000) 
        y1 = FPlot(x1,df1=deg.fred1,df2=deg.fred2)
        x1 = c(lower.x, min(Grid), x1, lower.x) 
        y1 = c(0, y1, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "White")
        
      }
      
    }
    
  }
  
  
  
}





