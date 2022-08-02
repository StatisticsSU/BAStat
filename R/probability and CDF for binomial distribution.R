#' Draw the the probability of the Binomial distribution and combine with the cumulative distribution function.
#'
#' @param size is the sample size of the Binomial distribution;
#' @param prob the probability of success of the Binomial distribution;
#' @param side it can be `equal` and it the plot with the probability in one specific value that it will assigned in `value.x`; if side it is equal to `left` then the probability is considered to be on the left side or equal to the chosen value; `right` if the probability is on the right side or equal to the chosen value; `between` if it is referred to the probability between or equal to two values and in that case it has to be specified `lower.x` and `upper.x`
#' @param cdf if it is true then it is drawn the CDF of the Binomial distribution and on the top of the plot it is shown the difference between the extremes
#' @return the probability distribution or the CDF of the Binomial distribution
#' @export
#' @examples
#' library(BAStat)
#' 
#' binProbsPlots(size=12,prob=1/6,side="equal",value.x=4)#P(x=4)
#' binProbsPlots(size=12,prob=1/6,side="left",value.x=4)#P(x<=4)
#' binProbsPlots(size=12,prob=1/6,side="left",value.x=4,cdf=TRUE)#F(x=4)
#' binProbsPlots(size=12,prob=1/6,side="right",value.x=4)#P(x>=4)
#' binProbsPlots(size=12,prob=1/6,side="right",value.x=4,cdf=TRUE)#F(x>=4)
#' binProbsPlots(size=12,prob=1/6,side="between",lower.x=1,upper.x=4)##P(2<=x<=4)

#' binProbsPlots(size=12,prob=1/6,side="between",lower.x=1,upper.x=4,cdf=TRUE)#F(x=4)-F(x=1)
#' binProbsPlots(size=12,prob=1/6,side="between",lower.x=2,upper.x=4)##P(2<=x<=4)#in order to get the same amount as in the CDF


binProbsPlots<-function(size,prob,side,value.x=NULL,lower.x=NULL,upper.x=NULL,cdf=FALSE){
x<-0:size
pdf<-dbinom(x,size,prob)  
plot(x,pdf,type="h",main="",lwd=2,ylab="PDF",ylim=c(0,max(pdf)))


if(side=="equal"){
  points(value.x,dbinom(value.x, size, prob),type="h",lwd=2,col="red")#P(x=x)
  mtext(paste("P(X=x)=", round((dbinom(value.x,size,prob)),4)), side = 3,col="red")
  p<-c(value.x, round(dbinom(value.x,size,prob),4))
  text(p,paste(round(dbinom(value.x,size,prob),4)),adj=-0.2,col="red")
}


if(side=="left"){
  xx<-0:value.x
  points(xx,dbinom(xx, size, prob),type="h",lwd=2,col="blue")#P(x=x)
  mtext(paste("P(X<=x) or F(X=x)=", round((pbinom(value.x,size,prob)),4)), side = 3,col="blue")
  p<-cbind(xx, round(dbinom(0:value.x,size,prob),4))
  text(p,paste(round(dbinom(0:value.x,size,prob),4)),adj=-0.2,col="blue")
}

if(side=="right"){
  xx<-value.x:size
  points(xx,dbinom(xx, size, prob),type="h",lwd=2,col="blue")#P(x=x)
  mtext(paste("P(X>=x)=1-P(X<x) or F(X=x)=", round(1-(pbinom(value.x,size,prob)),4)), side = 3,col="blue")
  p<-cbind(xx, round(dbinom(xx,size,prob),4))
  text(p,paste(round(dbinom(xx,size,prob),4)),adj=-0.2,col="blue")
}


if(side=="between"){
  xx<-lower.x:upper.x
  points(xx,dbinom(xx, size, prob),type="h",lwd=2,col="blue")#P(x=x)
  mtext(paste("P(x1<=X<=x2)=", round(sum(dbinom(lower.x:upper.x,size,prob)),4)), side = 3,col="blue")
  p<-cbind(xx, round(dbinom(xx,size,prob),4))
  text(p,paste(round(dbinom(xx,size,prob),4)),adj=-0.2,col="blue")
}

###  


if(cdf==TRUE){
  
  cdfbinom<-cumsum(pdf)
  plot(x,cdfbinom,type="s", main="",lty=2,ylab="CDF")
  points(x,cdfbinom,pch=16) 
  
  if(side=="left"){
    lower.x<-0
    mtext(paste("P(X<=x) or F(X=x)=", round((pbinom(value.x,size,prob)),4)), side = 3,col="blue")
    points(value.x,(pbinom(value.x,size,prob)),col="blue",pch=16)
    x1<-ifelse(x<=value.x & x>=lower.x,x,NA)
    cum1<-ifelse(cdfbinom<=pbinom(value.x,size,prob) & cdfbinom>=cumsum(pbinom(lower.x,size,prob)),cdfbinom,NA)
    p1<-cbind(x1,cum1)
    points(p1, col="blue", pch=16,type="s")
    points(p1, col="blue", pch=16)
    text(t(c(value.x, round(pbinom(value.x,size,prob),4))), paste(round(pbinom(value.x,size,prob),4)), adj=-0.2,col="blue")
  }
  
  if(side=="right"){
    upper.x<-size
    mtext(paste("P(X>=x)=", round(1-(pbinom(value.x,size,prob)),4)), side = 3,col="blue")
    points(value.x,(pbinom(value.x,size,prob)),col="blue",pch=16)
    points(upper.x,(pbinom(upper.x,size,prob)),col="blue",pch=16)
    x1<-ifelse(x>=value.x & x<=upper.x,x,NA)
    cum1<-ifelse(cdfbinom>=pbinom(value.x,size,prob) & cdfbinom<=cumsum(pbinom(upper.x,size,prob)),cdfbinom,NA)
    p1<-cbind(x1,cum1)
    points(p1, col="blue", pch=16,type="s")
    points(p1, col="blue", pch=16)
    text(t(c(value.x, round(pbinom(value.x,size,prob),4))), paste(round(pbinom(value.x,size,prob),4)), adj=-0.2)
    text(t(c(upper.x, round(pbinom(upper.x,size,prob),4))), paste(round(pbinom(upper.x,size,prob),4)), adj=-0.2)
    
  }
  
  
  if(side=="between"){
    mtext(paste("F(x2)-F(x1):", round(pbinom(upper.x,size,prob)-pbinom(lower.x,size,prob),4)), side = 3,col="blue")
    p <- c(upper.x, pbinom(upper.x,size,prob))
    points(t(p), col="blue", pch=16)
    text(t(p), paste(round(pbinom(upper.x,size,prob),4)), adj=-0.2)
    p2 <- c(lower.x, pbinom(lower.x,size,prob))
    points(t(p2), col="blue", pch=16)
    text(t(p2), paste(round(pbinom(lower.x,size,prob),4)), adj=-0.2)
    x1<-ifelse(x<=upper.x & x>=lower.x,x,NA)
    cum1<-ifelse(cdfbinom<=pbinom(upper.x,size,prob)& cdfbinom>=dbinom(lower.x,size,prob),cdfbinom,NA)
    p1<-cbind(x1,cum1)
    points(p1, col="blue", pch=16)
    points(p1, col="blue", pch=16,type="s")
  }
}#cdf

}