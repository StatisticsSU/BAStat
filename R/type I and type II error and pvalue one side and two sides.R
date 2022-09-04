#' Show the interaction between the probability of type 1 and type 2 error 
#' The aim is to show the areas based on the null and alternative sampling distributions of the mean
#' Tests H0: mu = mu0 vs Ha: mu > mu0; H0: mu = mu0 vs Ha: mu < mu0; H0: mu = mu0 vs Ha: mu != mu0
#' 
#' @param mu0 the parameter under the null hypothesis
#' @param mua the parameter under the alternative hypothesis
#' @param sigmax the standard deviation of the sample variance divided by the number of observations 
#' @param alpha significance level.
#' @param side it can be `less` if it is one side test and the alternative hypothesis is less than the null hypothesis;   
#'  side it can be `greater` if it is one side test and the alternative hypothesis is greater than the null hypothesis;   
#'  side it can be `two.sided` if it is two sides test and the alternative hypothesis can be smaller than the null hypothesis or the alternative hypothesis can be greater than the null hypothesis; in the plot there is alpha divided by 
#'  two and it is represented in light blue the area and it is also specified if it is on the right side or on the left side for the comparison with the beta.
#' @param Obs if it is *TRUE* so if it is a specific value than it is possible to see on the same plot the value and the p-value
#' @return Plot for the probability of type 1 and type 2 error as areas. The plots are based on Standardized Normal distributions it is also reported gamma which is the power of the test as 1-beta
#' @export
#' @examples
#' library(BAStat)
#' 
#' alpha.beta(mu0=200,mua=180,sigmax=7,alpha=0.05,Obs=183,side="left")
#'
#'alpha.beta(mu0=200,mua=180,sigmax=7,alpha=0.05,side="left")
#'alpha.beta(mu0=200,mua=180,sigmax=7,alpha=0.1,side="left")
#'
#'alpha.beta(mu0=200,mua=220,sigmax=7,alpha=0.05,Obs=218,side="right")
#'alpha.beta(mu0=200,mua=220,sigmax=7,alpha=0.05,side="right")
#'alpha.beta(mu0=200,mua=220,sigmax=7,alpha=0.1,side="right")
#'
#'alpha.beta(mu0=200,mua=220,sigmax=7,alpha=0.05,side="two.sided")
#'alpha.beta(mu0=200,mua=180,sigmax=7,alpha=0.05,side="two.sided")
#'alpha.beta(mu0=200,mua=220,sigmax=7,alpha=0.05,Obs=218,side="two.sided")
#'alpha.beta(mu0=200,mua=180,sigmax=7,alpha=0.05,Obs=183,side="two.sided")

alpha.beta<-function(mu0,mua,sigmax,alpha,side,Obs=FALSE){
  
  if(side=="greater"){
    cv<-(sigmax*(qnorm(1-alpha)))+mu0
    beta<-pnorm((cv-mua)/sigmax)
    
    minx<-min(mu0,mua)-3*sigmax
    maxx<-max(mu0,mua)+3*sigmax
    Grid= seq(minx,maxx,by = 0.01)
    
    plot(Grid, dnorm(Grid,mu0,sigmax), las=1,type = "l", xlab = "x",
         ylab = "Normal probability density",lwd=1,
         main=bquote(~H[0]:~mu==.(mu0)~H[A]:~mu==.(mua)~mu>mu[0]))
    lines(Grid,dnorm(Grid,mua,sigmax),type="l",xlab="",ylab="")
    
    Plot = function(x,mu0,sigmax){dnorm(x,mu0,sigmax)}  
    x = seq(from = cv, to = max(Grid), length.out = 1000) 
    y = Plot(x,mu0,sigmax)
    
    x = c(cv, x, max(Grid), cv) 
    y = c(0, y, 0, 0)
    polygon(x,y, lty = 3, border = NULL, col = "light blue")
    text(x=mua,y=max(y)-0.006,bquote(~alpha==.(round(alpha,4))),col="blue")
    legend(x = "topleft", inset=.05, legend = bquote(~H[0]), 
           cex = 1, col = "black", bty="n")
    text(x=mua,y=max(y),"Reject region",col="blue",cex=0.6)
    abline(v=cv,col=4,lwd=2,lty=2)
    
    Plota = function(x,mua,sigmax){dnorm(x,mua,sigmax)}  
    x = seq(from = min(Grid), to = cv, length.out = 1000) 
    y = Plota(x,mua,sigmax)
    text(x=cv,y=beta/2,"Crit.v.",col=4)
    x = c(cv,min(Grid), x, cv) 
    y = c(y, 0, 0, 0)
    polygon(x,y, lty = 3, border = NULL, col = "orange")
    
    legend(x = "topright", inset=.05, legend = bquote(~H[A]), 
           cex = 1,  col ="black", bty="n")
    
    mtext(bquote(~gamma==.(1-round(beta,4))~"crit.v."==.(round(cv,4))),side=3)
    
    text(x=mean(x),y=mean(y) ,bquote(~beta==.(round(beta,4))),col="orange")
    
    if(Obs){
      abline(v=Obs,col=2,lwd=2,lty=2)
      
      xp = seq(from = Obs, to = max(Grid), length.out = 1000) 
      yp = Plot(xp,mu0,sigmax) 
      
      xp = c(Obs, xp, max(Grid), Obs) 
      yp = c(0, yp, 0, 0)
      polygon(xp,yp, lty = 3, border = NULL, col = "red")
      text(x=Obs,y=alpha/2,"Obs",col=2)
      text(x=Obs,y=max(yp),paste("pv",round(1-pnorm((Obs-mu0)/sigmax),2)),col=2)
      
    }
  }
  #right side with pvalue
  
  if(side=="less"){ 
    cv<-(sigmax*(qnorm(alpha)))+mu0
    beta<-1-pnorm((cv-mua)/sigmax)
    
    minx<-min(mu0,mua)-3*sigmax
    maxx<-max(mu0,mua)+3*sigmax
    Grid= seq(minx,maxx,by = 0.01)
    
    plot(Grid, dnorm(Grid,mu0,sigmax), las=1,type = "l", xlab = "x",
         ylab = "Normal probability density",lwd=1,
         main=bquote(~H[0]:~mu==.(mu0)~H[A]:~mu==.(mua)~mu<mu[0]))
    lines(Grid,dnorm(Grid,mua,sigmax),type="l",xlab="",ylab="")
    
    Plot = function(x,mu0,sigmax){dnorm(x,mu0,sigmax)}  
    x = seq(from = min(Grid), to = cv, length.out = 1000) 
    y = Plot(x,mu0,sigmax)
    
    x = c(cv, min(Grid), x, cv) 
    y = c(y, 0, 0, 0)
    polygon(x,y, lty = 3, border = NULL, col = "light blue")
    text(x=mua,y=max(y)-0.006 ,bquote(~alpha==.(round(alpha,4))),col="blue")
    
    legend(x = "topright", inset=.05, legend = bquote(~H[0]), 
           cex = 1, col = "black", bty="n")
    
    text(x=mua,y=max(y),"Reject region",col="blue",cex=0.6)
    abline(v=cv,col=4,lwd=2,lty=2)
    Plota = function(x,mua,sigmax){dnorm(x,mua,sigmax)}  
    x = seq(from = cv, to = max(Grid), length.out = 1000) 
    y = Plota(x,mua,sigmax)
    
    x = c(cv,x,max(Grid),  cv) 
    y = c(0, y, 0, 0)
    polygon(x,y, lty = 3, border = NULL, col = "orange")
    
    legend(x = "topleft", inset=.05, legend = bquote(~H[A]), 
           cex = 1,  col ="black", bty="n")
    
    mtext(bquote(~gamma==.(1-round(beta,4))~"crit.v."==.(round(cv,4))),side=3)
    
    text(x=mean(x),y=mean(y) ,bquote(~beta==.(round(beta,4))),col="orange")
    
    
    text(x=cv,y=beta/2,"Crit.v.",col=4)
    if(Obs){
      
      abline(v=Obs,col=2,lwd=2,lty=2) 
      xp = seq(from = min(Grid), to = Obs, length.out = 1000) 
      yp = Plot(xp,mu0,sigmax) 
      
      xp = c(Obs, min(Grid), xp, Obs) 
      yp = c(yp, 0, 0, 0)
      polygon(xp,yp, lty = 3, border = NULL, col = "red") 
      text(x=Obs,y=alpha/2,"Obs",col=2)
      text(x=Obs,y=max(yp),paste("pv",round(pnorm((Obs-mu0)/sigmax),2)),col=2)
      
    }
  }#left
  
  if(side=="two.sided"){
    
    cvl<-(sigmax*(qnorm(alpha/2)))+mu0
    cvu<-(sigmax*(qnorm(1-alpha/2)))+mu0
    
    if(mua>=cvu){
      beta2<-pnorm((cvu-mua)/sigmax) 
      minx<-min(mu0,mua)-3*sigmax
      maxx<-max(mu0,mua)+3*sigmax
      Grid= seq(minx,maxx,by = 0.01)
      
      plot(Grid, dnorm(Grid,mu0,sigmax), las=1,type = "l", xlab = "x",
           ylab = "Normal probability density",lwd=1,
           main=bquote(~H[0]:~mu==.(mu0)~H[A]:~mu==.(mua)~mu!=mu[0]))
      lines(Grid,dnorm(Grid,mua,sigmax),type="l",xlab="",ylab="")
      
      abline(v=cvl,col=4,lwd=2,lty=2)
      abline(v=cvu,col=4,lwd=2,lty=2)
      
      
      Plot = function(x,mu0,sigmax){dnorm(x,mu0,sigmax)}  
      x = seq(from = cvu, to = max(Grid), length.out = 1000) 
      y = Plot(x,mu0,sigmax)
      x = c(cvu, x, max(Grid), cvu) 
      y = c(0, y, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "light blue")
      
      x1 = seq(from = min(Grid), to = cvl, length.out = 1000) 
      y1 = Plot(x1,mu0,sigmax)
      
      x1 = c(cvl,  min(Grid), x1,cvl) 
      y1 = c(y1, 0, 0, 0)
      polygon(x1,y1, lty = 3, border = NULL, col = "light blue",density=30,angle=45)
      
      legend(x = "topleft", inset=.05, legend = bquote(~H[0]), 
             cex = 1, col = "black", bty="n")
      
      Plota = function(x,mua,sigmax){dnorm(x,mua,sigmax)}  
      x = seq(from = min(Grid), to = cvu, length.out = 1000) 
      y = Plota(x,mua,sigmax)
      text(x=cvu,y=beta2/2,"Crit.v.",col=4)
      x = c(cvu,min(Grid), x, cvu) 
      y = c(y, 0, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "orange")
      
      legend(x = "topright", inset=.05, legend = bquote(~H[A]), 
             cex = 1,  col ="black", bty="n")
      
      mtext(bquote(~gamma==.(1-round(beta2,4))~"crit.v.l."==.(round(cvl,4))~"crit.v.r."==.(round(cvu,4))),side=3)
      
      text(x=mean(x),y=mean(y) ,bquote(~beta==.(round(beta2,4))),col="orange")
      text(x=cvu-1,y=max(y),"Reject region",col="blue",cex=0.6)
      text(x=cvl-1,y=max(y),"Reject region",col="blue",cex=0.6)
      
      text(x=cvl-1,y=max(y)-0.006,bquote(~alpha==.(round(alpha/2,4))),col="blue")
      text(x=cvu-1,y=max(y)-0.006,bquote(~alpha==.(round(alpha/2,4))),col="blue")
      
      if(Obs){
        x = seq(from = Obs, to = max(Grid), length.out = 1000) 
        y = Plot(x,mu0,sigmax)
        
        x = c(Obs, x, max(Grid), Obs) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "red")      
        abline(v=Obs,col=2,lwd=2,lty=2)
        text(x=Obs,y=alpha/2,"Obs",col=2)
        text(x=Obs+0.05,y=round(1-pnorm((Obs-mu0)/sigmax),2),paste("pv=",round((1-pnorm((Obs-mu0)/sigmax))/2,4)),col=2)
        
        
        
        
        
      }
      
    }
    
    if(mua<=cvl){
      beta1<-1-pnorm((cvl-mua)/sigmax)
      
      minx<-min(mu0,mua)-3*sigmax
      maxx<-max(mu0,mua)+3*sigmax
      Grid= seq(minx,maxx,by = 0.01)
      
      plot(Grid, dnorm(Grid,mu0,sigmax), las=1,type = "l", xlab = "x",
           ylab = "Normal probability density",lwd=1,
           main=bquote(~H[0]:~mu==.(mu0)~H[A]:~mu==.(mua)~mu!=mu[0]))
      lines(Grid,dnorm(Grid,mua,sigmax),type="l",xlab="",ylab="")
      
      Plot = function(x,mu0,sigmax){dnorm(x,mu0,sigmax)}  
      x = seq(from = min(Grid), to = cvl, length.out = 1000) 
      y = Plot(x,mu0,sigmax)
      
      x = c(cvl, min(Grid), x, cvl) 
      y = c(y, 0, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "light blue")
      
      
      x = seq(from = cvu, to = max(Grid), length.out = 1000) 
      y = Plot(x,mu0,sigmax)
      
      x = c(cvu, x, max(Grid), cvu) 
      y = c(y, 0, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "light blue",density=30,angle=45)
      
      legend(x = "topright", inset=.05, legend = bquote(~H[0]), 
             cex = 1, col = "black", bty="n")
      abline(v=cvl,col=4,lwd=2,lty=2)
      abline(v=cvu,col=4,lwd=2,lty=2)
      
      Plota = function(x,mua,sigmax){dnorm(x,mua,sigmax)}  
      x = seq(from = cvl, to = max(Grid), length.out = 1000) 
      y = Plota(x,mua,sigmax)
      
      x = c(cvl,x,max(Grid),  cvl) 
      y = c(0, y, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "orange")
      
      legend(x = "topleft", inset=.05, legend = bquote(~H[A]), 
             cex = 1,  col ="black", bty="n")
      mtext(bquote(~gamma==.(1-round(beta2,4))~"crit.v.l."==.(round(cvl,4))~"crit.v.r."==.(round(cvu,4))),side=3)
      
      text(x=mean(x),y=mean(y) ,bquote(~beta==.(round(beta2,4))),col="orange")
      text(x=cvl,y=beta2/2,"Crit.v.",col=4)
      text(x=cvl,y=max(y)-0.006 ,bquote(~alpha==.(round(alpha/2,4))),col="blue")
      text(x=cvu,y=max(y)-0.006 ,bquote(~alpha==.(round(alpha/2,4))),col="blue")
      text(x=cvl,y=max(y),"Reject region",col="blue",cex=0.6)
      text(x=cvu,y=max(y),"Reject region",col="blue",cex=0.6)
      if(Obs){
        x1 = seq(from = min(Grid), to = Obs, length.out = 1000) 
        y1 = Plot(x1,mu0,sigmax)
        x1 = c(cvl,min(Grid), x1, cvl) 
        
        y1 = c(y1,0, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "red")
        
        abline(v=Obs,col=2,lwd=2,lty=2)
        text(x=Obs,y=alpha/2,"Obs",col=2)
        text(x=Obs-0.05,y=round(pnorm((Obs-mu0)/sigmax),2),paste("pv=",round(pnorm((Obs-mu0)/sigmax)/2,4)),col=2)
        
      }
      
    }
  }
  
}
