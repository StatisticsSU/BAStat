#' Show the interaction between the probability of type 1 and type 2 error when it is based on Binomial distribution
#' The aim is to show the areas based on the null and alternative sampling distributions of the proportion. Computations are without the adjusting for small samples.
#' Tests H0: pi = pi0 vs Ha: pi > pi0; H0: pi = pi0 vs Ha: pi < pi0; H0: pi = pi0 vs Ha: pi != pi0
#' It was possible to add this function to the mean function, but the purpose is also to remember that at the beginning the distribution is discrete and in the standard deviation it might be under null hypothesis or alternative hypothesis.
#' 
#' @param pi0 the parameter under the null hypothesis
#' @param pia the parameter under the alternative hypothesis
#' @param n the sample size  
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
#' alphabetabinom(pi0=0.5,pia=0.6,n=50,alpha=0.20,side="greater")
#' alphabetabinom(pi0=0.5,pia=0.6,n=50,alpha=0.20,Obs=33,side="greater")
#' alphabetabinom(pi0=0.5,pia=0.4,n=50,alpha=0.20,side="less")
#' alphabetabinom(pi0=0.5,pia=0.4,n=50,alpha=0.20,Obs=13,side="less")
#'
#' alphabetabinom(pi0=0.5,pia=0.4,n=50,alpha=0.10,side="two.sided")
#' alphabetabinom(pi0=0.5,pia=0.4,n=50,alpha=0.10,Obs=16,side="two.sided")
#' alphabetabinom(pi0=0.5,pia=0.6,n=50,alpha=0.10,side="two.sided")
#' alphabetabinom(pi0=0.5,pia=0.6,n=50,alpha=0.10,Obs=33,side="two.sided")


alphabetabinom<-function(pi0,pia,n,alpha,side,Obs=FALSE){
  
  sdpi0<-sqrt(n*pi0*(1-pi0))
  sdpia<-sqrt(n*pia*(1-pia))
  epia<-pia*n
  epi0<-pi0*n
  if(side=="greater"){
    cv<-epia-(sdpia*(qnorm(1-alpha)))
    beta<-pnorm((cv-epia)/sdpia)
    
    minx<-min(epi0,epia)-3*sdpi0
    maxx<-max(epi0,epia)+3*sdpia
    Grid= seq(minx,maxx,by = 0.001)
    
    plot(Grid, dnorm(Grid,epi0,sdpi0), las=1,type = "l", xlab = "x",
         ylab = "Normal probability density",lwd=1,
         main=bquote(~H[0]:~pi==.(pi0)~H[A]:~pi==.(pia)~pi>pi[0]))
    lines(Grid,dnorm(Grid,epia,sdpia),type="l",xlab="",ylab="")
    
    Plot = function(x,epi0,sdpi0){dnorm(x,epi0,sdpi0)}  
    x = seq(from = cv, to = max(Grid), length.out = 1000) 
    y = Plot(x,epi0,sdpi0)
    
    x = c(cv, x, max(Grid), cv) 
    y = c(0, y, 0, 0)
    polygon(x,y, lty = 3, border = NULL, col = "light blue")
    text(x=epia,y=max(y)-0.006,bquote(~alpha==.(round(alpha,4))),col="blue")
    legend(x = "topleft", inset=.05, legend = bquote(~H[0]), 
           cex = 1, col = "black", bty="n")
    text(x=epia,y=max(y),"Reject region",col="blue",cex=0.6)
    abline(v=cv,col=4,lwd=2,lty=2)
    
    Plota = function(x,epia,sdpia){dnorm(x,epia,sdpia)}  
    x = seq(from = min(Grid), to = cv, length.out = 1000) 
    y = Plota(x,epia,sdpia)
    text(x=cv,y=beta/2,"Crit.v.",col=4)
    x = c(cv,min(Grid), x, cv) 
    y = c(y, 0, 0, 0)
    polygon(x,y, lty = 3, border = NULL, col = "orange")
    
    legend(x = "topright", inset=.05, legend = bquote(~H[A]), 
           cex = 1,  col ="black", bty="n")
    
    mtext(bquote(~gamma==.(1-round(beta,4))~"crit.v."==.(round(cv,4))),side=3)
    
    text(x=mean(x),y=mean(y) ,bquote(~beta==.(round(beta,4))),col="brown")
    
    if(Obs){
      abline(v=Obs,col=2,lwd=2,lty=2)
      
      xp = seq(from = Obs, to = max(Grid), length.out = 1000) 
      yp = Plot(xp,epi0,sdpi0) 
      
      xp = c(Obs, xp, max(Grid), Obs) 
      yp = c(0, yp, 0, 0)
      polygon(xp,yp, lty = 3, border = NULL, col = "red")
      text(x=Obs,y=alpha/2,"Obs",col=2)
      text(x=Obs,y=alpha/2-0.05,paste("pv:",round(1-pnorm((Obs-epi0)/sdpi0),4)),col=2)
      
    }
  }
  #right side with pvalue
  
  if(side=="less"){ 
    
    cv<-(sdpia*(qnorm(alpha)))+epia
    beta<-1-pnorm((cv-epia)/sdpia)
    
    minx<-min(epi0,epia)-3*sdpia
    maxx<-max(epi0,epia)+3*sdpi0
    Grid= seq(minx,maxx,by = 0.001)
    
    plot(Grid, dnorm(Grid,epi0,sdpi0), las=1,type = "l", xlab = "x",
         ylab = "Normal probability density",lwd=1,
         main=bquote(~H[0]:~pi==.(pi0)~H[A]:~pi==.(pia)~pi<pi[0]))
    lines(Grid,dnorm(Grid,epia,sdpia),type="l",xlab="",ylab="")
    
    Plot = function(x,epi0,sdpi0){dnorm(x,epi0,sdpi0)}  
    x = seq(from = min(Grid), to = cv, length.out = 1000) 
    y = Plot(x,epi0,sdpi0)
    
    x = c(cv, min(Grid), x, cv) 
    y = c(y, 0, 0, 0)
    polygon(x,y, lty = 3, border = NULL, col = "light blue")
    text(x=cv-(epi0-epia),y=max(y)-0.006,bquote(~alpha==.(round(alpha,4))),col="blue")
    
    legend(x = "topright", inset=.05, legend = bquote(~H[0]), 
           cex = 1, col = "black", bty="n")
    
    text(x=cv-(epi0-epia),y=max(y),"Reject region",col="blue",cex=0.6)
    abline(v=cv,col=4,lwd=2,lty=2)
    Plota = function(x,epia,sdpia){dnorm(x,epia,sdpia)}  
    x = seq(from = cv, to = max(Grid), length.out = 1000) 
    y = Plota(x,epia,sdpia)
    
    x = c(cv,x,max(Grid),  cv) 
    y = c(0, y, 0, 0)
    polygon(x,y, lty = 3, border = NULL, col = "orange")
    
    legend(x = "topleft", inset=.05, legend = bquote(~H[A]), 
           cex = 1,  col ="black", bty="n")
    
    mtext(bquote(~gamma==.(1-round(beta,4))~"crit.v."==.(round(cv,4))),side=3)
    
    text(x=mean(x),y=mean(y) ,bquote(~beta==.(round(beta,4))),col="brown")
    text(x=cv,y=beta/2,"Crit.v.",col=4)
    if(Obs){
      
      abline(v=Obs,col=2,lwd=2,lty=2) 
      xp = seq(from = min(Grid), to = Obs, length.out = 1000) 
      yp = Plot(xp,epi0,sdpi0) 
      
      xp = c(Obs, min(Grid), xp, Obs) 
      yp = c(yp, 0, 0, 0)
      polygon(xp,yp, lty = 3, border = NULL, col = "red") 
      text(x=Obs,y=alpha/2,"Obs",col=2)
      text(x=Obs,y=max(yp),paste("pv",round(pnorm((Obs-epi0)/sdpi0),2)),col=2)
      
    }
  }#left
  
  if(side=="two.sided"){
    
    cvl<-epi0-(sdpi0*(qnorm(1-alpha/2)))
    cvu<-epi0-(sdpi0*(qnorm(alpha/2)))
    
    if(pia>pi0){
      
      beta1<-pnorm((cvu-epia)/sdpia)
      
      minx<-min(epi0,epia)-3*sdpi0
      maxx<-max(epi0,epia)+3*sdpia
      Grid= seq(minx,maxx,by = 0.001)
      
      plot(Grid, dnorm(Grid,epi0,sdpi0), las=1,type = "l", xlab = "x",
           ylab = "Normal probability density",lwd=1,
           main=bquote(~H[0]:~pi==.(pi0)~H[A]:~pi==.(pia)~pi!=pi[0]))
      lines(Grid,dnorm(Grid,epia,sdpia),type="l",xlab="",ylab="")
      
      Plot = function(x,epi0,sdpi0){dnorm(x,epi0,sdpi0)}  
      x = seq(from = cvu, to = max(Grid), length.out = 1000) 
      y = Plot(x,epi0,sdpi0)
      x = c(cvu, x, max(Grid), cvu) 
      y = c(0, y, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "light blue")
      text(x=epia,y=max(y)-0.006,bquote(~alpha==.(round(alpha/2,4))),col="blue")
      legend(x = "topleft", inset=.05, legend = bquote(~H[0]), 
             cex = 1, col = "black", bty="n")
      text(x=epia,y=max(y),"Reject region",col="blue",cex=0.6)
      abline(v=cvu,col=4,lwd=2,lty=2)
      abline(v=cvl,col=4,lwd=2,lty=2)
      ###
      
      
      x = seq(from = min(Grid), to = cvl, length.out = 1000) 
      y = Plot(x,epi0,sdpi0)
      
      x = c(cvl,min(Grid), x, cvl) 
      y = c(y, 0, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "light blue",density=85,angle=45)
      ####
      Plota = function(x,epia,sdpia){dnorm(x,epia,sdpia)}  
      x = seq(from = min(Grid), to = cvu, length.out = 1000) 
      y = Plota(x,epia,sdpia)
      text(x=cvu,y=beta1/2,"Crit.v.",col=4)
      text(x=cvl,y=beta1/2,"Crit.v.",col=4)
      x = c(cvu,min(Grid), x, cvu) 
      y = c(y, 0, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "orange")
      
      legend(x = "topright", inset=.05, legend = bquote(~H[A]), 
             cex = 1,  col ="black", bty="n")
      
      mtext(bquote(~gamma==.(1-round(beta1,4))~"crit.vl"==.(round(cvl,4))~"crit.vu"==.(round(cvu,4))),side=3)
      
      text(x=mean(x),y=mean(y) ,bquote(~beta==.(round(beta1,4))),col="brown")
      
      
      if(Obs){
        x = seq(from = Obs, to = max(Grid), length.out = 1000) 
        y = Plot(x,epi0,sdpi0)
        
        x = c(Obs, x, max(Grid), Obs) 
        y = c(0, y, 0, 0)
        polygon(x,y, lty = 3, border = NULL, col = "red")      
        abline(v=Obs,col=2,lwd=2,lty=2)
        text(x=Obs,y=alpha/2,"Obs",col=2)
        text(x=Obs+0.05,y=round(1-pnorm((Obs-epi0)/sdpi0),2),paste("pv=",round((1-pnorm((Obs-epi0)/sdpi0))/2,4)),col=2)
        
      }
      
    }
    
    if(pia<pi0){
      beta2<-1-pnorm((cvl-epia)/sdpia)
      
      minx<-min(epi0,epia)-3*sdpia
      maxx<-max(epi0,epia)+3*sdpi0
      Grid= seq(minx,maxx,by = 0.001)
      
      plot(Grid, dnorm(Grid,epi0,sdpi0), las=1,type = "l", xlab = "x",
           ylab = "Normal probability density",lwd=1,
           main=bquote(~H[0]:~pi==.(pi0)~H[A]:~pi==.(pia)~pi!=pi[0]))
      lines(Grid,dnorm(Grid,epia,sdpia),type="l",xlab="",ylab="")
      
      Plot = function(x,epi0,sdpi0){dnorm(x,epi0,sdpi0)}  
      x = seq(from = min(Grid), to = cvl, length.out = 1000) 
      y = Plot(x,epi0,sdpi0)
      
      x = c(cvl, min(Grid), x, cvl) 
      y = c(y, 0, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "light blue")
      
      
      ##
      x = seq(from = cvu, to = max(Grid), length.out = 1000) 
      y = Plot(x,epi0,sdpi0)
      
      x = c(cvu,x,max(Grid),  cvu) 
      y = c(0, y, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "light blue",density=85,angle=45)
      ###
      text(x=cvl-(epi0-epia),y=max(y)-0.006,bquote(~alpha==.(round(alpha/2,4))),col="blue")
      
      legend(x = "topright", inset=.05, legend = bquote(~H[0]), 
             cex = 1, col = "black", bty="n")
      
      text(x=cvl-(epi0-epia),y=max(y),"Reject region",col="blue",cex=0.6)
      abline(v=cvl,col=4,lwd=2,lty=2)
      abline(v=cvu,col=4,lwd=2,lty=2)
      Plota = function(x,epia,sdpia){dnorm(x,epia,sdpia)}  
      x = seq(from = cvl, to = max(Grid), length.out = 1000) 
      y = Plota(x,epia,sdpia)
      
      x = c(cvl,x,max(Grid),  cvl) 
      y = c(0, y, 0, 0)
      polygon(x,y, lty = 3, border = NULL, col = "orange")
      
      legend(x = "topleft", inset=.05, legend = bquote(~H[A]), 
             cex = 1,  col ="black", bty="n")
      
      
      mtext(bquote(~gamma==.(1-round(beta2,4))~"crit.vl"==.(round(cvl,4))~"crit.vu"==.(round(cvu,4))),side=3)
      
      text(x=mean(x),y=mean(y) ,bquote(~beta==.(round(beta2,4))),col="brown")
      text(x=cvl,y=beta2/2,"Crit.v.",col=4)
      
      
      
      if(Obs){
        x1 = seq(from = min(Grid), to = Obs, length.out = 1000) 
        y1 = Plot(x1,epi0,sdpi0)
        x1 = c(cvl,min(Grid), x1, cvl) 
        
        y1 = c(y1,0, 0, 0)
        polygon(x1,y1, lty = 3, border = NULL, col = "red")
        
        abline(v=Obs,col=2,lwd=2,lty=2)
        text(x=Obs,y=alpha/2,"Obs",col=2)
        text(x=Obs-0.05,y=round(pnorm((Obs-epi0)/sdpi0),2),paste("pv=",round(pnorm((Obs-epi0)/sdpi0)/2,4)),col=2)
        
      }
      
    }
  }
  
}