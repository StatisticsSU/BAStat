#' Confidence intervals (CI) for the mean of one population. 
#' 
#' There are several functions in R for those computations and in particular for the proportion there are many formulas for the correction to normality and decentralizing the attention to the the initial formula and the power of the central limit theorem.
#'
#' @param distr by default is intended to use the Normal distribution, otherwise specify *distr="t"* for t-distribution or *distr="binom"* for Binomial distribution; *distr="chi.sq"* for chi-squared distribution.
#' @param alpha the confidence level;
#' @param center the mean of the sample for the Normal or t-distribution, the number of successes divided by the number of observations for the proportion
#' @param stddev the standard deviation of the sample for the t-distribution; or the standard deviation of the population in case of normality or the use of central limit theorem for the normal approximation; the standard deviation based on the estimated proportion
#' @param n number of observations
#' @param degree.fred for the t-distribution: specify the number of degrees of freedom
#' @return the confidence level, the statistical marginal error, the lower and upper bound of the confidence interval. 
#' In case of proportion and the number of observations is greater than 50, it is also added the Yates' continuity. In the plots area are also automatically simulated and shown the first 200 sampled confidence intervals in order to remind that the parameter of the population is unknown and it is only possible to guess with a certain confidence where the parameter should be. 
#' On the x-axes is also included the coverage obtained by the simulations. It shows the percentage of times the true parameter was between the bounds of the confidence interval.
#' In case of chi-squared distribution it is produced the density function and the confidence intervals. The focus is on the asymmetric effect on the probabilities.
#' @export
#' @examples
#' library(BAStat)
#' 
#' CI(alpha=0.05,center=12,stddev=3,n=30)#in case of Normal distribution
#' CI(distr="t",alpha=0.05,center=12,stddev=3,n=15,degree.fred=15-1) #for the t-distribution
#' #Binomial
#' phat<-7/12#number of successes divided by the number of observations
#' CI(distr="binom",alpha=0.05,center=phat,stddev=0.1423,n=12) #for the binomial and n is less than 50
#' x<-70 #number of successes
#' n<-120 #number of observations
#' #n>50
#' stddev<-sqrt((x/n*(1-x/n)/n))
#' CI(distr="binom",alpha=0.05,center=70/120,stddev=stddev,n=120)
#' #Chi-squared distribution
#' CI(distr="chi.sq",alpha=0.10,stddev=sqrt(10.57),n=3,degree.fred=2)



CI<-function(distr="Standard Normal",alpha,center=NULL,stddev=NULL,n=NULL,degree.fred=NULL){
  
  if(distr=="chi.sq"){
    
    errorl <- qchisq(alpha/2, df = degree.fred)
    erroru <- qchisq(1 - alpha/2, df = degree.fred)
    lower_bound <- (stddev^2 * errorl)/degree.fred
    upper_bound <- (stddev^2 * erroru)/degree.fred
    cat("Quantiles of the Chi-squared distribution\n------------------------------------------------\n")
    print(c(errorl, erroru), digits = 5, na.print = "")
    cat("(lower bound; upper bound)\n------------------------------------------------\n")
    print(c(lower_bound, upper_bound), digits = 5, na.print = "")
    
    
    min.x<-0
    max.x<-upper_bound+5
    x <- seq(0, max.x, by = 1)
    y <- dchisq(x, df = degree.fred)
    plot(x, y, type = "l", xlab = expression(chi^2), ylab = "Chi-squared density", 
         main = "Inequality of the probabilities in CI", lwd = 1, 
         xaxt = "n",las=1)
    axis(1, at = seq(0, max.x, by = 5), lwd = 0.1)
    abline(v = lower_bound, lty = 2, lwd = 2, col = "orange")
    abline(v = upper_bound, lty = 2, lwd = 2, col = "orange")
    p <- c(lower_bound, round((max(y)-min(y))/2, 2))
    text(t(p), paste(round(lower_bound, 2)), col = "red")
    p <- c(upper_bound, round(mean(y), 2))
    text(t(p), paste(round(upper_bound, 2)), col = "red")
    
  }
  
  ##################
  
  if(distr=="Standard Normal"){
    error <- qnorm(1-alpha/2)*stddev/sqrt(n)
    
    lower <- numeric(10000)
    upper <- numeric(10000)
    
    # loop sampling / estimation / CI
    for(i in 1:10000) {
      
      Y <- rnorm(n,mean=center,sd=stddev)
      lower[i] <- mean(Y) - qnorm(1-alpha/2)*sd(Y)/sqrt(n)
      upper[i] <- mean(Y) + qnorm(1-alpha/2)*sd(Y)/sqrt(n)
      
    }
    
  }
  
  
  if(distr=="t"){
    error <- qt(1-alpha/2, df=degree.fred)*stddev/sqrt(n)
    lower <- numeric(10000)
    upper <- numeric(10000)
    
    # loop sampling / estimation / CI
    for(i in 1:10000) {
      Y <- rt(n,df=degree.fred,ncp=center)
      lower[i] <- mean(Y) - qt(1-alpha/2, df=degree.fred)*sd(Y)/sqrt(n)
      upper[i] <- mean(Y) + qt(1-alpha/2, df=degree.fred)*sd(Y)/sqrt(n)
    }
    
    
  }
  
  
  
  if(distr=="binom"){
    error<-qnorm(1-alpha/2)*stddev
    
    lower <- numeric(10000)
    upper <- numeric(10000)
    
    # loop sampling / estimation / CI
    for(i in 1:10000) {
      Y <- rbinom(n,size=1,prob=center)
      pp<-sqrt(mean(Y) * (1 - (mean(Y)))/n)
      lower[i] <- mean(Y) - qnorm(1-alpha/2)*pp
      upper[i] <- mean(Y) + qnorm(1-alpha/2)*pp
    }
    
  }
  
  if((distr=="binom")&(n>=50)){
    #Yates continuity as in prop.test
    YATES <- 0.5
    z<-qnorm(1-alpha/2)
    YATES <- min(YATES, abs(1 - n * 0.05)) 
    z22n <- z^2/(2 * n)
    p.c <- center + YATES/n
    upper_bound<- (p.c + z22n + z * sqrt(p.c * (1 - p.c)/n + z22n/(2 * n)))/(1 + 2 * z22n)
    p.c <- center - YATES/n
    lower_bound<- (p.c + z22n - z * sqrt(p.c * (1 - p.c)/n + z22n/(2 * n)))/(1 + 2 * z22n)
    
    error<-qnorm(1-alpha/2)*stddev
    lower_bound1 <- center - error
    upper_bound1 <- center + error
    
    cat("(lower bound; upper bound)\n------------------------------------------------\n");
    print(c(lower_bound1,upper_bound1), digits = 5, na.print = "")
    
    
    cat("(lower bound; upper bound) with Yates correction for continuity\n------------------------------------------------\n")
    print(c(lower_bound,upper_bound), digits = 5, na.print = "")
    
    cat("confidence level\n------------------------------------------------\n");
    print(1-alpha, digits = 5, na.print = "")
    cat("the statistical margin of error\n------------------------------------------------\n");
    print(error, digits = 5, na.print = "")
    
    
    
    #return(c(lower_bound,upper_bound))
    lower <- numeric(10000)
    upper <- numeric(10000)
    
    # loop sampling / estimation / CI
    for(i in 1:10000) {
      Y <- rbinom(n,size=1,prob=center)
      pp<-sqrt(mean(Y) * (1 - (mean(Y)))/n)
      lower[i] <- mean(Y) - qnorm(1-alpha/2)*pp
      upper[i] <- mean(Y) + qnorm(1-alpha/2)*pp
    }
    
    
    CIs <- cbind(lower, upper)
    
    ID <- which(!(CIs[1:200, 1] <= center & center <= CIs[1:200, 2]))
    timesout<-length(which(!(CIs[, 1] <= center & center <= CIs[, 2])))/dim(CIs)[1]
    timesoutperc<-(1-timesout)*100
    
    # initialize the plot
    plot(center, 
         xlim = c(lower_bound, upper_bound), 
         ylim = c(1, 200), 
         ylab = "Simulation from the first 200 samples", 
         xlab = "", 
         main = "Confidence interval")
    
    
    
    #fix the color
    colors <- rep(gray(0.4), 200)
    colors[ID] <- "red"
    for(j in 1:200) {
      lines(c(CIs[j, 1], CIs[j, 2]), 
            c(j, j), 
            col = colors[j], 
            lwd = 2)
    } 
    
    abline(v = lower_bound, lty = 2,lwd=2,col="orange")
    abline(v = upper_bound, lty = 2,lwd=2,col="orange")
    abline(v = center, lty = 2,lwd=2)
    mtext(paste(timesoutperc,"%","coverage from the simulation"),side=1,line=2)    
    
    
  } 
  
  if((distr=="Standard Normal")|(distr=="t")|((distr=="binom")&(n<50))){
    
    
    
    lower_bound <- center - error
    upper_bound <- center + error
    
    cat("confidence level\n------------------------------------------------\n");
    print(1-alpha, digits = 5, na.print = "")
    cat("the statistical margin of error\n------------------------------------------------\n");
    print(error, digits = 5, na.print = "")
    cat("(lower bound; upper bound)\n------------------------------------------------\n");
    print(c(lower_bound,upper_bound), digits = 5, na.print = "")
    
    
    CIs <- cbind(lower, upper)
    
    ID <- which(!(CIs[1:200, 1] <= center & center <= CIs[1:200, 2]))
    timesout<-length(which(!(CIs[, 1] <= center & center <= CIs[, 2])))/dim(CIs)[1]
    timesoutperc<-(1-timesout)*100
    # initialize the plot
    plot(center, 
         xlim = c(lower_bound, upper_bound), 
         ylim = c(1, 200), 
         ylab = "Simulation from the first 200 samples", 
         xlab = "", 
         main = "Confidence interval")
    
    
    
    #fix the color
    colors <- rep(gray(0.4), 200)
    colors[ID] <- "red"
    for(j in 1:200) {
      lines(c(CIs[j, 1], CIs[j, 2]), 
            c(j, j), 
            col = colors[j], 
            lwd = 2)
    } 
    
    abline(v = lower_bound, lty = 2,lwd=2,col="orange")
    abline(v = upper_bound, lty = 2,lwd=2,col="orange")
    abline(v = center, lty = 2,lwd=2)
    
    mtext(paste(timesoutperc,"%","coverage from the simulation"),side=1,line=2)
    
    if((distr=="binom")|((distr=="binom")&(n>=50))){mtext("based on Binomial distribution",side=3)}
    if(distr=="t"){mtext("based on t-distribution",side=3) }
    if(distr=="Standard Normal"){mtext("based on Standard Normal distribution",side=3)}
  }
  
}





