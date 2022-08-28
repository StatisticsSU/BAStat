#' Confidence intervals (CI) and tests (T) for the mean of two populations. 
#' Tests H0: mu1 = mu2 vs H1: mu1 != mu2
#' This function is a guide to the test between for the mean of two population when the samples are independent.
#' 
#' @param x it is a numerical variable.
#' @param y it is a categorical variable.
#' @param datas it is the dataset in data.frame format.
#' @param alpha significance level.
#' @param step if it is *"1: Describe"* it is the first step with length, mean, standard deviation and standard error for each group and it is provided a boxplot for the numerical variable in the two subgroups; 
#' @param step if it is *"2: Analyze per group"* it is the second step with confidence intervals for the mean and the variance for each group; the normality is provided by Shapiro test and on the right side the qqPlot for the numerical variable subsetted by the categorical variable is also provided; for the qqplot is required to install "car"
#' @param step if it is *"3: Check the variances"* it is the third step the test F for the equality of the variance for the population: H0: sigma_1=sigma_2. If the p-value is bigger than the significance level than do not reject the null hypothesis means that it can be assumed that the mean of the two population comes from two Normal distributions with the same variance.
#' @param step if it is *"4: t-test for two pop. means"* it is the fourth step and it is based on t-test; the t-test is provided with equal variances (pooled variance in t-statistics) if it was not rejected the null hypothesis at the third step; the t-test for unequal variances (Welch test) if it was rejected the null hypothesis at the third step.
#' @return four steps: 1-descriptive statistics for each group; 2-confidence interval for the mean and the variance for each group; 3-test F check if the variances can be assumed equal or not; 4-t-test based on the assumption or not of equal variances. For step 3 and 4 are drawn plots in order to reject the null hypothesis
#' @export
#' @examples
#' library(BAStat)
#' 
#' xy<-c(29,	25,	24,	26,	29,	31,	30,	28,	32,	22, 14,	12,	11,	15,	26,	18,	27,	25,	48,	11)
#' group<-c(rep("Bus A",10),rep("Bus B",10))
#' data1<-data.frame(xy,group)
#'
#' CIT2pop(x=data1$xy,y=data1$group,datas=data1,alpha=0.05,step="1: Describe")
#' CIT2pop(x=data1$xy,y=data1$group,datas=data1,alpha=0.05,step="2: Analyze per group")
#' CIT2pop(x=data1$xy,y=data1$group,datas=data1,alpha=0.05,step="3: Check the variances")
#' CIT2pop(x=data1$xy,y=data1$group,datas=data1,alpha=0.05,step="4: t-test for two pop. means")
#' #one more example
#' ex2<-c(29,	35,	41,	26,	29,	37,	31,	28,	32, 34,	22,	19,	25,	26,	28,	35)		
#' group<-c(rep("Bus A",9),rep("Bus B",7))
#' data2<-data.frame(ex2,group)
#'
#' CIT2pop(x=data2$ex2,y=data2$group,datas=data2,alpha=0.05,step="1: Describe")
#' CIT2pop(x=data2$ex2,y=data2$group,datas=data2,alpha=0.05,step="2: Analyze per group")
#' CIT2pop(x=data2$ex2,y=data2$group,datas=data2,alpha=0.05,step="3: Check the variances")
#' CIT2pop(x=data2$ex2,y=data2$group,datas=data2,alpha=0.05,step="4: t-test for two pop. means")



CIT2pop<-function(x,y,datas,alpha,step="1: Describe"){
  group1<-split(x = x, f = y)[1]
  group2<-split(x = x, f = y)[2]
  
  if(step=="1: Describe"){
    boxplot(x~y,data=datas,col=c("brown","orange"))
    
    
    
    basic1<-function(x,alpha){
      n    =length(x)
      
      mean =  round(mean(x, na.rm = TRUE), 4)
      sd   = round(sd(x, na.rm = TRUE), 4)
      se   = round(sd / sqrt(n), 4)
      
      
      tab<-c(n,mean,sd,se)
      names(tab)<-c("n","Mean","sd","se")
      return(tab)
      
    }
    
    
    xx<-rbind(basic1(group1[[1]],alpha),basic1(group2[[1]],alpha))
    rownames(xx)<-c(names(group1),names(group2))
    # txx<-t(xx) 
    
    
    cat("\nDescriptive statistics per group\n------------------------------------------------\n")
    
    return(xx)
    
    
  }
  
  
  if(step=="2: Analyze per group"){
    library(car)
    qqPlot(x~y,data=datas) 
    
    #step2
    #  group1<-split(x = x, f = y)[1]
    #  group2<-split(x = x, f = y)[2]
    
    basic2<-function(x,alpha){
      n    =length(x)
      
      mean =  round(mean(x, na.rm = TRUE), 4)
      sd   = round(sd(x, na.rm = TRUE), 4)
      se   = round(sd / sqrt(n), 4)
      
      lower.ci =  round(mean - qt(1 - (alpha / 2), n - 1) * se, 4)
      upper.ci =  round(mean + qt(1 - (alpha / 2), n - 1) * se, 4)
      
      
      low_end <- round(sqrt((n-1)*sd*sd/qchisq(alpha/2,n-1,lower.tail=FALSE)), 4)
      high_end <- round(sqrt((n-1)*sd*sd/(qchisq(alpha/2,n-1))), 4)
      shapiro.stat<-round(shapiro.test(x)$statistic, 4)
      shapiro.pvalue<-round(shapiro.test(x)$p.value, 4)
      
      # tab<-c(n,mean,sd,se,lower.ci,upper.ci,low_end,high_end,shapiro.stat,shapiro.pvalue)
      #  names(tab)<-c("n","Mean","sd","se","95% Mean LCI", "95% Mean UCI","95% ST. Dev. LCI", 
      #               "95% ST. Dev. UCI", "Shapiro stat","Shapiro p-value")
      tab<-c(lower.ci,upper.ci,low_end,high_end,shapiro.stat,shapiro.pvalue)
      names(tab)<-c("95% Mean LCI", "95% Mean UCI","95% ST. Dev. LCI", 
                    "95% ST. Dev. UCI", "Shapiro stat","Shapiro p-value")
      return(tab)
      
    }
    
    
    
    xx<-rbind(basic2(group1[[1]],alpha)[1:4],basic2(group2[[1]],alpha)[1:4])
    rownames(xx)<-c(names(group1),names(group2))
    #txx<-t(xx) 
    
    cat("\nInference per group\n------------------------------------------------\n")
    print(xx, digits = 5, na.print = "")
    cat("\nNormality per group\n------------------------------------------------\n")
    
    xx1<-rbind(basic2(group1[[1]],alpha)[5:6],basic2(group2[[1]],alpha)[5:6])
    rownames(xx1)<-c(names(group1),names(group2))
    
    print(xx1, digits = 5, na.print = "")
    # return(xx)
    
  }
  
  #step3 check variances
  if(step=="3: Check the variances"){
    
    fstat<-var.test(x~y, data=datas)
    par(mfrow=c(1,2))
    deg.fred1<-length(group1[[1]])-1
    deg.fred2<-length(group2[[1]])-1
    
    Obs<-f.stat<-var(group1[[1]])/var(group2[[1]])
    Crit<-pf(1-alpha/2,df1=deg.fred1,df2=deg.fred2)
    
    pObs<-pf(f.stat,df1=deg.fred1,df2=deg.fred2)
    
    max.x<-max(Crit,Obs)+3
    min.x<-max(0,f.stat-3)
    Grid= seq(min.x,max.x,by = 0.01)
    
    
    
    plot(Grid, df(Grid,df1=deg.fred1,df2=deg.fred2), type = "l", xlab = "F", ylab = "Fisher density",lwd=1,xaxt="n")
    axis(1,seq(min.x,max.x,by=1))
    FPlot = function(x,df1,df2){df(x,df1=deg.fred1,df2=deg.fred2)}  
    
    upper.x<-max.x
    #Crit<-qf(1-alpha/2,df1=deg.fred1,df2=deg.fred2)
    # Obs<-f.stat
    
    points(Crit,0, col = "blue", pch = 19)
    points(Obs,0, col = "red", pch = 19)
    
    if(Obs>Crit){
      mtext(~italic("Reject null hypothesis: \n unequal pop. variances"), side=3)
    }
    
    if(Obs<=Crit){
      mtext(~italic("No Reject null hypothesis: \n  equal pop. variances"), side=3)
    }
    
    legend(x = "topright", inset=.05, legend = c(c("Crit",round(Crit,4)), c("F-stat",round(Obs,4))), pch = c(19,19),
           cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n",pt.cex = 0.7)
    
    ###############
    x = seq(from = max(Crit,Obs), to = max(Grid), length.out = 1000) 
    y = FPlot(x,df1=deg.fred1,df2=deg.fred2)
    
    plot(x, y, type = "l", xlab = "F", ylab = "Zoom on the right tail",lwd=1,xaxt="n")
    axis(1,seq(round(max(Crit,Obs),1),round(max(Grid),1),by=1))
    x = c(max(Crit,Obs), x, max(Grid), Crit) 
    y = c(0, y, 0, 0)
    
    polygon(x,y, lty = 3, border = NULL, col = "Light Blue")
    
    xp = seq(from = Obs, to = max(Grid), length.out = 1000) 
    yp = FPlot(xp,df1=deg.fred1,df2=deg.fred2)
    xp = c(Obs, xp, max(Grid), Obs) 
    yp = c(0, yp, 0, 0)
    polygon(xp,yp,  col = "red")
    
    
    if(fstat$p.value/2>=alpha/2){
      mtext(~italic("Reject null hypothesis \n unequal pop.variances"), side=3)
    }
    if(fstat$p.value/2<alpha/2){
      mtext(~italic("No Reject null hypothesis \n equal pop.variances"), side=3)
    }
    
    legend(x = "topright", inset=.05, legend = c(c("alpha/2:", round(alpha/2,4),"P-value/2:",round(fstat$p.value/2,4))), pch = c(19,19),
           cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n",pt.cex=0.7)
    
    
    
    return(fstat)
  }
  
  if(step=="4: t-test for two pop. means"){
    
    
    if(var.test(x~y, data=datas)$p.value/2>=alpha/2){
      
      # print(comp_table, digits = 5, na.print = "")
      testequal<-t.test(x~y, alternative = "two.sided", var.equal = TRUE, data=datas)
      degree.freed<-t.test(x~y, alternative = "two.sided", var.equal = TRUE, data=datas)$parameter[[1]]
      
      # deg.fred1<-length(group1[[1]])-1
      # deg.fred2<-length(group2[[1]])-1
      # degree.freed<-deg.fred1+deg.fred2
      Crit<-qt(1-alpha/2,df=degree.freed)
      Crits<-abs(Crit)
      Obs<-t.test(x~y, alternative = "two.sided", var.equal = TRUE, data=datas)$statistic[[1]]
      
      Obsa<-abs(Obs)
      
      par(mfrow=c(1,2))
      min.x<-min(Crits,Obs)-3
      max.x<-max(Crits,Obs)+3
      tGrid= seq(min.x,max.x,by = 0.01)
      plot(tGrid, (dt(tGrid,df=degree.freed)), type = "l", xlab = "X", ylab = "t-Student density",lwd=1)
      
      
      points(Crit,0, col = "blue", pch = 19)
      points(Obs,0, col = "red", pch = 19)
      
      
      if(Obsa>=Crits){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(Obsa<Crits){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
      legend(x = "topright", inset=.05, legend =c(c("Crit",round(Crit,4)), c("Obs",round(Obs,4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n",pt.cex=0.7)
      
      
      #   p.a<- alpha/2
      #   p.v<-round(t.test(x~y, alternative = "two.sided", var.equal = TRUE, data=datas)$p.value/2,4)
      #   decide<-ifelse(p.v>p.a,"No reject H0","Reject H0")
      #   comp_table<-c(p.v,p.a,decide)
      #  names(comp_table)<-c("p-value/2","alpha/2","decision")
      
      
      tGrid= seq(min.x,max.x,by = 0.01)
      plot(tGrid, (dt(tGrid,df=degree.freed)), type = "l", xlab = "X", ylab = "t-Student density",lwd=1)
      tPlot = function(x,df=degree.freed){dt(x,df=degree.freed)}  
      
      
      Crit<-qt(alpha/2,df=degree.freed)
      Crits<-abs(Crit)
      Obs<-t.test(x~y, alternative = "two.sided", var.equal = TRUE, data=datas)$statistic[[1]]
      
      Crits<-abs(Crit)
      Obsa<-abs(Obs)
      
      
      x = seq(from = Crits, to = max(tGrid), length.out = 1000) 
      y = tPlot(x,df=degree.freed)
      
      x = c(Crits, x, max(tGrid), Crits) 
      y = c(0, y, 0, 0)
      
      polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      
      xp = seq(from = Obsa, to = max(tGrid), length.out = 1000) 
      yp = tPlot(xp,df=degree.freed)
      xp = c(Obsa, xp, 10, Obsa) 
      yp = c(0, yp, 0, 0)
      lines(xp,yp, lwd=4,  col = "red")
      
      
      legend(x = "topright", inset=.05, legend = c(c("alpha/2:", round(pt(-Crits,degree.freed),4),"P-value/2:",round(pt(-Obsa,degree.freed),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n",pt.cex = 0.7)
      
      if(max(yp)<=max(y)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      #if(1-pt(Obs,degree.freed) >= 1-pt(Crit,degree.freed))
      if(max(yp)>max(y)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      legend(x = "topright", inset=.05, legend = c(c("alpha/2:", round(1-pt(Crits,degree.freed),4),"P-value/2:",round(1-pt(Obsa,degree.freed),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n",pt.cex = 0.7)
      
      
      #################
      
      #    xp1 = seq(from = min(tGrid), to = -Obsa, length.out = 1000) 
      #    yp1 = tPlot(xp1,df=degree.freed)
      #    xp1 = c(-Obsa,min(tGrid), xp1, -Obsa) 
      
      #   yp1 = c(yp1,0, 0, 0)
      #    lines(xp1,yp1, lwd=4,  col = "red")
      
      #    x1 = seq(from = min(tGrid), to = -Crits, length.out = 1000) 
      #    y1 = tPlot(x1,df=degree.freed)
      #    x1 = c(-Crits,min(tGrid), x1, -Crits) 
      
      #    y1 = c(y1,0, 0, 0)
      #    polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
      
      
      #     if(max(yp1)<=max(y1)){
      #       mtext(~italic("Reject null hypothesis"), side=3)
      #    }
      #    if(max(yp1)>max(y1)){
      #      mtext(~italic("No Reject null hypothesis"), side=3)
      #   }
      
      # if(pt(Obs,degree.freed)>=pt(Crit,degree.freed))
      
      
      #Obs<- qt(Obs,df=deg.fred)
      #Obsa<-abs(Obs)
      cat("\nInference across groups: t-test for pop. mean \n based on equal variances\n------------------------------------------------\n")
      
      
      return(testequal)
      
      
    }else{
      
      unequal<- t.test(x~y, alternative = "two.sided", var.equal = FALSE, data=datas)
      
      #  p.a<- alpha/2
      #   p.v<-round(t.test(x~y, alternative = "two.sided", var.equal = FALSE, data=datas)$p.value/2,4)
      #  decide<-ifelse(p.v>p.a,"No reject H0","Reject H0")
      #  comp_table<-c(p.v,p.a,decide)
      #   names(comp_table)<-c("p-value/2","alpha/2","decision")
      degree.freed<-t.test(x~y, alternative = "two.sided", var.equal = FALSE, data=datas)$parameter[[1]]
      
      Crit<-qt(1-alpha/2,df=degree.freed)
      Crits<-abs(Crit)
      Obs<-t.test(x~y, alternative = "two.sided", var.equal = FALSE, data=datas)$statistic[[1]]
      
      Obsa<-abs(Obs)
      
      par(mfrow=c(1,2))
      min.x<-min(Crits,Obs)-3
      max.x<-max(Crits,Obs)+3
      tGrid= seq(min.x,max.x,by = 0.01)
      plot(tGrid, (dt(tGrid,df=degree.freed)), type = "l", xlab = "X", ylab = "t-Student density",lwd=1)
      
      
      points(Crit,0, col = "blue", pch = 19)
      points(Obs,0, col = "red", pch = 19)
      
      
      if(Obsa>=Crits){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      if(Obsa<Crits){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
      legend(x = "topright", inset=.05, legend =c(c("Crit",round(Crit,4)), c("Obs",round(Obs,4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("blue","blue","red","red"), bty="n",pt.cex=0.7)
      
      
      #   p.a<- alpha/2
      #   p.v<-round(t.test(x~y, alternative = "two.sided", var.equal = TRUE, data=datas)$p.value/2,4)
      #   decide<-ifelse(p.v>p.a,"No reject H0","Reject H0")
      #   comp_table<-c(p.v,p.a,decide)
      #  names(comp_table)<-c("p-value/2","alpha/2","decision")
      
      
      tGrid= seq(min.x,max.x,by = 0.01)
      plot(tGrid, (dt(tGrid,df=degree.freed)), type = "l", xlab = "X", ylab = "t-Student density",lwd=1)
      tPlot = function(x,df=degree.freed){dt(x,df=degree.freed)}  
      
      
      Crit<-qt(alpha/2,df=degree.freed)
      Crits<-abs(Crit)
      Obs<-t.test(x~y, alternative = "two.sided", var.equal = TRUE, data=datas)$statistic[[1]]
      
      Crits<-abs(Crit)
      Obsa<-abs(Obs)
      
      legend(x = "topright", inset=.05, legend = c(c("alpha/2:", round(1-pt(Crits,degree.freed),4),"P-value/2:",round(1-pt(Obsa,degree.freed),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n",pt.cex=0.7)
      
      x = seq(from = Crits, to = max(tGrid), length.out = 1000) 
      y = tPlot(x,df=degree.freed)
      
      x = c(Crits, x, max(tGrid), Crits) 
      y = c(0, y, 0, 0)
      
      polygon(x,y, lty = 3, border = NULL, col = "Light blue")
      
      xp = seq(from = Obsa, to = max(tGrid), length.out = 1000) 
      yp = tPlot(xp,df=degree.freed)
      xp = c(Obsa, xp, 10, Obsa) 
      yp = c(0, yp, 0, 0)
      lines(xp,yp, lwd=4,  col = "red")
      
      
      legend(x = "topright", inset=.05, legend = c(c("alpha/2:", round(pt(-Crits,degree.freed),4),"P-value/2:",round(pt(-Obsa,degree.freed),4))), pch = c(19,19),
             cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","light blue","red","red"), bty="n",pt.cex = 0.7)
      
      if(max(yp)<=max(y)){
        mtext(~italic("Reject null hypothesis"), side=3)
      }
      
      #if(1-pt(Obs,degree.freed) >= 1-pt(Crit,degree.freed))
      if(max(yp)>max(y)){
        mtext(~italic("No Reject null hypothesis"), side=3)
      }
      
      
      
      cat("\nInference across groups: t-test for pop. mean \n based on unequal variances\n------------------------------------------------\n")
      #   print(comp_table, digits = 5, na.print = "")
      
      return(unequal)
      
    }
  }
  
}

