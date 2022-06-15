
###############p-values
rm(list=ls())
while (dev.cur()>1) dev.off()
if(!is.null(dev.list())) dev.off()



t.pvalues<-function(tCrit, tObs,deg.freed,side){
  tGrid= seq(-10,10,by = 0.01)
  tCrit1= -tCrit
     plot(tGrid, (dt(tGrid,df=deg.freed-1)), type = "l", xlab = "X", ylab = "density",lwd=1)
    legend(x = "topleft", inset=.05, legend = c("alpha", "P-value"), pch = c(19,19),
           cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","red"), bty="n")
    
    tPlot = function(x,df){dt(x,df=deg.freed-1)}  
  
    
    if(side=="L"){

    
    x1 = seq(from = min(tGrid), to = tCrit1, length.out = 1000) 
    y1 = tPlot(x1,df)
    x1 = c(tCrit1,min(tGrid), x1, tCrit1) 
    
    y1 = c(y1,0, 0, 0)
    polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")
    
    
    xp1 = seq(from = min(tGrid), to = -tObs, length.out = 1000) 
    yp1 = tPlot(xp1,df)
    xp1 = c(-tObs,min(tGrid), xp1, -tObs) 
    
    yp1 = c(yp1,0, 0, 0)
    lines(xp1,yp1, lwd=4,  col = "red")
    
  }
  if(side=="R"){
    
   x = seq(from = tCrit, to = max(tGrid), length.out = 1000) 
   y = tPlot(x,df)
  
    x = c(tCrit, x, max(tGrid), tCrit) 
    y = c(0, y, 0, 0)
  
     polygon(x,y, lty = 3, border = NULL, col = "Light blue")
  
    xp = seq(from = tObs, to = max(tGrid), length.out = 1000) 
     yp = tPlot(xp,df)
    xp = c(tObs, xp, 10, tObs) 
     yp = c(0, yp, 0, 0)
   lines(xp,yp, lwd=4,  col = "red")
  }
  if(side=="B"){
    

legend(x = "topleft", inset=.05, legend = c("alpha/2", "P-values/2"), pch = c(19,19),
       cex = c(1,1), pt.lwd = c(NA,NA), col = c("light blue","red"), bty="n")

tPlot = function(x,df){dt(x,df=deg.freed-1)} 

x = seq(from = tCrit, to = max(tGrid), length.out = 1000) 
y = tPlot(x,df)

x = c(tCrit, x, max(tGrid), tCrit) 
y = c(0, y, 0, 0)

polygon(x,y, lty = 3, border = NULL, col = "Light blue")

x1 = seq(from = min(tGrid), to = tCrit1, length.out = 1000) 
y1 = tPlot(x1,deg.freed)
x1 = c(tCrit1,min(tGrid), x1, tCrit1) 

y1 = c(y1,0, 0, 0)
polygon(x1,y1, lty = 3, border = NULL, col = "Light blue")

#################

xp = seq(from = tObs, to = max(tGrid), length.out = 1000) 
yp = tPlot(xp,df)
xp = c(tObs, xp, 10, tObs) 
yp = c(0, yp, 0, 0)
lines(xp,yp, lwd=4,  col = "red")

xp1 = seq(from = min(tGrid), to = -tObs, length.out = 1000) 
yp1 = tPlot(xp1,df)
xp1 = c(-tObs,min(tGrid), xp1, -tObs) 

yp1 = c(yp1,0, 0, 0)
lines(xp1,yp1, lwd=4,  col = "red")

}
}


t.pvalues(tCrit = qt(0.975,28),tObs= 3.0866537,deg.freed=28,side="R")
