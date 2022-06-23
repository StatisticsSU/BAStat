#' Expected value of product of random variables E(xy)
#'
#' This function is useful for the covariance and correlation between two random variables. It is not substituting the command `cov` or `cor` but integrating them.
#' @param xx the first numerical variable, x
#' @param yy the second numerical variable, y
#' @param pxxyy the distribution function between the two variables; it has to be presented as a matrix, even rectangular
#' @return the expected value of product of random variables: E(XY)=sum_i sum_j x_i y_j p(x_iy_j)
#' @export
#' @examples
#' library(BAStat)
#' pxy<-matrix(
#' c(0.1, 0.05,  0.00, 0.15,
#'  0.05, 0.10,  0.15, 0.10,
#'  0.10, 0.05,  0.10, 0.05),
#' nrow=4,ncol=3)
#' y<-c(3,5,4)
#' x<-c(2,4,6,8)
#' Exy(xx=x,yy=y,pxxyy=pxy)


Exy<-function(xx,yy,pxxyy){
  
  
  n<-length(xx)
  m<-length(yy)
  exy<-matrix(0,nrow=n,ncol=m)
  
  
  for(i in 1:n){
    for(j in 1:m){
      exy[i,j]<-xx[i]*yy[j]*pxxyy[i,j]
      
    }
  }
  sumexy<-sum(apply(exy,2,sum))
  return(sumexy)
  
}
