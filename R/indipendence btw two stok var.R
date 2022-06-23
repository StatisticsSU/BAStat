#' Indipendence between two random variables
#'
#' @param xx the first numerical variable, x
#' @param yy the second numerical variable, y
#' @param pxxyy the distribution function between the two variables; it has to be presented as a matrix, even rectangular
#' @return the thoretical distribution function obtained by multiplying the marginal distributions: p(x=x_iy=y_j)=p(x=x_i)*p(y=y_j)
#' @export
#' @examples
#' library(BAStat)
#' y<-c(6,7,8)
#' x<-c(10,20)
#' pxy<-matrix(
#'  c(0.1,0.2,0.1,0.2,0.3,0.1),
#'  nrow=2,ncol=3) 
#'  indip(xx=x,yy=y,pxxyy=pxy)



indip<-function(xx,yy,pxxyy){
  
  px<-marginSums(pxxyy,2)
  py<-marginSums(pxxyy,1)
  n<-length(px)
  m<-length(py)
  teoxy<-matrix(0,nrow=n,ncol=m)
  pxy
  
  for(i in 1:n){
    for(j in 1:m){
      teoxy[i,j]<-px[i]*py[j]
      
    }
  } 
  
  # colnames(teoxy)<-(yy)
  #rownames(teoxy)<-(xx)
  teoxy<-t(teoxy)
  return(teoxy)
  
}

