#' Compute for one quantitative variable essential measurements
#'
#' @param x quantitative variable (continous or discrete)
#' @export 
#' @examples
#' library(BAStat)
#' x<-c(25,25,26,26,27,27,28,28,29,30,30,31)
#' describe(x)


describe<-function(x) {
  c( n=length(x), summary(x)[1], summary(x)[2],
     summary(x)[3],summary(x)[4],
     summary(x)[5],summary(x)[6],
     variance = var(x),st.dev=sd(x))
}
