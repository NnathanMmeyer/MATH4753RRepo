#' My CLT function
#'
#' @param n sample size
#' @param iter how many iterations
#' @param a lower bound
#' @param b upper bound
#'
#' @return a vector of the sum of the random uniform distribution
#' @export
#'

myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  hist(sm)
  sm
}
