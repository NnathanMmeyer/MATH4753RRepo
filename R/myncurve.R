#'Creates a plot with a curve plotting the probabilities with a given interval a
#'
#'@param mu Average for a normal curve
#'@param sigma Standard deviation of a normal curve
#'@param a P(Y <= a)
#'
#' @return plot and list
#' @export
#'

myncurve = function(mu, sigma, a){
  curve(dnorm(a,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), col = "Red", lwd = 2)
  xcurve = seq(-1000, a, length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(-1000,xcurve,a),c(0,ycurve,0),col="Red")
  pnorm(a,mean=mu, sd=sigma)
}
