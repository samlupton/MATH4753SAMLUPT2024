#' Maximum Likelihood Estimation
#'
#' This function performs maximum likelihood estimation for a given likelihood function
#' and plots the resulting likelihood values against the specified parameters.
#'
#' @param lfun A function representing the likelihood function to be evaluated. It should take
#'              parameters in the form of `lfun(x, param)` where `x` is a vector of data points
#'              and `param` is a parameter value.
#' @param x A numeric vector of data points for which the likelihood function will be evaluated.
#' @param param A numeric vector of parameter values at which the likelihood function will be evaluated.
#' @param ... Additional arguments to be passed to the plotting function (e.g., main, xlab, ylab).
#'
#' @return A list containing:
#'         - `i`: The index of the maximum likelihood estimate.
#'         - `parami`: The parameter value that maximizes the likelihood.
#'         - `yi`: The maximum likelihood value at `parami`.
#'         - `slope`: The estimated slope of the likelihood function at the maximum, or "NA" if not computable.
#' @importFrom graphics points
#' @importFrom graphics axis
#' @export
#'
#' @examples
#' # Example usage of the mymaxlik function with a sample likelihood function
#' sample_likelihood = function(x, theta) {
#'   dnorm(x, mean = theta, sd = 1)
#' }
#' data_points = rnorm(100, mean = 5, sd = 2)
#' results = mymaxlik(sample_likelihood, data_points, param = seq(0, 10, length = 100))
#'
mymaxlik=function(lfun,x,param,...){
  np=length(param)
  z=outer(x,param,lfun)
  y=apply(z,2,sum)
  plot(param,y,col="Blue",type="l",lwd=2,...)
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
