#' The My Boot Function
#'
#' This function performs a parametric bootstrap procedure for a specified sample statistic (default is the mean).
#' It generates bootstrap samples, calculates the bootstrap distribution of the sample statistic, and provides a
#' confidence interval. A histogram of the bootstrap sample statistics is displayed along with the confidence
#' interval and the point estimate.
#'
#' @param iter Integer. The number of bootstrap iterations to perform. Default is 10000.
#' @param x Numeric vector. The original sample data for which the bootstrap procedure is applied.
#' @param fun Character or function. The function to be applied to each bootstrap sample (e.g., "mean", "median").
#'            Default is "mean".
#' @param alpha Numeric. The significance level for constructing the confidence interval. Default is 0.05.
#' @param cx Numeric. The character expansion (cex) value used for displaying text in the plot. Default is 1.5.
#' @param ... Additional graphical parameters to be passed to the `hist` function for customizing the histogram plot.
#'
#' @return A list containing the following elements:
#'         \item{ci}{The bootstrap confidence interval for the specified statistic.}
#'         \item{fun}{The function used to calculate the sample statistic.}
#'         \item{x}{The original sample data.}
#'
#' @export
#'
#' @importFrom graphics abline hist segments text
#' @importFrom stats quantile
#'
#' @examples
#' # Example usage:
#' set.seed(123)
#' sample_data <- rnorm(50, mean = 5, sd = 2)
#' myboot2(iter = 1000, x = sample_data, fun = "mean", alpha = 0.05, cx = 1.2)
#'
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)   #sample size
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)
  text(pte,max(para$density)/2,round(pte,2),cex=cx)
  invisible(list(ci=ci,fun=fun,x=x))
}
