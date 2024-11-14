#' Bootstrapped p-value for hypothesis testing
#'
#' This function performs a bootstrapped hypothesis test for the difference between sample means. It computes the p-value by resampling the data and calculating the test statistic for each resample. The function also computes a confidence interval for the resampled mean values and visualizes the distribution of the resampled test statistics.
#'
#' @param x A numeric vector of data values. This is the sample for which you want to compute the test statistic and p-value.
#' @param conf.level A numeric value between 0 and 1 representing the confidence level for the confidence interval. Default is 0.95.
#' @param iter The number of bootstrap iterations to perform. Default is 3000.
#' @param mu0 The value of the null hypothesis mean. Default is 0.
#' @param test A character string specifying the type of hypothesis test. Can be one of:
#'   - "two" for a two-tailed test (default),
#'   - "upper" for a one-tailed test with the alternative hypothesis being that the mean is greater than `mu0`,
#'   - "lower" for a one-tailed test with the alternative hypothesis being that the mean is less than `mu0`.
#'
#' @return A list containing:
#'   - `pvalue`: The computed p-value based on the bootstrap resampling.
#'   - `tcalc`: The observed test statistic for the given data.
#'   - `n`: The sample size of the input data.
#'   - `x`: The original data vector.
#'   - `test`: The type of hypothesis test used.
#'   - `ci`: The confidence interval for the mean based on resampling.
#'
#' @importFrom graphics title
#'
#' @export
#'
#' @examples
#' # Example usage:
#' set.seed(123)
#' data <- rnorm(50, mean = 5, sd = 2)
#' result <- bootpval(data, conf.level = 0.95, iter = 3000, mu0 = 0, test = "two")
#' result$pvalue
#' result$ci
#'
#' # One-tailed test (upper):
#' result_upper <- bootpval(data, conf.level = 0.95, iter = 3000, mu0 = 0, test = "upper")
#' result_upper$pvalue
#'
#' # One-tailed test (lower):
#' result_lower <- bootpval(data, conf.level = 0.95, iter = 3000, mu0 = 0, test = "lower")
#' result_lower$pvalue
bootpval<-function(x,conf.level=0.95,iter=3000,mu0=0, test="two"){
  n=length(x)
  y=x-mean(x)+mu0  # transform the data so that it is centered at the NULL
  rs.mat<-c()    #rs.mat will become a resample matrix -- now it is an empty vector
  xrs.mat<-c()
  for(i in 1:iter){ # for loop - the loop will go around iter times
    rs.mat<-cbind(rs.mat,sample(y,n,replace=TRUE)) #sampling from y cbind -- column bind -- binds the vectors together by columns
    xrs.mat<-cbind(xrs.mat,sample(x,n,replace=TRUE)) #sampling from x cbind -- column bind -- binds the vectors together by columns

  }

  tstat<-function(z){ # The value of t when the NULL is assumed true (xbar-muo)/z/sqrt(n)
    sqrt(n)*(mean(z)-mu0)/sd(z)
  }

  tcalc=tstat(x) # t for the data collected
  ytstat=apply(rs.mat,2,tstat) # tstat of resampled y's, ytstat is a vector and will have iter values in it
  xstat=apply(xrs.mat,2,mean)  # mean of resampled x's
  alpha=1-conf.level # calculating alpha
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  pvalue=ifelse(test=="two",length(ytstat[ytstat>abs(tcalc) | ytstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(ytstat[ytstat>tcalc])/iter,
                       length(ytstat[ytstat<xstat])/iter))

  h=hist(ytstat,plot=FALSE)
  mid=h$mid
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }
  if(test=="upper"){
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Gray",length(mid)-ncolr),rep("Green",ncolr))
  }

  if(test=="lower"){
    ncoll=length(mid[mid<=  -abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll))
  }
  hist(ytstat,col=col,freq=FALSE,las=1,main="",xlab=expression(T[stat]))
  #segments(ci[1],0,ci[2],0,lwd=2)
  pround=round(pvalue,4)
  title(substitute(paste(P[value],"=",pround)))
  return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}
