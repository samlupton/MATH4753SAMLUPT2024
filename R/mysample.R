#' The Sample Function
#'
#' @param n Integer. The number of samples to draw in each iteration.
#' @param iter Integer. The number of iterations to perform.
#' @param time Numeric. The amount of time (in seconds) to pause between iterations.
#'
#' @return This function does not return a value. It produces a series of bar plots displaying the distribution of the sampled data.
#' @export
#'
#' @examples
#' # Example usage of the mysample function:
#' mysample(n = 20, iter = 5, time = 1)
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    s=sample(1:10,n,replace=TRUE)
    sf=factor(s,levels=1:10)
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    Sys.sleep(time)
  }
}
