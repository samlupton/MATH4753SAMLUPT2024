#' My CLT Poisson Distribution
#'
#' This function demonstrates the Central Limit Theorem (CLT) using a Poisson distribution. It simulates random samples from a Poisson distribution with a specified lambda parameter, calculates the sample means over multiple iterations, and creates graphical representations of the results. These include a histogram of the sample means, a barplot of the relative frequencies of the Poisson-distributed samples, and a plot of the theoretical Poisson probability mass function.
#'
#' @param n Integer. The sample size for each iteration (i.e., the number of random values drawn from the Poisson distribution in each iteration).
#' @param iter Integer. The number of iterations to perform. In each iteration, a new random sample of size `n` is drawn, and the sample mean is calculated.
#' @param lambda Numeric. The rate parameter (lambda) for the Poisson distribution. The default value is 10.
#' @param ... Additional graphical parameters passed to the `hist` function for customizing the histogram of the sample means.
#'
#' @return This function does not return a value but produces three plots:
#' \itemize{
#'   \item A histogram of the sample means with an overlaid theoretical normal curve.
#'   \item A barplot showing the relative frequencies of the sampled Poisson values.
#'   \item A plot of the theoretical Poisson probability mass function.
#' }
#' @export
#'
#' @examples
#' # Example usage of the function with n = 10, iter = 1000, lambda = 5
#' mycltp(n = 10, iter = 1000, lambda = 5)
mycltp = function(n, iter, lambda = 10, ...) {
  ## r-random sample from the Poisson
  y = stats::rpois(n * iter, lambda = lambda)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w = apply(data, 2, mean)
  ## We will make a histogram of the values in w
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param = graphics::hist(w, plot = FALSE)
  ## Since the histogram will be a density plot we will find the max density
  ymax = max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax = 1.1 * ymax
  ## Make a suitable layout for graphing
  graphics::layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE))
  ## Now we can make the histogram
  graphics::hist(w, freq = FALSE, ylim = c(0, ymax), col = rainbow(max(w)),
                 main = paste("Histogram of sample mean", "\n", "sample size= ", n, " iter=", iter, " lambda=", lambda, sep = ""),
                 xlab = "Sample mean", ...)
  ## Add a theoretical normal curve
  curve(stats::dnorm(x, mean = lambda, sd = sqrt(lambda/n)), add = TRUE, col = "Red", lty = 2, lwd = 3)
  ## Now make a new plot
  ## Since y is discrete we should use a barplot
  graphics::barplot(table(y) / (n * iter), col = rainbow(max(y)), main = "Barplot of sampled y",
                    ylab = "Rel. Freq", xlab = "y")
  ## Plot the theoretical Poisson probability mass function
  x = 0:max(y)
  plot(x, stats::dpois(x, lambda = lambda), type = "h", lwd = 5, col = rainbow(max(y)),
       main = "Probability function for Poisson", ylab = "Probability", xlab = "y")
}
