#' The myci Function
#'
#' This function calculates the 95% confidence interval for the population mean mu based on a single sample.
#' It uses the t-distribution and assumes the sample size is less than 30 or that the population variance is unknown.
#'
#' @param x A numeric vector representing the sample data. The function will calculate the sample mean and standard deviation.
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds of the 95% confidence interval for the population mean.
#' The vector is named "Lower" and "Upper" for the respective bounds.
#'
#' @export
#'
#' @importFrom stats qt sd
#'
#' @examples
#' # Example usage:
#' set.seed(23)
#' x <- rnorm(30, mean = 10, sd = 12)  # Generate a sample
#' myci(x)  # Call the myci function to get the 95% CI
#'
myci <- function(x) {
  # Sample statistics
  sample_mean <- mean(x)  # Sample mean
  sample_sd <- sd(x)      # Sample standard deviation
  n <- length(x)          # Sample size
  se <- sample_sd / sqrt(n)  # Standard error

  # t critical value for 95% confidence level
  t_critical <- qt(0.975, df = n - 1)  # For 95% CI, use 0.975 (two-tailed)

  # Margin of error
  margin_of_error <- t_critical * se

  # Confidence Interval
  ci_lower <- sample_mean - margin_of_error
  ci_upper <- sample_mean + margin_of_error

  # Return the confidence interval as a vector
  return(c(Lower = ci_lower, Upper = ci_upper))
}
