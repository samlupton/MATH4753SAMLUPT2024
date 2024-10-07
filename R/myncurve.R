#' The My Curve N Function
#'
#' This function plots the probability density function (PDF) of a normal distribution
#' with a given mean (`mu`) and standard deviation (`sigma`). It shades the area under
#' the curve between `-∞` and a given value `a`. The function also calculates the cumulative
#' probability \(P(X ≤ a)\) and returns it as part of the output.
#'
#' @param mu Numeric. The mean of the normal distribution.
#' @param sigma Numeric. The standard deviation of the normal distribution.
#' @param a Numeric. The value up to which the area under the curve is shaded and the cumulative probability is calculated.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{mu}{The mean of the normal distribution.}
#'   \item{sigma}{The standard deviation of the normal distribution.}
#'   \item{a}{The value up to which the area is shaded.}
#'   \item{probability}{The cumulative probability \(P(X ≤ a)\).}
#' }
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#' @export
#'
#' @examples
#' # Example usage of the myncurve function:
#' result <- myncurve(mu = 0, sigma = 1, a = 1.5)
#' print(result)
#'
#' # This will plot the standard normal curve, shade the area up to 1.5,
#' # and calculate the probability P(X <= 1.5).
#'
myncurve <- function(mu, sigma, a) {
  # Define x values over a range from mu - 3*sigma to mu + 3*sigma
  x <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 1000)

  # Plot the normal curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        xlab = "x",
        ylab = "Density",
        main = paste("Normal Curve with mu =", mu, "and sigma =", sigma))

  # Define x values for shading the area under the curve from -∞ to a
  x_values <- seq(mu - 3 * sigma, a, length = 1000)
  y_values <- dnorm(x_values, mean = mu, sd = sigma)

  # Shade the area under the curve between -∞ and a
  polygon(c(x_values, a, mu - 3 * sigma),
          c(y_values, 0, 0),
          col = "lightblue", border = NA)

  # Calculate the cumulative probability P(X <= a)
  area <- pnorm(a, mean = mu, sd = sigma)

  # Return the mean, standard deviation, value of a, and calculated probability
  return(list(mu = mu, sigma = sigma, a = a, probability = area))
}
