#' @title Quadratic Function
#'
#' @param x A numeric vector or scalar representing the input values for which the quadratic function should be evaluated.
#' @param coefficients A numeric vector of length 3 containing the coefficients for the quadratic function.
#'        The first element is the intercept, the second is the coefficient for the linear term, and the third
#'        is the coefficient for the quadratic term.
#'
#' @return A numeric vector or scalar representing the computed values of the quadratic function for the input `x`.
#' @export
#'
#' @examples
#'
#' quadratic(x = 1:10, coefficients = c(2, 3, -1))
#'
quadratic = function(x, coefficients) {
  coefficients[1] + coefficients[2] * x + coefficients[3] * x ^ 2
}
