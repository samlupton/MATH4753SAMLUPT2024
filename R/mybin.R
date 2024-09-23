#' My Bin
#'
#' @param x Number of successes
#' @return a vector of probability
#' @export
#' @importFrom stats dbinom
#'
#' @examples
#' mybin(5)  # Example usage
mybin <- function(x) {
  dbinom(x, size = 10, prob = 0.5)
}
