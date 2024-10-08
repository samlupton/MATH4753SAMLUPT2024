#' The N Tickets Function
#'
#' This function estimates the optimal number of tickets to sell for an event with limited capacity `N`, based on a given
#' probability of no overbooking `p` and a risk tolerance `gamma`. The function calculates both a discrete and a normal approximation
#' to find the optimal number of tickets to sell such that the probability of exceeding capacity is controlled.
#'
#' @param N Integer. The capacity of the event (the maximum number of people that can attend).
#' @param gamma Numeric. The risk tolerance level, representing the maximum acceptable probability of exceeding the capacity `N`.
#' @param p Numeric. The probability that a single person who bought a ticket will actually attend the event.
#'
#' @return A list with the following components:
#'   \item{nd}{Optimal number of tickets to sell using the discrete binomial distribution approximation.}
#'   \item{nc}{Optimal number of tickets to sell using the normal approximation.}
#'   \item{N}{The event capacity, as provided in the input.}
#'   \item{gamma}{The risk tolerance, as provided in the input.}
#'   \item{p}{The attendance probability, as provided in the input.}
#'
#' @export
#'
#' @examples
#' # Example usage:
#' # Calculate the optimal number of tickets to sell when the event capacity is 100,
#' # the risk tolerance is 0.05 (5% chance of overbooking), and the attendance probability is 0.9.
#' result <- ntickets(N=400,gamma = 0.02, p = 0.95)
#' print(result)
ntickets <- function(N, gamma, p) {
  # Discrete case: find nd by iterating
  candidate_n <- N
  discrete_found <- FALSE
  while (!discrete_found) {
    cumulative_prob <- sum(stats::dbinom(0:N, candidate_n, p))
    if (1 - cumulative_prob <= gamma) {
      nd <- candidate_n
      discrete_found <- TRUE
    } else {
      candidate_n <- candidate_n + 1
    }
  }

  # Continuous case: find nc using normal approximation
  expected_mean <- N * p
  expected_sd <- sqrt(N * p * (1 - p))
  nc <- stats::qnorm(1 - gamma, mean = expected_mean, sd = expected_sd)

  # Plotting and objective calculation for discrete case
  possible_ns <- seq(N, N + 20)
  objective_values <- numeric(length(possible_ns))
  for (i in seq_along(possible_ns)) {
    cumulative_prob <- sum(stats::dbinom(0:N, floor(possible_ns[i]), p))
    objective_values[i] <- 1 - cumulative_prob
  }
  diff_values <- abs(objective_values - gamma)
  optimal_n_discrete <- possible_ns[which.min(diff_values)]

  plot(possible_ns, objective_values, type = "b",
       main = paste("Objective vs n for optimal ticket sales\n",
                    "(", optimal_n_discrete, ") ", "gamma=", gamma, " N=", N, " Discrete", sep = ""),
       ylab = "Objective (1 - P(X <= N))", xlab = "n")
  graphics::abline(h = gamma, col = "red", lwd = 2)
  graphics::abline(v = optimal_n_discrete, col = "red", lwd = 2)

  # Plotting and objective calculation for continuous case
  objective_values_continuous <- numeric(length(possible_ns))
  for (i in seq_along(possible_ns)) {
    mean_val <- possible_ns[i] * p
    stddev_val <- sqrt(possible_ns[i] * p * (1 - p))
    prob_val <- stats::pnorm(N, mean = mean_val, sd = stddev_val)
    objective_values_continuous[i] <- 1 - prob_val
  }
  diff_values_continuous <- abs(objective_values_continuous - gamma)
  optimal_n_continuous <- possible_ns[which.min(diff_values_continuous)]

  plot(possible_ns, objective_values_continuous, type = "l",
       main = paste("Objective vs n for optimal ticket sales\n",
                    "(", optimal_n_continuous, ") ", "gamma=", gamma, " N=", N, " Continuous", sep = ""),
       ylab = "Objective", xlab = "n")
  graphics::abline(h = gamma, col = "blue", lwd = 2)
  graphics::abline(v = optimal_n_continuous, col = "blue", lwd = 2)

  # Return the results
  results <- list(nd = optimal_n_discrete, nc = optimal_n_continuous, N = N, gamma = gamma, p = p)

  return(results)
}


