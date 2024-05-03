#' Find the number of tickets to sell to avoid overbooking, with discrete and continuous distributions.
#'
#' This function calculates the number of tickets to be sold given N, gamma, and p using discrete and continuous distributions.
#'
#' @param N Number of seats on flight, 200 by default.
#' @param gamma Probability of overbooking 0.02 by default.
#' @param p Probability a passenger will show, 0.95 by default.
#'
#' @return A list containing: The number of tickets to be sold calculated using the discrete and continuous distribution, along with the parameters passed in the function call.
#'
#' @examples
#' ntickets(N = 200, gamma = 0.02, p = 0.95)
#' @export
#'
#' @importFrom stats pbinom optimize
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {
  # First get the size, which will be passed to pbinom and pnorm, for the discrete and continuous distributions.
  n <- seq(N, N * 1.1)

  # Now, get the values for the discrete and continuous distributions, along with their minimum index which will be used to
  objective_discrete <- 1 - gamma - stats::pbinom(q = N, size = n, prob = p)
  min_index_discrete <- which.min(abs(objective_discrete))

  # Continuous distribution
  objective_continuous <- 1 - gamma - stats::pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))
  objective_func <- function(n) {abs(1 - gamma - stats::pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p))))}

  # Obtaining the minimum index, used later for plots, and the resulting list
  # These are used for nd and nc.
  min_index_continuous <- stats::optimize(objective_func, interval = c(N, floor(N + N/10)))$minimum

  title <- "Objective vs. n to find optimal tickets sold\n"

  # Plot both distributions
  plot(n, objective_discrete, type = 'b', ylab = "Objective", col = "blue", pch = 16, main = paste(title, "(", n[min_index_discrete], ")", "gamma = ", gamma, "N = ", N, "discrete"))
  abline(h = 0, v = n[min_index_discrete], lwd = 2, col = "red")

  plot(n, objective_continuous, type = 'l', ylab = "Objective", col = "black",
       main = paste(title, "(", min_index_continuous, ")", "gamma = ", gamma, "N = ", N, "continuous"))

  abline(v = min_index_continuous, h = 0, col = "blue", lwd = 2)

  # Printing results
  result <- list(nd = n[min_index_discrete], nc = min_index_continuous, N = N, p = p, gamma = gamma)
  print(result)
}

