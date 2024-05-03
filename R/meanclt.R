#' Central Limit Theorem CLT Function for Sample Means
#'
#' This function generates random numbers from a uniform distribution,
#' calculates the mean of each iteration, and plots a histogram of the sample means.
#'
#' @param n Number of random variables to generate per iter.
#' @param iter Number of iterations to perform.
#' @return A vector containing sample means.
#' @examples
#' meanclt(n = 10, iter = 10000)
#' @importFrom stats runif
#' @importFrom graphics hist
#' @export
meanclt <- function(n, iter) {
  y <- stats::runif(n * iter, 0, 5)  # Generate random numbers from a uniform distribution
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)  # Puts data into matrix
  means <- apply(data, 2, mean)  # Calculate the mean per iteration
  graphics::hist(means)  # histogram of sample means
  return(means)  # vector of sample means
}


