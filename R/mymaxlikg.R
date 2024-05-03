#' Maximum Likelihood Estimation
#'
#' Finds the parameter value that maximizes the likelihood function.
#'
#' @param lfun The likelihood function, logbin2 by default.
#' @param theta Parameter values.
#'
#' @return Param value that maximizes the likelihood function.
#'
#' @examples
#' logbin2 <- function(theta) {
#'     log(dbinom(3, prob = theta, size = 6)) +
#'     log(dbinom(5, prob = theta, size = 10))
#' }
#' mymaxlikg(theta = seq(0, 1, length = 10000), lfun = logbin2)
#' @export
#' @importFrom graphics axis abline plot
mymaxlikg <- function(lfun = "logbin2", theta) {
  nth <- length(theta)  # Number of values in theta
  thmat <- matrix(theta, nrow = nth, ncol = 1, byrow = TRUE)  # Matrix of theta
  z <- apply(thmat, 1, lfun)  # Log-likelihood values
  zmax <- max(which(z == max(z)))  # Index of maximum likelihood
  graphics::plot(theta, exp(z), type = "l")  # Plot likelihood
  graphics::abline(v = theta[zmax], col = "blue")  # Vertical line at max likelihood
  graphics::axis(3, theta[zmax], round(theta[zmax], 4))  # Tick on third axis
  theta[zmax]  # Return theta at max likelihood
}

