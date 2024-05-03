#' Normal Curve Function
#'
#' @param mu Mean of normal distribution.
#' @param sigma Standard Deviation of normal distribution.
#' @param a Limit for probability calculation, which will be <= a.
#'
#' @return A list containing mean, standard deviation, and the probability area.
#' @export
#'
#' @examples
#' myncurve(5, 10, 3)
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
myncurve <- function(mu, sigma, a) {
  x <- seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 1000)  # Define x explicitly

  graphics::curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma), main = paste("Y ~ N(", mu, ",", sigma, "), P(Y <= ", a, ")", sep = ""))

  x_curve <- seq(mu - 3 * sigma, a, length.out = 1000)
  y_curve <- dnorm(x_curve, mean = mu, sd = sigma)

  graphics::polygon(c(mu - 3 * sigma, x_curve, a), c(0, y_curve, 0), col = 'pink')

  area <- pnorm(a, mean = mu, sd = sigma)
  area <- round(area, 4)

  list(mu = mu, sigma = sigma, area = area)
}

