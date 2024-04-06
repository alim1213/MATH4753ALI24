#' Bootstrap Confidence Interval
#'
#' Compute bootstrap confidence interval
#'
#' @param iter Iterations for bootstrap sampling, 10000 by default.
#' @param x Vector of data.
#' @param fun The function to compute; default is "mean".
#' @param alpha Confidence level; default is 0.05.
#' @param cx Expansion factor for confidence interval text; default is 1.5.
#' @param ... Additional arguments.
#'
#' @return A list containing information about the function.
#'
#' @details Computes a bootstrap confidence interval for a given statistic applied to the data.
#'
#' @examples
#' x <- rnorm(100)
#'
#' @importFrom stats quantile
#' @importFrom graphics abline segments text
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)  # Sample size

  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = iter, ncol = n, byrow = TRUE)  # Corrected arguments for matrix
  xstat <- apply(rs.mat, 1, fun)  # Apply the function along rows
  ci <- stats::quantile(xstat, c(alpha/2, 1-alpha/2))  # Compute quantiles

  # Plotting
  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha=", alpha, " iter=", iter, sep = ""),
               ...)  # Use '...' for additional arguments

  pte <- fun(x)  # Compute the point estimate
  graphics::abline(v = pte, lwd = 3, col = "black")  # Add vertical line for point estimate
  graphics::segments(ci[1], 0, ci[2], 0, lwd = 4)  # Confidence interval segment
  graphics::text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "red", cex = cx)  # Text for lower bound
  graphics::text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "red", cex = cx)  # Text for upper bound

  graphics::text(pte, max(para$density)/2, round(pte, 2), cex = cx)  # Text for point estimate

  return(list(ci = ci, fun = fun, x = x))  # Return results
}

