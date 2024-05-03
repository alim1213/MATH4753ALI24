#' Rainbow Barplot
#'
#' @param data_vector data vector
#'
#' @return bar plot
#' @export
#'
#' @examples
#' data <- c(10, 20, 30, 40, 50)
#' barplot_rainbow(data)
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
barplot_rainbow = function(data_vector) {
  # Generate colors using the rainbow function (from grDevices package)
  colors <- grDevices::rainbow(length(data_vector))  # Import from grDevices

  # Create the bar plot (from graphics package)
  graphics::barplot(data_vector, col = colors, main = "Bar Plot with Rainbow Colors", xlab = "Data Points", ylab = "Values")  # Import from graphics
}

