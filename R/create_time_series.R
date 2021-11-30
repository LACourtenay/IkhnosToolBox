
#' Creation of multiple time series plot.
#'
#' @description The present function is used to generate a plot including the
#' mark frequencies on several bone elements and provide the basis for the
#' addition of further samples.
#'
#' @param h A data frame containing the spatial coordinates representing the
#' marks in the first humerus sample.
#' @param f A data frame containing the spatial coordinates representing the
#' marks in the first femur sample.
#' @param r A data frame containing the spatial coordinates representing the
#' marks in the first radius sample.
#' @param t A data frame containing the spatial coordinates representing the
#' marks in the first tibia sample.
#' @param mt A data frame containing the spatial coordinates representing the
#' marks in the first metatarsus sample.
#' @param mc A data frame containing the spatial coordinates representing the
#' marks in the first metacarpus sample.
#' @param colour A lower case string specifying the colour of points and lines
#' in the plot to represent the first set of samples.
#' @param create_external_plot A boolean TRUE or FALSE (default = TRUE) option
#' to create a popup window with the plot for mark frequency for all the samples.
#'
#' @return A data frame including the sequence and frequencies of all the samples.
#' If create_external_plot = TRUE, then the function returns a popup
#' window with a single plot showing the mark frequencies for all the elements.
#'
#' @seealso \code{\link{add_time_series}}, \code{\link{load_marks}}.
#'
#' @author Lloyd A. Courtenay
#'
#'
#' @examples
#' data("humerus_right_circular1")
#' data("femur_right_circular1")
#' data("radius_right_circular1")
#' data("tibia_right_circular1")
#' h1 <- load_marks(femur_right_circular1, mark_type = "circular", plot = FALSE)
#' f1 <- load_marks(femur_right_circular1, mark_type = "circular", plot = FALSE)
#' r1 <- load_marks(femur_right_circular1, mark_type = "circular", plot = FALSE)
#' t1 <- load_marks(femur_right_circular1, mark_type = "circular", plot = FALSE)
#' time_series_1 <- create_time_series(h = h1, f = f1, r = r1, t = t1)
#' @export

create_time_series <- function(
  h = NULL, f = NULL, r = NULL, t = NULL, mt = NULL, mc = NULL,
  colour = "black",
  create_external_plot = TRUE
) {

  if (colour != "black") {
    colour_bool <- check_colours(colour)
    if (FALSE %in% colour_bool) {
      stop("Invalid colour provided for argument 'colour'")
    }
  }

  if (!is.logical(create_external_plot)) {
    stop("create_external_plot must be either TRUE or FALSE")
  }

  ts <- extract_ts_data(h = h, f = f, r = r, t = t, mt = mt, mc = mc)

  # figure

  if(create_external_plot == TRUE) {
    X11(width = 15, height = 7.5); par(mar = c(5.1, 5, 4.1, 2.))
  } else {
    par(mar = c(5.1, 5, 4.1, 2.))
  }

  plot(ts$total_line, ts$percentages, type = "l",
       ylim = ts$y_lim, lwd = 1, col = colour, ylab = "", xlab = "")
  mtext(side = 2, line = 3, "Percentage", cex = 1.25, font = 2)
  points(ts$total_line, ts$percentages, pch = 19, col = colour)
  for(i in 1:length(ts$line_nums)) {
    if (ts$line_nums[i] == 1) {
      abline(v = (ts$total_line[i] - 0.5), col = "blue")
    }
  }; abline(v = (max(ts$total_line) + 0.5), col = "blue")
  text(x = (0 + 5), y = (ts$y_lim[2] - 0.0005), ts$labels[1], font = 2, cex = 1.5)

  iteration = 1

  while (iteration < (length(ts$labels) - 2)) {

    if (ts$labels[iteration] != ts$labels[iteration + 1]) {
      text(x = (ts$total_line[iteration] + 5), y = (ts$y_lim[2] - 0.0005),
           ts$labels[iteration + 1], font = 2,
           cex = 1.5)
    }
    iteration = iteration + 1
  }

  par(mar = c(5.1, 4.1, 4.1, 2.1))

  # return histogram

  time_series <- data.frame(Sequence = ts$total_line, Frequencies = ts$series)

  return(time_series)

} # need to debug error in while loop
