
#' Add time series to a multiple time series plot.
#'
#' @description The present function is used to add a time series plot to an
#' already existing time series plot.
#'
#' @param h A data frame containing the spatial coordinates representing the
#' marks in an additional humerus sample.
#' @param f A data frame containing the spatial coordinates representing the
#' marks in an additional femur sample.
#' @param r A data frame containing the spatial coordinates representing the
#' marks in an additional radius sample.
#' @param t A data frame containing the spatial coordinates representing the
#' marks in an additional tibia sample.
#' @param mt A data frame containing the spatial coordinates representing the
#' marks in an additional metatarsus sample.
#' @param mc A data frame containing the spatial coordinates representing the
#' marks in an additional metacarpus sample.
#' @param colour A lower case string specifying the colour of points and lines
#' in the plot to represent the additional set of samples.
#'
#' @return A data frame including the sequence and frequencies of all the
#' additional samples. Mark frequencies are added to the plot in the popup
#' window previously created with the \code{create_time_series} function.
#'
#' @seealso \code{\link{create_time_series}}, \code{\link{load_marks}}.
#'
#' @author Lloyd A. Courtenay
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
#'
#' data("humerus_right_circular2")
#' data("femur_right_circular2")
#' data("radius_right_circular2")
#' data("tibia_right_circular2")
#' h1 <- load_marks(femur_right_circular2, mark_type = "circular", plot = FALSE)
#' f1 <- load_marks(femur_right_circular2, mark_type = "circular", plot = FALSE)
#' r1 <- load_marks(femur_right_circular2, mark_type = "circular", plot = FALSE)
#' t1 <- load_marks(femur_right_circular2, mark_type = "circular", plot = FALSE)
#' time_series_2 <- create_time_series(h = h2, f = f2, r = r2, t = t2, colour = "red")
#'
#' data("humerus_right_circular3")
#' data("femur_right_circular3")
#' data("radius_right_circular3")
#' data("tibia_right_circular3")
#' h1 <- load_marks(femur_right_circular3, mark_type = "circular", plot = FALSE)
#' f1 <- load_marks(femur_right_circular3, mark_type = "circular", plot = FALSE)
#' r1 <- load_marks(femur_right_circular3, mark_type = "circular", plot = FALSE)
#' t1 <- load_marks(femur_right_circular3, mark_type = "circular", plot = FALSE)
#' time_series_3 <- create_time_series(h = h3, f = f3, r = r3, t = t3, colour = "blue")


add_time_series <- function(
  h = NULL, f = NULL, r = NULL, t = NULL, mt = NULL, mc = NULL,
  colour = "red"
) {

  if (colour != "red") {
    colour_bool <- check_colours(colour)
    if (FALSE %in% colour_bool) {
      stop("Invalid colour provided for argument 'colour'")
    }
  }

  active_window <- try(points(1,2), silent = TRUE)

  if(class(active_window) == "try-error") {
    stop(
      paste0("\n\nThis function is used to add a time series plot to an already existing plot.\n",
              "No plot has yet been called using the create_time_series function.\n")
    )
  }

  ts <- extract_ts_data(h = h, f = f, r = r, t = t, mt = mt, mc = mc)

  lines(ts$total_line, ts$percentages, type = "l",
        ylim = ts$y_lim, lwd = 1, col = colour, ylab = "Percentage", xlab = "")
  points(ts$total_line, ts$percentages, pch = 19, col = colour)

  #time_series <- cbind(ts$total_line, ts$series)
  time_series <- data.frame(Sequence = ts$total_line, Frequencies = ts$series)

  return(time_series)

}
