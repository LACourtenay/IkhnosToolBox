
add_time_series <- function(
  h = NULL, f = NULL, r = NULL, t = NULL, mt = NULL, mc = NULL,
  colour = "red"
) {

  active_window <- try(points(1,2), silent = TRUE)

  if(class(active_window) == "try-error") {
    return(
      warning(
        paste0("\n\nThis function is used to add a time series plot to an already existing plot.\n",
               "No plot has yet been called using the create_time_series function.\n")
      )
    )
  }

  ts <- extract_ts_data(h = h, f = f, r = r, t = t, mt = mt, mc = mc)

  lines(ts$total_line, ts$percentages, type = "l",
        ylim = ts$y_lim, lwd = 1, col = colour, ylab = "Percentage", xlab = "")
  points(ts$total_line, ts$percentages, pch = 19, col = colour)

  #time_series <- cbind(ts$total_line, ts$series)
  time_series <- data.frame(sequence = ts$total_line, frequencies = ts$series)

  return(time_series)

}
