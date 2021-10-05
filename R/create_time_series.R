

create_time_series <- function(
  h = NULL, f = NULL, r = NULL, t = NULL, mt = NULL, mc = NULL,
  colour = "black",
  create_external_plot = TRUE
) {

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
