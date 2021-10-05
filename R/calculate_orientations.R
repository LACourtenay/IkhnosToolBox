
calculate_orientations <- function(linear_data, bone,
                                   create_external_plot = TRUE) {

  # maybe to ensure that data is linear, a linear_data class should be created...?

  `%!in%` = Negate(`%in%`)

  if (missing(bone)) {
    return(warning(
      paste0(
        "\nThe bone is missing!\nThe user must load the bone where marks are located",
        " using the data() function, so as to correctly calculate mark orientations"
      )
    ))
  }

  if(missing(linear_data)) {
    return(warning(
      "No data has been introduced!"
    ))
  }

  if ("x2" %!in% colnames(linear_data)) {
    return(warning(
      "Data provided are not linear marks"
    ))
  }

  example_bone <- bone

  start_mark <- data.frame(x = linear_data$x1,
                           y = linear_data$y1,
                           z = linear_data$z1,
                           Sample = "mark",
                           position = "start")
  end_mark <- data.frame(x = linear_data$x2,
                         y = linear_data$y2,
                         z = linear_data$z2,
                         Sample = "mark",
                         position = "end")
  bone <- data.frame(x = example_bone[,1],
                     y = example_bone[,2],
                     z = example_bone[,3],
                     Sample = "bone",
                     position = "none")

  threed_model <- rbind(start_mark, end_mark, bone)
  threed_model$Sample <- as.factor(threed_model$Sample)
  threed_model$position <- as.factor(threed_model$position)

  pc_bone <- prcomp(threed_model[,1:3])

  adjusted_threed <- as.data.frame(pc_bone$x)
  adjusted_threed$Sample <- threed_model$Sample
  adjusted_threed$Position <- threed_model$position

  bone_outline <- cbind(adjusted_threed[threed_model$Sample == "bone",1],
                        adjusted_threed[threed_model$Sample == "bone",2])
  bone_outline <- bone_outline[!duplicated(bone_outline),]

  bone_conv <- alphahull::ashape(bone_outline,
                                 alpha = 2.5)

  if (create_external_plot == TRUE) {
    X11(width = 10, height = 10)
  } else {
    par(mfrow = c(1,2))
  }

  plot(bone_conv$edges[,4], bone_conv$edges[,3], pch = 19, asp = 1, cex = 0.1)
  segments(adjusted_threed[adjusted_threed$Position == "start", 2],
           adjusted_threed[adjusted_threed$Position == "start", 1],
           adjusted_threed[adjusted_threed$Position == "end", 2],
           adjusted_threed[adjusted_threed$Position == "end", 1], col = "red",
           lwd = 2)

  mark_start <- adjusted_threed[adjusted_threed$Position == "start",]
  mark_end <- adjusted_threed[adjusted_threed$Position == "end",]
  slopes <- c(); for (i in 1:nrow(mark_start)) {
    numerator <- mark_start[i,1] - mark_end[i,1]
    denominator <- mark_start[i,2] - mark_end[i,2]
    slopes <- c(slopes, (numerator / denominator))
  }; slopes

  angles <- atan(slopes)

  circ_obj <- circular::circular(angles,
                                 type = "directions", units = "radians")

  circ_2 <- circular::circular(angles - pi,
                               type = "directions", units = "radians")

  circ <- c(circ_obj, circ_2)

  if (create_external_plot == TRUE) {
    X11(width = 10, height = 10)
  }

  circular::rose.diag(circ, bins = 16, col = "darkgrey", cex = 1.5, prop = 1.3)
  suppressMessages(
    suppressWarnings(
      circular::arrows.circular(calc_mean(
        circular::circular(circ_obj, type = "directions", units = "radians")
      ), lwd = 2, shrink = 0.9)
    )
  )
  suppressMessages(
    suppressWarnings(
      circular::arrows.circular(calc_mean(
        circular::circular(circ_2, type = "directions", units = "radians")
      ), col = "red", lwd = 2, shrink = 0.9)
    )
  )
  text(0.75, 1, "Proximal Epiphysis", font = 2)
  text(0, 1.1, "Diaphysis", font = 2)
  text(-0.75, 1, "Distal Epiphysis", font = 2)

  par(mfrow = c(1,1))

  return(circ_obj)

}
