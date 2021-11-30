
#' Calculation of orientation of linear marks.
#'
#' @description Function to conduct orientation analyses of scores and/or cut
#' marks.
#'
#' @param linear_data A string containing the path of the Ikhnos product file
#' containing linear data (e.g., scores or cut marks). If no file name is
#' specified, then a popup window will appear allowing the user to locate the
#' file in their system.
#' @param bone An IkhnosToolBox data object (e.g., data(left_femur)) containing
#' the 3d model of the bone being studied.
#' @param create_external_plot A boolean TRUE or FALSE (default = TRUE) option
#' to create two popup windows; one for the visualisation of the location of the
#' linear marks on the specified bone; and a second one with the rose diagram
#' based on the orientations of the marks.
#'
#' @return A \code{circular} object containing the angles calculated between
#' the bone long axis and each linear mark. If create_external_plot = TRUE, then
#' two popup windows are created for the rose diagram and the visualisation
#' of the linear marks on the specified bone.
#'
#' @seealso \code{\link{load_marks}}, \code{\link{ashape}}, \code{\link{circular}},
#' \code{\link{arrows.circular}}
#'
#' @author Lloyd A. Courtenay
#'
#' @section Bibliography:
#' Edelsbrunner, H., Kirkpatrick, D.G. and Seidel, R. (1983). On the shape of a
#' set of points in the plane. IEEE Transactions on Information Theory, 29(4), pp.551-559.
#'
#' Jammalamadaka, S. Rao and SenGupta, A. (2001). Topics in Circular Statistics,
#' World Scientific Press, Singapore.
#'
#' @examples
#' data("right_femur")
#' data("femur_right_linear1")
#' data("femur_right_linear2")
#' example_circ_1 <- load_marks(femur_right_linear1, mark_type = "linear")
#' example_circ_2 <- load_marks(femur_right_linear2, mark_type = "linear")
#' example_calculate_orientations_1 <- calculate_orientations(example_circ_1, right_femur)
#' example_calculate_orientations_2 <- calculate_orientations(example_circ_2, right_femur)

calculate_orientations <- function(linear_data, bone,
                                   create_external_plot = TRUE) {

  # maybe to ensure that data is linear, a linear_data class should be created...?

  `%!in%` = Negate(`%in%`)

  if (missing(bone)) {
    stop(
      paste0(
        "\nThe bone is missing!\nThe user must load the bone where marks are located",
        " using the data() function, so as to correctly calculate mark orientations"
      )
    )
  }

  if(missing(linear_data)) {
    stop(
      "No data has been introduced!"
    )
  }

  if ("x2" %!in% colnames(linear_data)) {
    stop(
      "Data provided are not linear marks"
    )
  }

  if (!is.logical(create_external_plot)) {
    stop("create_external_plot must be either TRUE or FALSE")
  }

  example_bone <- bone

  start_mark <- data.frame(
    x = linear_data$x1,
    y = linear_data$y1,
    z = linear_data$z1#,
    #Sample = "mark",
    #position = "start"
  )
  end_mark <- data.frame(
    x = linear_data$x2,
    y = linear_data$y2,
    z = linear_data$z2#,
    #Sample = "mark",
    #position = "end"
  )
  bone <- data.frame(
    x = example_bone[,1],
    y = example_bone[,2],
    z = example_bone[,3]#,
    #Sample = "bone",
    #position = "none"
  )

  oriented_bone <- prcomp(bone, scale = FALSE)
  oriented_start <- as.data.frame(predict(oriented_bone, newdata = start_mark[,1:3]))
  oriented_end <- as.data.frame(predict(oriented_bone, newdata = end_mark[,1:3]))

  threed_model <- as.data.frame(oriented_bone$x)
  threed_model <- data.frame(threed_model, Sample = "bone", Position = "none")

  oriented_start <- data.frame(
    oriented_start, Sample = "mark", Position = "start"
  )
  oriented_end <- data.frame(
    oriented_end, Sample = "mark", Position = "end"
  )

  adjusted_threed <- rbind(threed_model, oriented_start, oriented_end)
  adjusted_threed$Sample <- as.factor(adjusted_threed$Sample)
  adjusted_threed$Position <- as.factor(adjusted_threed$Position)

  bone_outline <- cbind(adjusted_threed[adjusted_threed$Sample == "bone",1],
                        adjusted_threed[adjusted_threed$Sample == "bone",2])
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
