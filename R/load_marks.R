
#' Load marks from IKHNOS files.
#'
#' The present function allows the user to locate a file and load data derived
#' from Ikhnos for both visualization
#' and preparation for statistical applications. If the \code{load_bone}
#' function has already been used, then marks are visualized directly on the
#' surface of the bone that is currently in view. If no \code{load_bone} function
#' has been called, then the plot will contain only the 3D distribution of marks.
#' This can then be combined with functions such as \code{load_marks}
#' to visualize marks on the 3D model, and \code{save_3d_image} to
#' save the window to a *.png file.
#'
#' @param file_name A string containing the path of the Ikhnos product file.
#' If no file name is specified, then a popup window will appear allowing the user
#' to locat the file in their system.
#' @param mark_type A lower case string specifying whether the marks under study
#' are "circular" or "linear".
#' @param plot A TRUE or FALSE option defining whether to produce a 3D plot of the marks.
#' @param delim A string specifying how marks are delimited within the Ikhnos product file.
#' @param colour_value A lower case string specifying the colour of the marks in the case where
#' plot = TRUE.
#' @param mark_size The size of the points or lines produced in the 3D plot.
#'
#' @return Returns a \code{data.frame} object containing the data extracted from Ikhnos.
#' If plot = TRUE, then a 3D visualization of marks is also created.
#'
#' @seealso \code{\link{load_bone}}, \code{\link{save_3d_image}}.
#' @examples
#' data(femur_left)
#' load_bone(femur_left)
#' load_marks("circular", plot = TRUE, colour_value = "black")
#' load_marks("linear", plot = TRUE, colour_value = "red")
#' save_3d_image("my_first_plot")

load_marks <- function(file_name = NULL, mark_type = "circular",
                       delim = ",", plot = FALSE,
                       colour_value = "black", mark_size = 6) {

  # need to check that input dataframe is valid.

  `%!in%` = Negate(`%in%`)

  if (mark_type != "circular" & mark_type != "linear") {
    return(warning("Please select between circular or linear mark types"))
  }

  if (!is.logical(plot)) {
    return(warning("plot parameter must be equal to TRUE or FALSE"))
  }

  possible_delims <- c(",", ";", "\t", " ")

  if (delim %!in% possible_delims) {
    return(warning(cat("Invalid delimiter value. Please ensure files are seperated",
                       "by either commas, semi colons, tabs or spaces.",
                       sep = " ")))
  }

  if (colour_value != "black") {
    col_bool <- check_colours(colour_value)
    if (FALSE %in% col_bool) {
      return(warning("Invalid colour provided for the plot"))
    }
  }

  if (!is.numeric(mark_size) | mark_size < 0) {
    return(warning("point_size must be a positive value"))
  }

  if (!is.null(file_name)) {
    new_points <- read.table(file_name, header = T, sep = delim)
  } else {
    new_points <- read.table(file.choose(), header = T, sep = delim)
  }

  if (plot == TRUE) {
    if (mark_type == "circular") {
      new_points<-new_points[,3:5]
      rgl::points3d(new_points[,1], new_points[,2], new_points[,3],
                    size = mark_size, col = colour_value)
    } else {
      markers <- data.frame(startX = c(new_points$x1),
                            startY = c(new_points$y1),
                            startZ = c(new_points$z1),
                            endX = c(new_points$x2),
                            endY = c(new_points$y2),
                            endZ = c(new_points$z2))
      rgl::segments3d(x = as.vector(t(markers[,c(1,4)])),
                      y = as.vector(t(markers[,c(2,5)])),
                      z = as.vector(t(markers[,c(3,6)])),
                      colvar = TRUE, lwd = mark_size, col = colour_value,
                      xlab = "X", ylab = "Y", zlab = "Z")
    }
  }

  return(new_points)

}
