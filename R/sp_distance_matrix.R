
#' Nearest Neighbour distance matrix.
#'
#' @description The present function is used to calculate the distances between
#' the marks in the sample.
#'
#' @param spatial_object A pp3 object containing the 3D point pattern
#' @param name A string to define the title of the plot.
#' @param create_external_plot A boolean TRUE or FALSE option (default = FALSE)
#' to create a popup window with the plot for the distances between the marks.
#'
#' @return Plot for the visualisation of the calculated nearest neighbour distances between
#' marks within the sample.
#'
#' @seealso \code{\link{sp_distance_matrices}}, \code{\link{nndist}}
#'
#' @section Bibliography:
#' A. Baddeley, E. Rubak and R.Turner. Spatial Point Patterns: Methodology and
#' Applications with R. Chapman and Hall/CRC Press, 2015.
#'
#' @examples
#' data("femur_right_circular1")
#' example_data <- load_marks(femur_right_circular1, mark_type = "circular")
#' example_sp_object <- extract_spatial_data(example_data, "circular")
#' example_distance_matrix <- sp_distance_matrix(
#'   example_sp_object, name = "marks"
#' )
#' @export


sp_distance_matrix <- function(spatial_object, name = "marks",
                               create_external_plot = FALSE) {

  `%!in%` = Negate(`%in%`)

  if(missing(spatial_object)) {
    stop("No spatial object has been defined.")
  }

  if("pp3" %!in% class(spatial_object)) {
    stop("Invalid spatial object")
  }

  if (!is.logical(create_external_plot)) {
    stop("create_external_plot must be either TRUE or FALSE")
  }

  name <- as.character(name)

  pd_1 <- spatstat.geom::nndist(spatial_object)

  if(create_external_plot == TRUE) {
    dev.new(); plot(pd_1, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
                main = paste("Distance between ", name, sep = ""))
  } else {
    plot(pd_1, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
         main = paste("Distance between ", name, sep = ""))
  }

}
