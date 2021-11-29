
#' Nearest Neighbour distance matrix.
#'
#' @description The present function is used to calculate the distances between
#' the marks in the sample.
#'
#' @param spatial_object A pp3 object containing the 3-dimensional point pattern
#' @param name_1 A string to define the title of the plot.
#' @param create_external_plot A boolean TRUE or FALSE option (default = TRUE)
#' to create a popup window with the plot of the distances between the marks.
#'
#' @return If create_external_plot = TRUE, then the function returns an popup
#' window with the plot for the calculated nearest neighbour distances between
#' marks within the sample.
#'
#' @seealso \code{\link{sp_distance_matrices}}
#'
#' @examples
#' data(femur_right_circular1) #COMPROBAR ESTO
#' example_data <- load_marks(femur_right_circular1, mark_type = "circular") #ESTO DA ERROR POR NO CARGAR EL RDA
#' example_sp_object <- extract_spatial_data(example_data, "circular")
#' example_distance_matrix <- sp_distance_matrix(example_sp_object, name = "marks")


sp_distance_matrix <- function(spatial_object, name_1 = "marks",
                               create_external_plot = TRUE) {

  `%!in%` = Negate(`%in%`)

  if("pp3" %!in% class(spatial_object)) {
    return(stop("Invalid spatial object"))
  }

  name_1 <- as.character(name_1)

  pd_1 <- spatstat.geom::nndist(spatial_object)

  if(create_external_plot == TRUE) {
    X11(); plot(pd_1, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
                main = paste("Distance between ", name_1, sep = ""))
  } else {
    plot(pd_1, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
         main = paste("Distance between ", name_1, sep = ""))
  }

}
