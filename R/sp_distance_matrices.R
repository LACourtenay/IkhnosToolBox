
#' Comparison of nearest neighbour distance matrices.
#'
#' @description The present function is used to calculate the nearest neighbour
#' distance within and between two samples.
#'
#' @param spatial_object_1 A pp3 object containing a 3-dimensional point pattern.
#' @param spatial_object_2 A pp3 object containing a second 3-dimensional point
#' pattern.
#' @param name_1 A string to define the title of the plot that corresponds to
#' \code{spatial_object_1}.
#' @param name_2 A string to define the title of the plot that corresponds to
#' \code{spatial_object_2}.
#' @param create_external_plot A boolean TRUE or FALSE (default = TRUE) option
#' to create two popup windows with the plots of the distances between the marks
#' within and between each sample.
#'
#' @return If create_external_plot = TRUE, then the function returns two popup
#' windows with:
#' 1. two plots for the calculated nearest neighbour distances between marks
#' within each sample
#' 2. one plot for the calculated nearest neighbour distances between marks
#' between the two samples
#'
#' @seealso \code{\link{sp_distance_matrix}}.
#'
#' @examples
#' data(femur_right_circular1) #COMPROBAR ESTO
#' data(femur_right_linear1) #COMPROBAR ESTO
#' example_data1 <- load_marks(femur_right_circular1, mark_type = "circular") #ESTO DA ERROR POR NO CARGAR EL RDA
#' example_data2 <- load_marks(femur_right_linear1, mark_type = "linear") #ESTO DA ERROR POR NO CARGAR EL RDA
#' example_sp_object1 <- extract_spatial_data(example_data1, "circular")
#' example_sp_object2 <- extract_spatial_data(example_data2, "linear")
#' example_distance_matrices <- sp_distance_matrices(example_sp_object1, example_sp_object2, name_1 = "circular", name_2 = "linear")

sp_distance_matrices <- function(spatial_object_1, spatial_object_2,
                              name_1, name_2,
                              create_external_plot = TRUE) {


  `%!in%` = Negate(`%in%`)

  if("pp3" %!in% class(spatial_object_1) |
     "pp3" %!in% class(spatial_object_2)) {
    return(stop("Invalid spatial objects"))
  }

  name_1 <- as.character(name_1)
  name_2 <- as.character(name_2)

  pd_1 <- spatstat.geom::nndist(spatial_object_1)
  pd_2 <- spatstat.geom::nndist(spatial_object_2)

  if(create_external_plot == TRUE) {
    X11(); par(mfrow = c(1,2))
  } else {
    par(mfrow = c(1,2))
  }

  plot(pd_1, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
       main = paste("Distance between ", name_1, sep = ""))
  plot(pd_2, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
       main = paste("Distance between ", name_2, sep = ""))
  par(mfrow = c(1,1))

  if(create_external_plot == TRUE) {
    X11(); par(mfrow = c(1,1))
  } else {
    par(mfrow = c(1,1))
  }

  cd <- spatstat.geom::nndist(
    spatstat.geom::crossdist(spatial_object_1, spatial_object_2)
  )

  plot(cd, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
       main = paste("Distance Matrix between ", name_1, " and ", name_2, sep = ""))

}
