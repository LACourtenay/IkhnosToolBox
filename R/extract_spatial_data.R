
#' Prepare data for spatial analysis.
#'
#' @description The present function is used to prepare data for further spatial
#' analysis.
#'
#' @param input_data A data frame containing spatial coordinates.
#' @param mark_type A string specifying whether marks are "circular" or "linear".
#' @param print_summary A boolean TRUE or FALSE option (default = TRUE) to print
#' a summary of the data included in the spatial object.
#' @param plot_results A boolean TRUE or FALSE option (default = FALSE) to plot
#' the spatial information.
#' @param create_external_plot A boolean TRUE or FALSE (default = TRUE) option
#' to create a popup window with the plot for mark frequency for all the samples.
#'
#' @return A spatial object for statistical analyses.
#'
#' @seealso \code{\link{box3}}, \code{\link{pp3}}.
#'
#' @author Lloyd A.courtenay
#'
#' @examples
#' data("femur_right_circular1")
#' example_data <- load_marks(femur_right_circular1, mark_type = "circular")
#' example_sp_object <- extract_spatial_data(
#'   example_data, "circular"
#' )
#' @export

extract_spatial_data <- function(input_data,
                                 mark_type = "circular",
                                 print_summary = TRUE,
                                 plot_results = FALSE,
                                 create_external_plot = FALSE) {

  if(missing(input_data)) {
    stop("Warning no input data")
  }

  if (mark_type != "circular" & mark_type != "linear") {
    stop("Please select between circular or linear mark types")
  }

  if (!is.logical(print_summary)) {
    stop("print_summary plot must be either TRUE or FALSE")
  }

  if (!is.logical(plot_results)) {
    stop("plot_results must be either TRUE or FALSE")
  }

  if (!is.logical(create_external_plot)) {
    stop("create_external_plot must be either TRUE or FALSE")
  }

  if (mark_type == "circular") {

    # in reality... will the columns always be named x1, y1 and z1?

    xyz <- input_data[,c("x1", "y1", "z1")]
    colnames(xyz) <- c("x", "y", "z")

    xlim <- c(min(xyz$x), max(xyz$x))
    ylim <- c(min(xyz$y), max(xyz$y))
    zlim <- c(min(xyz$z), max(xyz$z))

    box <- spatstat.geom::box3(xlim, ylim, zlim, unitname = "mm")
    spatial_data <- spatstat.geom::pp3(xyz$x, xyz$y, xyz$z, box) # Create 3d spatial object

  } else {
    #xyz <- as_tibble(data) %>%
    #  select(x = x3, y = y3, z = z3)

    xyz <- input_data[,c("x3", "y3", "z3")]
    colnames(xyz) <- c("x", "y", "z")

    xlim <- c(min(xyz$x), max(xyz$x))
    ylim <- c(min(xyz$y), max(xyz$y))
    zlim <- c(min(xyz$z), max(xyz$z))

    box <- spatstat.geom::box3(xlim, ylim, zlim, unitname = "mm")
    spatial_data <- spatstat.geom::pp3(xyz$x, xyz$y, xyz$z, box) # Create 3d spatial object

  }

  if (print_summary == TRUE) {
    print(summary(spatial_data))
  }

  if (plot_results == TRUE) {
    if (create_external_plot == TRUE) {
      dev.new(); plot(spatial_data, main = "",
                  pch = 1, bg = "white", cex = 1.2)
    } else {
      plot(spatial_data, main = "",
           pch = 1, bg = "white", cex = 1.2)
    }

  }

  return(spatial_data) # check whether the box is necessary

}
