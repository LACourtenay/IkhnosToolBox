
#' Save 3D visualisation.
#'
#' @description Function to save visualisation of data.
#'
#' @param file_name A string specifying the file name where the *.png
#' image will be saved.
#'
#' @return Saves the visualised 3D window as a *.png file.
#'
#' @seealso \code{\link{load_bone}}, \code{\link{save_3d_image}},
#' \code{\link{rgl.snapshot}}.
#'
#' @examples
#' data("right_femur")
#' data("femur_right_circular1")
#' data("femur_right_linear1")
#' load_bone(right_femur)
#' load_marks(femur_right_circular1, mark_type = "circular", plot = TRUE, colour_value = "black")
#' load_marks(femur_right_linear1, mark_type = "linear", plot = TRUE, colour_value = "red")
#' save_3d_image("my_first_plot")

save_3d_image <- function(file_name) {

  if(rgl::rgl.cur() == 0) {
    stop("No visualisation device is currently active or open.")
  }

  if (!is.character(file_name)) {
    stop("Please insert a valid file name to save the image")
  }

  suff <- substr(file_name, nchar(file_name)-4+1, nchar(file_name))

  if(suff != ".png") {
    file_name <- paste(file_name, ".png", sep = "")
  }

  # need to find a way of checking that an rgl object already exists

  rgl::rgl.snapshot(filename = file_name)

}
