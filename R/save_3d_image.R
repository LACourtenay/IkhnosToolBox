
#' Saves 3D visualization.
#'
#' Function to save visualization of data.
#'
#' @param file_name a string specifying the file name where the .png image will be saved.
#'
#' @return Saves the visualized 3D window as a .png file.
#'
#' @seealso \code{\link{load_bone}}, \code{\link{save_3d_image}}.
#'
#' @examples
#' data(femur_left)
#' load_bone(femur_left)
#' load_marks("circular", plot = TRUE, colour_value = "black")
#' load_marks("linear", plot = TRUE, colour_value = "red")
#' save_3d_image("my_first_plot")

save_3d_image <- function(file_name) {

  if(rgl::rgl.cur() == 0) {
    return(warning("No visualisation device is currently active or open."))
  }

  if (!is.character(file_name)) {
    return(warning("Please insert a valid file name to save the image"))
  }

  suff <- substr(file_name, nchar(file_name)-4+1, nchar(file_name))

  if(suff != ".png") {
    file_name <- paste(file_name, ".png", sep = "")
  }

  # need to find a way of checking that an rgl object already exists

  rgl::rgl.snapshot(filename = file_name)

}
