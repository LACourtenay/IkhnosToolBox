
#' Load bone for visualisation.
#'
#' @description The present function creates a 3D popup window visualising
#' the bone under study. This can then be combined with functions such as
#' \code{load_marks} to visualise marks on the 3D model, and
#' \code{save_3d_image}nto save the window to a *.png file.
#'
#' @param bone_data An IkhnosToolBox data object (e.g. data(left_femur)) containing
#' the 3d model of the bone being studied.
#'
#' @return A 3D popup window with the selected bone mesh points.
#'
#' @seealso \code{\link{load_marks}}, \code{\link{save_3d_image}},
#' \code{\link{plot3d}}, \code{\link{aspect3d}}
#'
#' @examples
#' data("right_femur")
#' data("femur_right_circular1")
#' data("femur_right_linear1")
#' load_bone(right_femur)
#' load_marks(femur_right_circular1, mark_type = "circular", plot = TRUE, colour_value = "black")
#' load_marks(femur_right_linear1, mark_type = "linear", plot = TRUE, colour_value = "red")
#' save_3d_image("my_first_plot")
#' @export

load_bone <- function(bone_data) {

  #`%!in%` = Negate(`%in%`)

  if(missing(bone_data)) {
    stop("Bone is missing - please select bone to study")
  }

  if(dim(bone_data)[2] < 3 | dim(bone_data)[2] > 3 ) {
    stop("Invalid bone. Please insert one of the bones provided by the data() function.")
  }

  rgl::open3d()
  rgl::plot3d(bone_data[,1], bone_data[,2], bone_data[,3],
              col="lightgray",
              xlim=c(-20,350), ylim=c(-20,100), zlim=c(-20,100),
              xlab="", ylab="", zlab="")
  rgl::aspect3d("iso")

}
