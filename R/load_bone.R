
#' Load bone for visualization.
#'
#' The present function creates a 3D popup window visualizing the bone under
#' study. This can then be combined with functions such as \code{load_marks}
#' to visualize marks on the 3D model, and \code{save_3d_image} to
#' save the window to a *.png file.
#'
#' @param bone An IkhnosToolBox data object (e.g. data(left_femur)) containing the 3d model of the bone being studied.
#'
#' @return A 3D popup window
#'
#' @seealso \code{\link{load_marks}}, \code{\link{save_3d_image}}.
#'
#' @examples
#' data(left_femur)
#' load_bone(left_femur)
#' load_marks(mark_type = "circular", plot = TRUE, colour_value = "black")
#' load_marks(mark_type = "linear", plot = TRUE, colour_value = "red")
#' save_3d_image("my_first_plot")

load_bone <- function(bone_data) {

  #`%!in%` = Negate(`%in%`)

  if(missing(bone_data)) {
    return(warning("Bone is missing - please select bone to study"))
  }

  if(dim(bone_data)[2] < 3 | dim(bone_data)[2] > 3 ) {
    return(warning("Invalid bone. Please insert one of the bones provided by the data() function."))
  }

  #if(missing(side)) {
  #  return(warning("Side is missing - please select laterality of bone"))
  #}

  #possible_bones <- c("femur","humerus","metacarpus","radius",
  #                    "tibia","metatarsus")

  #if (bone %!in% possible_bones) {
  #  return(warning("Invalid bone"))
  #}

  #possible_sides <- c("left", "right")

  #if (side %!in% possible_sides) {
  #  return(warning("Invalid side"))
  #}

  # need to find a way of connecting this function to the internal documentation of the library

  #a<-read.table(paste("data\\", side, "_", bone, ".txt", sep = ""), header=F, sep = "\t")
  #data(deparse(substitute(paste0("right", "_", "femur"))))

  rgl::open3d()
  rgl::plot3d(bone_data[,1], bone_data[,2], bone_data[,3],
              col="lightgray",
              xlim=c(-20,350), ylim=c(-20,100), zlim=c(-20,100),
              xlab="", ylab="", zlab="")
  rgl::aspect3d("iso")

}
