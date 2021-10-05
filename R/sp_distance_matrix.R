
sp_distance_matrix <- function(spatial_object, name_1 = "marks",
                               create_external_plot = TRUE) {

  `%!in%` = Negate(`%in%`)

  if("pp3" %!in% class(spatial_object)) {
    return(warning("Invalid spatial object"))
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
