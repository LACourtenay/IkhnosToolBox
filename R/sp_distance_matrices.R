
distance_matrices <- function(spatial_object_1, spatial_object_2,
                              name_1, name_2) {


  `%!in%` = Negate(`%in%`)

  if("pp3" %!in% class(spatial_object_1) |
     "pp3" %!in% class(spatial_object_2)) {
    return(warning("Invalid spatial objects"))
  }

  name_1 <- as.character(name_1)
  name_2 <- as.character(name_2)


  pd_1 <- spatstat.geom::nndist(spatial_object_1)
  pd_2 <- spatstat.geom::nndist(spatial_object_2)

  X11(); par(mfrow = c(1,2))
  plot(pd_1, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
       main = paste("Distance between ", name_1, sep = ""))
  plot(pd_2, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
       main = paste("Distance between ", name_2, sep = ""))
  par(mfrow = c(1,1))

  X11(); par(mfrow = c(1,1))

  cd <- spatstat.geom::nndist(
    spatstat.geom::crossdist(spatial_object_1, spatial_object_2)
  )

  plot(cd, pch = 19, xlab = "Nearest Neighbour Distance", ylab = "Index",
       main = paste("Distance Matrix between ", name_1, " and ", name_2, sep = ""))

}
