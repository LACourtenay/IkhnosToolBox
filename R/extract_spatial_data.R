
extract_spatial_data <- function(input_data,
                                 mark_type,
                                 print_summary = TRUE,
                                 plot_results = FALSE) {

  if (mark_type != "circular" & mark_type != "linear") {
    return(warning("Please select between circular or linear mark types"))
  }

  if (mark_type == "circular") {

    # in reality... will the columns always be named x1, y1 and z1?

    #xyz <- tibble::as_tibble(data) %>%
    #  select(x = x1, y = y1, z = z1)
    xyz <- input_data[[c("x1", "y1", "z1")]]
    colnames(xyz) <- c("x", "y", "z")

    xlim <- c(min(xyz$x), max(xyz$x))
    ylim <- c(min(xyz$y), max(xyz$y))
    zlim <- c(min(xyz$z), max(xyz$z))

    box <- spatstat::box3(xlim, ylim, zlim, unitname = "mm")
    spatial_data <- spatstat::pp3(xyz$x, xyz$y, xyz$z, box) # Create 3d spatial object

  } else {
    #xyz <- as_tibble(data) %>%
    #  select(x = x3, y = y3, z = z3)

    xyz <- input_data[[c("x1", "y1", "z1")]]
    colnames(xyz) <- c("x", "y", "z")

    xlim <- c(min(xyz$x), max(xyz$x))
    ylim <- c(min(xyz$y), max(xyz$y))
    zlim <- c(min(xyz$z), max(xyz$z))

    box <- spatstat::box3(xlim, ylim, zlim, unitname = "mm")
    spatial_data <- spatstat::pp3(xyz$x, xyz$y, xyz$z, box) # Create 3d spatial object

  }

  if (print_summary == TRUE) {
    print(summary(spatial_data))
  }

  if (plot_results == TRUE) {
    plot(spatial_data, main = "",
         pch = 1, bg = "white", cex = 1.2)
  }

  return(spatial_data) # check whether the box is necessary

}

#extract_spatial_data(example_c, "circular")
