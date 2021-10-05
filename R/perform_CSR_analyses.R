
perform_CSR_analyses <- function(spatial_object, n_permutations = 1000,
                                 pcf_method = "c",
                                 create_external_plot = TRUE) {

  `%!in%` = Negate(`%in%`)

  if("pp3" %!in% class(spatial_object)) {
    return(warning("Invalid spatial object"))
  }

  if (n_permutations <= 0 | n_permutations %% 1 != 0) {
    return(warning("The number of permutations must be a positive, non-zero integer"))
  }

  if (n_permutations < 101) {
    return(warning("The minimum number of permutations required for this function is 101"))
  }

  if (pcf_method != "c" & pcf_method != "b") {
    return(warning("Please select a pcf_method of a or b"))
  }

  cat("Generating simulations of Complete Spatial Randomness...\n")
  pb <- txtProgressBar(min = 0, max = 4,
                       style = 3, width = 100, char = "=")

  Ke <- spatstat.core::envelope(spatial_object,
                                spatstat.core::K3est,
                                nsim = n_permutations,
                                nrank = 50, nrval = 512,
                                verbose = FALSE)

  setTxtProgressBar(pb, 1)
  Fe <- spatstat.core::envelope(spatial_object,
                                spatstat.core::F3est,
                                nsim = n_permutations,
                                nrank = 50, nrval = 512,
                                verbose = FALSE)

  setTxtProgressBar(pb, 2)
  Ge <- spatstat.core::envelope(spatial_object,
                                spatstat.core::G3est,
                                nsim = n_permutations,
                                nrank = 50, nrval = 512,
                                verbose = FALSE)

  setTxtProgressBar(pb, 3)
  PCF_plot <- spatstat.core::envelope(spatial_object,
                                      spatstat.core::pcf3est,
                                      nsim = n_permutations,
                                      nrank = 50, nrval = 512,
                                      verbose = FALSE)

  setTxtProgressBar(pb, 4)

  for (i in 1:2) {

    if (create_external_plot == TRUE) {
      X11(
        width = 15, height = 6.5
      ); par(mfrow = c(1,3))
    } else {
      par(mfrow = c(1,3))
    }

    if ( i == 1) {
      plot(Ke, main = "Ripley's K f(x)")
      plot(Fe, main = "Estimator of Empty Space f(x)")
      plot(Ge, main = "Nearest-Neighbour f(x)")
    } else {
      plot(Ke, sqrt(.)~r, main = "Sqrt Ripley's K f(x)")
      plot(Fe, sqrt(.)~r, main = "Sqrt Estimator of Empty Space f(x)")
      plot(Ge, sqrt(.)~r, main = "Sqrt Nearest-Neighbour f(x)")
    }

  }

  par(mfrow = c(1,1))

  if(create_external_plot == TRUE) {
    X11(); plot(PCF_plot, main = "K Pair Correlation Function")
  } else {
    plot(PCF_plot, main = "K Pair Correlation Function")
  }

}
