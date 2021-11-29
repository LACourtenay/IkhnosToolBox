
#' Spatial analysis on 3-dimensional point patterns.
#'
#' @description The present function conducts spatial analyses on the
#' 3-dimensional point patterns representing the marks using the \code{K3est},
#' \code{F3est}, \code{G3est} and \code{pcf3est} functions.
#'  - \code{K3est}: estimates the K-function from a 3d point pattern
#'  - \code{F3est}: estimates the empty space function \code{F3(r)} from a 3d
#'  point pattern
#'  - \code{G3est}: estimates the nearest-neighbour distance distribution
#'  function \code{G3(r)} from a 3d point pattern
#'  - \code{pcf3est}: estimates the pair correlation function from a 3d point
#'  pattern
#' Square root versions of the \code{K3est}, \code{F3est} and \code{G3est}
#' functions are included to stabilise variance if needed.
#' Warning: functions assume that point distribution is homogeneous and could
#' therefore inflate the identification of clustering patterns when in-
#' homogeneous distributions are present. Functions for inhomogeneous patterns
#' are not available for 3-dimensional point patterns yet.
#'
#' @param spatial_object A pp3 object containing the 3-dimensional point pattern.
#' @param n_permutations A positive, non-zero integer to define the number of
#' permutations (default = 1000, min n = 101).
#' @param create_external_plot A boolean TRUE or FALSE option (default = TRUE)
#' to create three popup windows with the plots for the four functions and their
#' respective square root versions.
#'
#' @return If create_external_plot = TRUE, then the function returns three popup
#' windows containing:
#' 1. the plots for the output of the \code{K3est}, \code{F3est} and \code{G3est}
#' functions
#' 2. the plots for the output of the square root versions of the \code{K3est},
#' \code{F3est} and \code{G3est} functions
#' 3. the plot for the output of the \code{pcf3est} function
#'
#' @seealso \code{\link{spatstat.core}}.
#'
#' @examples
#' data(femur_right_circular1) #COMPROBAR ESTO
#' example_data <- load_marks(femur_right_circular1, mark_type = "circular") #ESTO DA ERROR POR NO CARGAR EL RDA
#' example_sp_object <- extract_spatial_data(example_data, "circular")
#' example_CSR_analyses <- perform_CSR_analyses(example_sp_object, n_permutations = 101)

perform_CSR_analyses <- function(spatial_object, n_permutations = 1000,
                                 create_external_plot = TRUE) {

  `%!in%` = Negate(`%in%`)

  if("pp3" %!in% class(spatial_object)) {
    return(stop("Invalid spatial object"))
  }

  if (n_permutations <= 0 | n_permutations %% 1 != 0) {
    return(stop("The number of permutations must be a positive, non-zero integer"))
  }

  if (n_permutations < 101) {
    return(stop("The minimum number of permutations required for this function is 101"))
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
