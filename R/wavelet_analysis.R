
#' Bivariate wavelet analyses in frequency domains.
#'
#' @description The present function is used to conduct wavelet coherence
#' analyses to assess the dependence existing between two mark patterns.
#'
#' @param first_ts A matrix (n rows x 2 columns) containing the time series of
#' the first sample. The first column contains the time steps (breaks) and the
#' second column contains the values (counts).
#' @param second_ts A matrix (n rows x 2 columns) containing the time series of
#' the second sample. The first column contains the time steps (breaks) and the
#' second column contains the values (counts).
#' @param sample_1_name A string to define the label that corresponds to
#' \code{first_ts}.
#' @param sample_2_name A string to define the label that corresponds to
#' \code{second_ts}.
#' @param n_iterations A positive, non-zero integer to define the number of
#' Monte Carlo randomisation (default = 10000).
#' @param x_lab A string to define the title of the x axis.It should coincide
#' with the dimension selected for the \code{two_sample_histogram_distributions}
#' function.
#' @param create_external_plot A boolean TRUE or FALSE (default = TRUE) option
#' to create a popup window with the plot for the wavelet coherence analysis.
#'
#' @return If create_external_plot = TRUE, then the function returns a popup
#' window with a biwavelet plot to visualise the dependence between
#' \code{first_ts} and \code{second_ts}.
#'
#' @seealso \code{\link{biwavelet}}, \code{\link{wtc}}, \code{\link{plot.biwavelet}},
#' \code{\link{two_sample_histogram_distributions}}.
#'
#'
#' @examples
#' data(femur_right_circular1) #COMPROBAR ESTO
#' data(femur_right_linear1) #COMPROBAR ESTO
#' example_data1 <- load_marks(femur_right_circular1, mark_type = "circular") #ESTO DA ERROR POR NO CARGAR EL RDA
#' example_data2 <- load_marks(femur_right_linear1, mark_type = "linear") #ESTO DA ERROR POR NO CARGAR EL RDA
#' example_sp_object1 <- extract_spatial_data(example_data1, "circular")
#' example_sp_object2 <- extract_spatial_data(example_data2, "circular")
#' example_time_series <- two_sample_histogram_distributions(example_sp_object1, example_sp_object2,"circular", "linear","x", "femur")
#' example_wavelet_analysis <- wavelet_analysis(example_time_series$first_sample_ts, example_time_series$second_sample_ts, "circular", "linear")


wavelet_analysis <- function (first_ts, second_ts, sample_1_name, sample_2_name,
                              n_iterations = 10000,
                              x_lab = "Inter-epiphysis distance",
                              create_external_plot = TRUE) {

  if(missing(first_ts) | missing(second_ts)) {
    stop(
      "This function is for the comparison of two spatial sime series, yet only one has been provided"
    )
  } else if (!is.matrix(first_ts) | !is.matrix(first_ts)) {
    stop(
      "Input data must be in a numerical matrix format"
    )
  }

  if(missing(sample_1_name)) {
    stop("Missing sample 1 name")
  }

  if(missing(sample_2_name)) {
    stop("Missing sample 2 name")
  }

  if (n_iterations <= 0 | n_iterations %% 1 != 0) {
    stop("The number of permutations must be a positive, non-zero integer")
  }

  wavelet_coh <- biwavelet::wtc(first_ts,
                     second_ts,
                     nrands = n_iterations)

  if(create_external_plot == TRUE) {
    X11(width = 30, height = 20); par(
      oma = c(0, 0, 0, 2), mar = c(5.1, 4.1, 5.1, 5.1)
    )
  } else {
    par(
      oma = c(0, 0, 0, 2), mar = c(5.1, 4.1, 5.1, 5.1)
    )
  }

  biwavelet::plot.biwavelet(
    wavelet_coh, plot.phase = TRUE, lty.coi = 1,
    col.coi = "grey", lwd.coi = 2, tol = 0.6,
    lwd.sig = 5, arrow.lwd = 0.03, arrow.len = 0.12,
    arrow.cutoff = 0.5, plot.cb = TRUE,
    ylab = "Scale", xlab = as.character(x_lab),
    main = paste(
      "Wavelet Coherence: ", sample_1_name, " vs ", sample_2_name, sep = ""
    )
  )

  par(mfrow = c(1,1), oma = c(0, 0, 0, 0), mar = c(5.1, 4.1, 4.1, 2.1))

}
