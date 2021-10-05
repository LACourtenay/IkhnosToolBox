
wavelet_analysis <- function (first_ts, second_ts, sample_1_name, sample_2_name,
                              n_iterations = 10000,
                              x_lab = "Inter-epiphysis distance",
                              create_external_plot = TRUE) {

  if(missing(first_ts) | missing(second_ts)) {
    return(warning(
      "This function is for the comparison of two spatial sime series, yet only one has been provided"
    ))
  } else if (!is.matrix(first_ts) | !is.matrix(first_ts)) {
    return(warning(
      "Input data must be in a numerical matrix format"
    ))
  }

  if(missing(sample_1_name)) {
    return(warning("Missing sample 1 name"))
  }

  if(missing(sample_2_name)) {
    return(warning("Missing sample 2 name"))
  }

  if (n_iterations <= 0 | n_iterations %% 1 != 0) {
    return(warning("The number of permutations must be a positive, non-zero integer"))
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
