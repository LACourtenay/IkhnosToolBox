
time_series_clustering <- function(series_dataframe) {

  `%!in%` = Negate(`%in%`)

  if (!is.data.frame(series_dataframe) |
      ncol(series_dataframe) != 3 |
      "Frequencies" %!in% colnames(series_dataframe) |
      "Sequence" %!in% colnames(series_dataframe) |
      "Sample" %!in% colnames(series_dataframe)) {
    return(warning(
      paste0("\nInvalid data.frame input into time_series_clustering.\n",
             "Input to this function must be a 3 column data.frame containing",
             " a Sequence column, a Frequencies column, and a Sample column.")
    ))
  }

  split_dataframe <- split(series_database, series_database$Sample)

  wavelet_array <- array(
    numeric(),
    dim = c(
      length(split_dataframe),
      nrow(biwavelet::wt(split_dataframe[[1]][,1:2])$wave),
      ncol(biwavelet::wt(split_dataframe[[1]][,1:2])$wave)
    )
  )

  for (series_i in 1:length(split_dataframe)) {
    separate_series <- split_dataframe[[series_i]]
    wave_data <- biwavelet::wt(separate_series[,1:2])$wave
    wavelet_array[series_i, , ] <- wave_data
  }

  distance_matrix <- as.matrix(biwavelet::wclust(wavelet_array)$dist.mat)
  colnames(distance_matrix) <- levels(series_dataframe$Sample)
  rownames(distance_matrix) <- levels(series_dataframe$Sample)
  distance_matrix <- as.dist(distance_matrix)
  clustering_data <- hclust(distance_matrix, method = "ward.D2")

  X11()
  gridExtra::grid.arrange(
    factoextra::fviz_dend(clustering_data , cex = 0.5, k = 2,
                          k_colors = "jco",
                          type = "circular"),
    factoextra::fviz_dend(clustering_data , k = 2, cex=1.9,
                          k_colors = "jco",
                          type = "phylogenic", repel = TRUE),
    ncol = 2
  )

}
