
#' Cluster analysis.
#'
#' @description The present function is used to conduct a cluster analysis on
#' several samples.
#'
#' @param series_dataframe A data frame including the sequence and frequencies
#' per sample.
#' @param create_external_plot A boolean TRUE or FALSE (default = TRUE) option
#' to create a popup window with the plot for the cluster analysis.
#'
#'
#' @return If create_external_plot = TRUE, then the function returns a popup
#' window with a plot on the cluster analysis.
#'
#' @seealso \code{\link{create_time_series}}, \code{\link{add_time_series}}.
#'
#' @examples
#' data(humerus_right_circular1)
#' data(femur_right_circular1)
#' data(radius_right_circular1)
#' data(tibia_right_circular1)  #COMPROBAR ESTO
#' h1 <- load_marks(femur_right_circular1, mark_type = "circular", plot = FALSE) #ESTO DA ERROR POR NO CARGAR EL RDA
#' f1 <- load_marks(femur_right_circular1, mark_type = "circular", plot = FALSE)
#' r1 <- load_marks(femur_right_circular1, mark_type = "circular", plot = FALSE)
#' t1 <- load_marks(femur_right_circular1, mark_type = "circular", plot = FALSE)
#' time_series_1 <- create_time_series(h = h1, f = f1, r = r1, t = t1)
#'
#' data(humerus_right_circular2)
#' data(femur_right_circular2)
#' data(radius_right_circular2)
#' data(tibia_right_circular2)
#' h1 <- load_marks(femur_right_circular2, mark_type = "circular", plot = FALSE)
#' f1 <- load_marks(femur_right_circular2, mark_type = "circular", plot = FALSE)
#' r1 <- load_marks(femur_right_circular2, mark_type = "circular", plot = FALSE)
#' t1 <- load_marks(femur_right_circular2, mark_type = "circular", plot = FALSE)
#' time_series_2 <- create_time_series(h = h2, f = f2, r = r2, t = t2, colour = "red")
#'
#' data(humerus_right_circular3)
#' data(femur_right_circular3)
#' data(radius_right_circular3)
#' data(tibia_right_circular3)
#' h1 <- load_marks(femur_right_circular3, mark_type = "circular", plot = FALSE)
#' f1 <- load_marks(femur_right_circular3, mark_type = "circular", plot = FALSE)
#' r1 <- load_marks(femur_right_circular3, mark_type = "circular", plot = FALSE)
#' t1 <- load_marks(femur_right_circular3, mark_type = "circular", plot = FALSE)
#' time_series_3 <- create_time_series(h = h3, f = f3, r = r3, t = t3, colour = "blue")
#'
#'series_database <- rbind(time_series_1, time_series_2, time_series_3)
#'series_database$Sample <- as.factor(c(rep("Series_1", nrow(time_series_1)),rep("Series_2", nrow(time_series_2)),rep("Series_3", nrow(time_series_3))))
#'time_series_clustering(series_database)




#'Warning message:`guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead.


time_series_clustering <- function(series_dataframe,
                                   create_external_plot = TRUE) {

  `%!in%` = Negate(`%in%`)

  if (!is.data.frame(series_dataframe) |
      ncol(series_dataframe) != 3 |
      "Frequencies" %!in% colnames(series_dataframe) |
      "Sequence" %!in% colnames(series_dataframe) |
      "Sample" %!in% colnames(series_dataframe)) {
    stop(
      paste0("\nInvalid data.frame input into time_series_clustering.\n",
             "Input to this function must be a 3 column data.frame containing",
             " a Sequence column, a Frequencies column, and a Sample column.")
    )
  }

  if (!is.logical(create_external_plot)) {
    stop("create_external_plot must be either TRUE or FALSE")
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

  if (create_external_plot == TRUE) {
    X11(); gridExtra::grid.arrange(
      factoextra::fviz_dend(clustering_data , cex = 0.5, k = 2,
                            k_colors = "jco",
                            type = "circular"),
      factoextra::fviz_dend(clustering_data , k = 2, cex=1.9,
                            k_colors = "jco",
                            type = "phylogenic", repel = TRUE),
      ncol = 2
    )
  } else {
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

}
