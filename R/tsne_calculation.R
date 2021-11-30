
#' Calculate a t-Distributed Stochastic Neighbor Embedding (t-SNE).
#'
#' @description The present function provides a data exploration and
#' visualisation technique based on a non-linear dimensionality reduction
#' algorithm that constructs a low dimensional embedding of high-dimensional
#' data for the identification of patterns and trends in the data. An additional
#' non-supervised machine learning algorithm (DBSCAN) is used in this function to
#' detect patterns in the new feature space, which can be used to define groups
#' of marks.
#'
#' @param data An array containing the 3-dimensional points.
#' @param bone A 3D model of the bone used for analyses provided by data()
#' @param labels A factor containing the group association for each dataset.
#' @param n_iterations A positive, non-zero integer to define the number of
#' permutations for optimization (default = 1000).
#' @param perplexity A positive, non-zero integer to define the optimal number
#' of neighbors. Larger datasets require a larger perplexity, typical values are
#' between 5 and 50. By default the optimal parameter is calculated for each
#' dataset.
#' @param cluster_eps A positive, non-zero numeric value used to define the eps
#' parameter in the DBSCAN clustering algorithm. If no value is provided, 3 is used as default.
#' @param plot_colours A character vector to assign a specific colour to each
#' label.
#' @param point_size A non-negative integer to define the size of the points in
#' the plot.
#' @param create_external_plot A boolean TRUE or FALSE (default = TRUE) option
#' to generate an external window for the t-SNE plot.
#'
#' @section Bibliography:
#' Hinton, G.E.; Roweis, S.T. (2003) Stochastic Neighbor Embedding, Advances in Neural
#' Information Processing Systems. 857-864.
#'
#' Esther, M.; Kriegel, H.P.; Sander, J.; Xu, X. (1996) A density based algorithm
#' for discovering clusters in large spatial databases with noise. In: Proceedings of the
#' 2nd International Conference on Knowledge Discovery and Data Mining, Munchen, Germany.
#' \code{\link{https://www.aaai.org/Papers/KDD/1996/KDD96-037.pdf}}
#'
#' @section Notes:
#' Due to the stochastic nature of the tSNE algorithm, the authors recommend
#' executing this function a number of times to obtain the most reliable overview
#' of the results.
#'
#' @return A \code{data.frame} object containing the x, y coordinates and the
#' associated group represented in the graph that appears in
#' a popup window if create_external_plot = TRUE.
#'
#' @seealso \code{\link{Rtsne}} for tSNE, \code{\link{fpc}} for DBSCAN
#'
#' @author Lloyd A. Courtenay
#'
#' @examples
#' data("femur_right_circular1")
#' data("femur_right_linear1")
#' data("right_femur")
#' example_data1 <- load_marks(femur_right_circular1, mark_type = "circular")
#' example_data2 <- load_marks(femur_right_linear1, mark_type = "linear")
#' example_sp_object1 <- extract_spatial_data(example_data1, "circular")
#' example_sp_object2 <- extract_spatial_data(example_data2, "circular")
#' sample1_coords <- as.matrix(example_sp_object1)
#' sample2_coords <- as.matrix(example_sp_object2)
#' sample1_sample2 <- rbind(sample1_coords, sample2_coords)
#' group_labels <- as.factor(
#'   c(rep("circular", nrow(sample1_coords)),
#'   rep("linear", nrow(sample2_coords)))
#' )
#' tsne_calculation(
#'   sample1_sample2, right_femur, group_labels, plot_colours = c("red", "green"),
#'   create_external_plot = FALSE
#' )
#' @export

tsne_calculation <- function(data, bone, labels = NULL, n_iterations = 1000, perplexity = NULL,
                             cluster_eps = 3,
                             plot_colours = NULL,
                             point_size = 2,
                             create_external_plot = TRUE) {

  if(missing(data)) {
    stop("The user has not specified the data to be used for tSNE calculations")
  } else if(!is.matrix(data)) {
    stop("Input data must be in a numerical matrix format")
  }

  if(missing(bone)) {
    stop(paste0(
      "\nThe bone is missing!\nThe user must load the bone where marks are located",
      " using the data() function."
    ))
  }

  if (is.null(perplexity)) {
    perplexity = dim(data)[1] ** (1/2)
  } else {
    if (perplextiy < 0) {
      stop("Perplexity must be a non-negative number")
    }
  }

  if (!is.numeric(cluster_eps) | cluster_eps <= 0) {
    stop("cluster_eps must be a non-negative numeric number")
  }

  if (n_iterations <= 0 | n_iterations %% 1 != 0) {
    stop("The number of permutations must be a positive, non-zero integer")
  }

  if (!is.numeric(point_size) | point_size <= 0) {
    stop("point_size must be a numeric value")
  }

  if (!is.null(plot_colours)) {
    colour_bool <- check_colours(plot_colours)
    if (FALSE %in% colour_bool) {
      stop("Invalid colour provided for plot_colours")
    }
  }

  if(!is.null(labels)) {
    if (nrow(data) != length(as.factor(labels))) {
      stop(
        paste0(
          "Only ", length(as.factor(labels)), " labels have been provided for",
          "a set of ", nrow(data), " coordinates."
        )
      )
    }
  }

  if (!is.logical(create_external_plot)) {
    stop("create_external_plot must be either TRUE or FALSE")
  }

  data_base <- as.data.frame(data)
  if (!is.null(labels)) {
    data_base$Sample <- as.factor(labels)
  }
  data_base <- data_base[!duplicated(data_base[,1:3]),]
  coordinates <- as.matrix(data_base[,1:3])

  tsne_results <- Rtsne::Rtsne(
    coordinates, dims = 2,
    perplexity = perplexity,
    verbose = TRUE,
    max_iter = n_iterations
  )

  cluster_data <- fpc::dbscan(tsne_results$Y, eps = cluster_eps, MinPts = 3, method = "hybrid")
  clusters <- as.factor(cluster_data$cluster)
  threed_plot <- data.frame(coordinates, Sample = clusters)
  original_plot <- data.frame(tsne_results$Y, Sample = clusters)

  rgl::open3d()
  rgl::plot3d(bone[,1], bone[,2], bone[,3],
              col="lightgray",
              xlim=c(-20,350), ylim=c(-20,100), zlim=c(-20,100),
              xlab="", ylab="", zlab="")
  rgl::aspect3d("iso")

  cols <- RColorBrewer::brewer.pal(n = length(levels(clusters)),
                                   name = "Dark2")
  for(i in 1:length(levels(clusters))){
    rgl::spheres3d(threed_plot[clusters == levels(clusters)[i],1],
                   threed_plot[clusters == levels(clusters)[i],2],
                   threed_plot[clusters == levels(clusters)[i],3],
                   r = 2, col = cols[i])
  }

  tsne_cluster_plot <- ggplot2::ggplot(data = original_plot,
                                       ggplot2::aes(x = X1, y = X2, colour = clusters))

  if (is.null(labels)) {
    dim_red <- data.frame(x = tsne_results$Y[,1],
                          y = tsne_results$Y[,2])
    base_plot <- ggplot2::ggplot(
      data = dim_red,
      ggplot2::aes(x = x, y = y)
    )
  } else {
    dim_red <- data.frame(x = tsne_results$Y[,1],
                          y = tsne_results$Y[,2],
                          Sample = data_base$Sample)
    base_plot <- ggplot2::ggplot(
      data = dim_red,
      ggplot2::aes(x = x, y = y,
                   colour = Sample)
    )

  }

  if (is.null(plot_colours)) {

    plot_colours <- c("black", "red", "blue", "orange", "darkgreen", "violetred")

    if (length(plot_colours) < length(levels(as.factor(labels)))) {
      stop(
        paste0("This function by default provides 6 possible label colours,",
            " however the data introduced contains ",
            length(levels(as.factor(labels))), " groups.\n",
            "Please provide a set of ", length(levels(as.factor(labels))),
            " colours for the current plot.")
      )
    }

    ggplot_colours <- ggplot2::scale_color_manual(
      values = plot_colours
    )

  } else {

    if (length(plot_colours) < length(levels(as.factor(labels)))) {
      stop("Insufficient label_colours arguments provided.")
    }

    ggplot_colours <- ggplot2::scale_color_manual(
      values = plot_colours
    )

  }

  tsne_cluster_plot <- tsne_cluster_plot +
    ggplot2::geom_point(size = point_size) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::xlab("tSNE 1") + ggplot2::ylab("tSNE 2") +
    ggplot2::ggtitle("Clustering Results") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(1,1,1,1), "cm"),
      plot.title = ggplot2::element_text(face = "bold", size = 20),
      plot.subtitle = ggplot2::element_text(size = 15),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      axis.title.x = ggplot2::element_text(face = "bold", size = 18,
                                           margin = ggplot2::margin(
                                             t = 10, r = 0, b = 5, l = 0
                                           )),
      axis.title.y = ggplot2::element_text(face = "bold", size = 18,
                                           margin = ggplot2::margin(
                                             t = 0, r = 10, b = 0, l = 0
                                           )),
      axis.text.x = ggplot2::element_text(angle = 90, size = 15, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 15, face = "bold"),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::geom_vline(xintercept = 0, colour = "black", size = 0.5, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, colour = "black", linetype = "dashed", size = 0.5)

  tsne_plot <- base_plot +
    ggplot2::geom_point(size = point_size) +
    ggplot_colours +
    ggplot2::xlab("tSNE 1") + ggplot2::ylab("tSNE 2") +
    ggplot2::ggtitle("tSNE Results") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(1,1,1,1), "cm"),
      plot.title = ggplot2::element_text(face = "bold", size = 20),
      plot.subtitle = ggplot2::element_text(size = 15),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      axis.title.x = ggplot2::element_text(face = "bold", size = 18,
                                           margin = ggplot2::margin(
                                             t = 10, r = 0, b = 5, l = 0
                                           )),
      axis.title.y = ggplot2::element_text(face = "bold", size = 18,
                                           margin = ggplot2::margin(
                                             t = 0, r = 10, b = 0, l = 0
                                           )),
      axis.text.x = ggplot2::element_text(angle = 90, size = 15, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 15, face = "bold"),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 18, face = "bold"),
      legend.background = ggplot2::element_rect(fill = add_alpha("#CCCCCC",
                                                                 alpha = 0.2)),
      legend.box.background = ggplot2::element_rect(colour = "black"),
      legend.position = "bottom"
    ) +
    ggplot2::geom_vline(xintercept = 0, colour = "black", size = 0.5, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, colour = "black", linetype = "dashed", size = 0.5)

  if (create_external_plot == TRUE) {
    if (!is.null(labels)) {
      X11(); gridExtra::grid.arrange(tsne_plot, tsne_cluster_plot, ncol = 2)
    } else {
      X11(); gridExtra::grid.arrange(tsne_cluster_plot)
    }
  } else {
    if (!is.null(labels)) {
      gridExtra::grid.arrange(tsne_plot, tsne_cluster_plot, ncol = 2)
    } else {
      gridExtra::grid.arrange(tsne_cluster_plot)
    }
  }

  return(dim_red)
}
