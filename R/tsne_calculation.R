
tsne_calculation <- function(data, labels = NULL, n_iterations = 1000, perplexity = 0,
                             plot_colours = NULL,
                             point_size = 2) {

  if(!is.matrix(data)) {
    return(warning("Input data must be in a numerical matrix format"))
  }

  if (perplexity == 0) {
    perplexity = dim(data)[1] ** (1/2)
  } else {
    if (perplextiy < 0) {
      return(warning("Perplexity must be a non-negative number"))
    }
  }

  if (n_iterations <= 0 | n_iterations %% 1 != 0) {
    return(warning("The number of permutations must be a positive, non-zero integer"))
  }

  if (!is.null(plot_colours)) {
    colour_bool <- check_colours(plot_colours)
    if (FALSE %in% colour_bool) {
      return(warning("Invalid colour provided for plot_colours"))
    }
  }

  if(!is.null(labels)) {
    if (nrow(data) != length(as.factor(labels))) {
      return(warning(
        paste0(
          "Only ", length(as.factor(labels)), " labels have been provided for",
          "a set of ", nrow(data), " coordinates."
        )
      ))
    }
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
      return(warning(
        paste0("This function by default provides 6 possible label colours,",
            " however the data introduced contains ",
            length(levels(as.factor(labels))), " groups.\n",
            "Please provide a set of ", length(levels(as.factor(labels))),
            " colours for the current plot.")
      ))
    }

    ggplot_colours <- ggplot2::scale_color_manual(
      values = plot_colours
    )

  } else {

    if (length(plot_colours) < length(levels(as.factor(labels)))) {
      return(warning("Insufficient label_colours arguments provided."))
    }

    ggplot_colours <- ggplot2::scale_color_manual(
      values = plot_colours
    )

  }

  tsne_plot <- base_plot +
    ggplot2::geom_point(size = point_size) +
    ggplot_colours +
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

  return(list(
    tsne_plot_object = tsne_plot,
    plot_data = dim_red
  ))
}
