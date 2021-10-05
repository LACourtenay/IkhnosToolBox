
check_colours <- function(col_list) {
  sapply(col_list, function (values) {
    tryCatch(is.matrix(col2rgb(values)),
             error = function(e) FALSE)
  })
}

add_alpha <- function(col, alpha=1){ # Function for setting the alpha of colours
  if(missing(col))
    return(warning("Please provide a vector of colours."))
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}
