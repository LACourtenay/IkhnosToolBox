
check_colours <- function(col_list) {
  sapply(col_list, function (values) {
    tryCatch(is.matrix(col2rgb(values)),
             error = function(e) FALSE)
  })
}
