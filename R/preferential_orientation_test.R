
preferential_orientation_test <- function(circular_object) {

  `%!in%` = Negate(`%in%`)

  if("circular" %!in% class(circular_object)) {
    return(warning(
      "Input to this function must be of a 'circular' type"
    ))
  }

  # p < 0.05 = non-uniform distribution i.e. concentration of measurements

  # Testing for Uniformity
  # Rayleigh on continuous data

  return(list(
    test_statistic = circular::rayleigh.test(circular_object)$statistic,
    p_value = circular::rayleigh.test(circular_object)$p))


}
