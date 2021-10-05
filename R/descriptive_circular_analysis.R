
descriptive_circular_analysis <- function(circular_object) {

  `%!in%` = Negate(`%in%`)

  if("circular" %!in% class(circular_object)) {
    return(warning(
      "Input to this function must be of a 'circular' type"
    ))
  }

  target <- circular::circular(circular_object,
                               type = "directions",
                               units = "radians")

  # shat according to Mardia 1972
  # datapoints will be concentrated about the mean if shat is close to 0

  s_hat <- (
    circular::trigonometric.moment(target,
                         p = 2,
                         center = TRUE)$sin)/((1 - circular::trigonometric.moment(
                           target,
                           p = 1
                         )$rho)**(3/2)
                         )

  # khat according to Mardia 1972
  # datapoints close to 0 are flat while peaks are seen the further away you get from 0
  k_hat <- (
    circular::trigonometric.moment(target,
                         p = 2,
                         center = TRUE)$cos - circular::trigonometric.moment(
                           target,
                           p = 1)$rho**4
                         )/((1 - circular::trigonometric.moment(
                           target,
                           p = 1)$rho)**2

                         )

  # Sample circular variance (V)
  # value between 0 and 1. the closer to 0 the value the more concentrated the datapoints
  V <- (
    1 - circular::trigonometric.moment(target, p = 1)$rho
  )

  # delta hat is the sample circular dispersion

  delta_hat <- (
    1-circular::trigonometric.moment(
      target, p = 2
    )$rho)/(2*circular::trigonometric.moment(
      target, p = 1
    )$rho**2
    )

  Opposite <- circular::circular(target - pi, type = "directions", units = "radians")

  return(list(
    Standardised_Skewness = s_hat,
    Standardised_Kurtosis = k_hat,
    Sample_Circular_Variance = V,
    Sample_Circular_Dispersion = delta_hat,
    Central_Orientation_Radians = c(calc_mean(target), calc_mean(Opposite)),
    Central_Orientation_Degrees = c(calc_mean(target) * (180/pi), calc_mean(Opposite) * (180/pi))
  ))

}
