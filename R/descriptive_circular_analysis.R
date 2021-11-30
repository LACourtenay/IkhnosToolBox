
#' Descriptive circular analysis.
#'
#' @description The present function contains several parameters to characterise
#' the location and variability of linear marks along the bone, including
#' normality distribution tests such as skewness (measure of asymmetry) and
#' kurtosis (measure of tailedness and peakedness), circular variance and
#' dispersion, and the orientation of the radians and degrees.
#'
#' @param circular_object A circular object containing the angles calculated
#' between the bone long axis and each linear mark.
#'
#' @return A list with the results obtained for the standarised measures of
#' Skewness and Kurtosis, the sample circular variance and dispersion,
#' and the central orientation radians and degrees.
#'
#' @seealso \code{\link{calculate_orientations}}, \code{\link{circular}},
#' \code{\link{trigonometric.moment}}.
#'
#'
#' @author Lloyd A.Courtenay
#'
#' @section Bibliography:
#' Jammalamadaka, S. Rao and SenGupta, A. (2001). Topics in Circular Statistics,
#' World Scientific Press, Singapore.
#'
#'
#' @examples
#' data("femur_right_linear1")
#' data("femur_right_linear2")
#' data("right_femur")
#' example_circ_1 <- load_marks(femur_right_linear1, mark_type = "linear")
#' example_circ_2 <- load_marks(femur_right_linear2, mark_type = "linear")
#' example_calculate_orientations_1 <- calculate_orientations(
#'   example_circ_1, right_femur,
#'   create_external_plot = FALSE
#' )
#' example_calculate_orientations_2 <- calculate_orientations(
#'   example_circ_2, right_femur,
#'   create_external_plot = FALSE
#' )
#' example_circ_analysis1 <- descriptive_circular_analysis(example_calculate_orientations_1)
#' example_circ_analysis2 <- descriptive_circular_analysis(example_calculate_orientations_2)
#' @export

descriptive_circular_analysis <- function(circular_object) {

  `%!in%` = Negate(`%in%`)

  if("circular" %!in% class(circular_object)) {
    stop(
      "Input to this function must be of a 'circular' type"
    )
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
