
#' Uniformity test for the orientation of linear marks.
#'
#' @description Function to conduct orientation analyses on scores and/or cut
#' marks using the Rayleigh distribution test.
#'
#' @param circular_object A circular object containing the angles calculated
#' between the bone long axis and each linear mark.
#'
#'
#' @return A list with the results for Rayleigh uniformity test and the
#' associated p value, being p < 0.05 indicative of a non-uniform distribution.
#'
#' @seealso \code{\link{calculate_orientations}}, \code{\link{rayleigh.test}}.
#'
#' @section Bibliography:
#' Jammalamadaka, S. Rao and SenGupta, A. (2001). Topics in Circular Statistics,
#' World Scientific Press, Singapore.
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
#' example_pref_orientation1 <- preferential_orientation_test(example_calculate_orientations_1)
#' example_pref_orientation2 <- preferential_orientation_test(example_calculate_orientations_2)
#' @export


preferential_orientation_test <- function(circular_object) {

  `%!in%` = Negate(`%in%`)

  if("circular" %!in% class(circular_object)) {
    stop(
      "Input to this function must be of a 'circular' type"
    )
  }

  # p < 0.05 = non-uniform distribution i.e. concentration of measurements

  # Testing for Uniformity
  # Rayleigh on continuous data

  return(list(
    test_statistic = circular::rayleigh.test(circular_object)$statistic,
    p_value = circular::rayleigh.test(circular_object)$p))


}
