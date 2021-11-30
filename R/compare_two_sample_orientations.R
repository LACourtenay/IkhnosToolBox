
#' Pair comparison for homogeneity on two orientation patterns.
#'
#' @description Function to compare the orientation patterns observed in two
#' different samples of linear marks (scores and/or cut marks)using the
#' randomised Mardia-Watson-Wheeler test.
#'
#' @param sample_1 A circular object containing the angles calculated between
#' the bone long axis and each linear mark in the first dataset.
#' @param sample_2 A circular object containing the angles calculated between
#' the bone long axis and each linear mark in the second dataset.
#'
#' @return A list containing the results obtained for the Mardia-Watson-Wheeler
#' test and the associated p value.
#'
#' @seealso \code{\link{calculate_orientations}}, \code{\link{circular}}.
#'
#' @author Lloyd A.Courtenay
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
#' example_comp_orientations <- compare_two_sample_orientations(
#'   example_calculate_orientations_1, example_calculate_orientations_2
#' )
#' @export

compare_two_sample_orientations <- function (sample_1, sample_2) {

  `%!in%` = Negate(`%in%`)

  if("circular" %!in% class(sample_1) |
     "circular" %!in% class(sample_2)) {
    stop(
      "Input to this function must be two objects of a 'circular' type"
    )
  }

  sample_1_sym <- circular::circular(sample_1 - pi,
                                     type = "directions",
                                     units = "radians")
  sample_2_sym <- circular::circular(sample_2 - pi,
                                     type = "directions",
                                     units = "radians")

  sample_1 <- c(sample_1, sample_1_sym)
  sample_2 <- c(sample_2, sample_2_sym)


  target<-c(sample_1, sample_2)
  ndat<-c(length(sample_1), length(sample_2))
  g<-2

  pchisq(WgVal(CosSinUniScores(target = target), ndat = ndat, g = g),
         2*(g-1), lower.tail = FALSE)

  # Randomized Madria-Watson-Wheeler **** - test statistic is Wg

  return(list(
    test_statistic = WgVal(CosSinUniScores(target = target), ndat = ndat, g = g),
    p_value = WgTestRand(CosSinUniScores(target = target), ndat = ndat, g = g,
                         NR = 9999)))

}
