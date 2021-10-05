
compare_two_sample_orientations <- function (sample_1, sample_2) {

  `%!in%` = Negate(`%in%`)

  if("circular" %!in% class(sample_1) |
     "circular" %!in% class(sample_2)) {
    return(warning(
      "Input to this function must be two objects of a 'circular' type"
    ))
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
