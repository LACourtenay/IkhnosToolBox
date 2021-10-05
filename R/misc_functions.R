
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

extract_ts_data <- function(h = NULL, f = NULL, r = NULL, t = NULL, mt = NULL, mc = NULL) {
  f_breaks = seq(0, 300, 10)
  h_breaks = seq(0, 300, 10)
  mc_breaks = seq(0, 300, 10)
  r_breaks = seq(0, 350, 10)
  t_breaks = seq(0, 350, 10)
  mt_breaks = seq(0, 300, 10)

  series <- c()
  line_nums <- c()
  labels <- c()

  if (!is.null(h)) {

    if ("pp3" %in% class(h)) {

      h_series <- hist(as.data.frame(h$data[,1]),
                       breaks = h_breaks,
                       plot = FALSE)$counts

    } else if (is.data.frame(h)) {

      h_series <- hist(h$x1,
                       breaks = h_breaks,
                       plot = FALSE)$counts

    } else if (is.matrix(h)) {

      h_series <- hist(h[,1],
                       breaks = h_breaks,
                       plot = FALSE)$counts

    } else {
      return(warning(
        "Invalid input type for the creation of time_series_data in h"
      ))
    }

    freq_h <- seq(1, length(h_series))
    labels_h <- rep("H", length(h_series))
    series <- c(series, h_series)
    line_nums <- c(line_nums, freq_h)
    labels <- c(labels, labels_h)
  }

  if (!is.null(f)) {

    if ("pp3" %in% class(f)) {

      f_series <- hist(as.data.frame(f$data[,1]),
                       breaks = f_breaks,
                       plot = FALSE)$counts

    } else if (is.data.frame(f)) {

      f_series <- hist(f$x1,
                       breaks = f_breaks,
                       plot = FALSE)$counts

    } else if (is.matrix(f)) {

      f_series <- hist(f[,1],
                       breaks = f_breaks,
                       plot = FALSE)$counts

    } else {
      return(warning(
        "Invalid input type for the creation of time_series_data in f"
      ))
    }

    freq_f <- seq(1, length(f_series))
    labels_f <- rep("F", length(f_series))
    series <- c(series, f_series)
    line_nums <- c(line_nums, freq_f)
    labels <- c(labels, labels_f)
  }

  if (!is.null(r)) {

    if ("pp3" %in% class(r)) {

      r_series <- hist(as.data.frame(r$data[,1]),
                       breaks = r_breaks,
                       plot = FALSE)$counts

    } else if (is.data.frame(r)) {

      r_series <- hist(r$x1,
                       breaks = r_breaks,
                       plot = FALSE)$counts

    } else if (is.matrix(r)) {

      r_series <- hist(r[,1],
                       breaks = r_breaks,
                       plot = FALSE)$counts

    } else {
      return(warning(
        "Invalid input type for the creation of time_series_data in r"
      ))
    }

    freq_r <- seq(1, length(r_series))
    labels_r <- rep("R", length(r_series))
    series <- c(series, r_series)
    line_nums <- c(line_nums, freq_r)
    labels <- c(labels, labels_r)
  }

  if (!is.null(t)) {

    if ("pp3" %in% class(t)) {

      t_series <- hist(as.data.frame(t$data[,1]),
                       breaks = t_breaks,
                       plot = FALSE)$counts

    } else if (is.data.frame(t)) {

      t_series <- hist(t$x1,
                       breaks = t_breaks,
                       plot = FALSE)$counts

    } else if (is.matrix(t)) {

      t_series <- hist(t[,1],
                       breaks = t_breaks,
                       plot = FALSE)$counts

    } else {
      return(warning(
        "Invalid input type for the creation of time_series_data in t"
      ))
    }

    freq_t <- seq(1, length(t_series))
    labels_t <- rep("T", length(t_series))
    series <- c(series, t_series)
    line_nums <- c(line_nums, freq_t)
    labels <- c(labels, labels_t)
  }

  if (!is.null(mc)) {

    if ("pp3" %in% class(mc)) {

      mc_series <- hist(as.data.frame(mc$data[,1]),
                        breaks = mc_breaks,
                        plot = FALSE)$counts

    } else if (is.data.frame(mc)) {

      mc_series <- hist(mc$x1,
                        breaks = mc_breaks,
                        plot = FALSE)$counts

    } else if (is.matrix(mc)) {

      mc_series <- hist(mc[,1],
                        breaks = mc_breaks,
                        plot = FALSE)$counts

    } else {
      return(warning(
        "Invalid input type for the creation of time_series_data in mc"
      ))
    }

    freq_mc <- seq(1, length(mc_series))
    labels_mc <- rep("MC", length(mc_series))
    series <- c(series, mc_series)
    line_nums <- c(line_nums, freq_mc)
    labels <- c(labels, labels_mc)
  }

  if (!is.null(mt)) {

    if ("pp3" %in% class(mt)) {

      mt_series <- hist(as.data.frame(mt$data[,1]),
                        breaks = mt_breaks,
                        plot = FALSE)$counts

    } else if (is.data.frame(mc)) {

      mt_series <- hist(mt$x1,
                        breaks = mt_breaks,
                        plot = FALSE)$counts

    } else if (is.matrix(mt)) {

      mt_series <- hist(mt[,1],
                        breaks = mt_breaks,
                        plot = FALSE)$counts

    } else {
      return(warning(
        "Invalid input type for the creation of time_series_data in mt"
      ))
    }

    freq_mt <- seq(1, length(mt_series))
    labels_mt <- rep("MC", length(mt_series))
    series <- c(series, mt_series)
    line_nums <- c(line_nums, freq_mt)
    labels <- c(labels, labels_mt)
  }

  total_line <- seq(1, length(series))
  percentages = (series / sum(series))
  y_lim = c(0, max(percentages + 0.005))

  return(list(
    series = series,
    line_nums = line_nums,
    labels = labels,
    total_line = total_line,
    percentages = percentages,
    y_lim = y_lim
  ))

}

calc_mean<-function(target){
  x = numeric()
  y = numeric()
  for(i in 1:length(target)){
    y = c(y, sin(target[i][[1]]))
    x = c(x, cos(target[i][[1]]))
    Y = sum(y)/length(target) # B1
    X = sum(x)/length(target) # A1
    r = sqrt((X^2) + (Y^2)) # equivalent to rbar
    cosa = X/r
    sina = Y/r
    mr = atan2(sina,cosa) # equivalent to tbar
  }
  return(mr)
}

CosSinUniScores<-function(target) {
  N = length(target)
  ranks = rank(target, ties.method= "random")
  CosUniScores = cos(ranks*2*pi/N)
  SinUniScores = sin(ranks*2*pi/N)
  return(list(CosUniScores, SinUniScores))
}

WgVal<-function(CSUScores, ndat, g) {
  CosUScores = CSUScores[[1]]
  SinUScores = CSUScores[[2]]
  N = length(CosUScores)
  ndatcsum = cumsum(ndat)
  Wg = 0
  for (k in 1:g) {
    CosUScoresk = 0 ; SinUScoresk = 0
    if (k==1)
    { low = 0 } else
      if (k > 1)
      { low = ndatcsum[k - 1] }
    for (j in 1:ndat[k]) {
      CosUScoresk[j] = CosUScores[j+low] ; SinUScoresk[j] = SinUScores[j+low]
    }
    sumCkSq = (sum(CosUScoresk))**2
    sumSkSq = (sum(SinUScoresk))**2
    Wg = Wg+(sumCkSq+sumSkSq)/ndat[k] }
  Wg = 2*Wg
  return(Wg)
}

WgTestRand<-function(CSUScores, ndat, g, NR) {
  CosUScores = CSUScores[[1]]
  SinUScores = CSUScores[[2]]
  N = length(CosUScores)
  ndatcsum = cumsum(ndat)
  WgObs = WgVal(CSUScores, ndat, g)
  nxtrm = 1
  ind = seq(1, N)
  for (r in 1:NR) {
    CosUScoresRand = 0; SinUScoresRand = 0
    randind = sample(ind)
    for (k in 1:g) {
      CosUScoresk = 0 ; SinUScoresk = 0
      if (k==1)
      { low = 0 } else
        if (k > 1)
        { low = ndatcsum[k - 1] }
      for (j in 1:ndat[k]) {
        CosUScoresk[j] = CosUScores[randind[j+low]]
        SinUScoresk[j] = SinUScores[randind[j+low]] }
      CosUScoresRand = c(CosUScoresRand, CosUScoresk)
      SinUScoresRand = c(SinUScoresRand, SinUScoresk) }
    CosUScoresRand = CosUScoresRand[-1]
    SinUScoresRand = SinUScoresRand[-1]
    CSUScoresRand = list(CosUScoresRand, SinUScoresRand)
    WgRand = WgVal(CSUScoresRand, ndat, g)
    if (WgRand >= WgObs)
    { nxtrm = nxtrm+1 }
  }
  pval = nxtrm/(NR+1)
  return(pval)
}
