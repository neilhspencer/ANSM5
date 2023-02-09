wilcoxon.signed.rank.test <-
  function(x, H0, alternative=c("two.sided", "less", "greater"),
           cont.corr = TRUE, CI.width = 0.95, max.exact.cases = 1000,
           do.asymp = TRUE, do.exact = TRUE, do.CI = TRUE) {
  stopifnot(is.vector(x), is.numeric(x), is.numeric(H0), length(H0) == 1,
            is.numeric(max.exact.cases), length(max.exact.cases) == 1,
            is.logical(cont.corr) == TRUE, CI.width > 0, CI.width < 1,
            is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE,
            is.logical(do.CI) == TRUE)
  alternative <- match.arg(alternative)

  #labels
  varname <- deparse(substitute(x))

  #default outputs
  pval <- NULL
  pval.stat <- NULL
  pval.note <- NULL
  pval.asymp <- NULL
  pval.asymp.stat <- NULL
  pval.asymp.note <- NULL
  pval.exact <- NULL
  pval.exact.stat <- NULL
  pval.exact.note <- NULL
  actualCIwidth <- NULL
  CI.lower <- NULL
  CI.upper <- NULL
  CI.note <- NULL
  test.note <- NULL

  #prepare
  x <- x[complete.cases(x)] #remove missing cases
  x <- x[x != H0] #remove cases equal to H0
  n <- length(x)
  s <- rank(abs(x - H0), ties.method = "average")
  multiplier = 2 - all(s == round(s,0)) # multiplier of 2 if average ranks, otherwise multiplier of 1
  ranksumplus <- sum(s[sign(x - H0) == 1]) * multiplier
  ranksumminus <- sum(s[sign(x - H0) == -1]) * multiplier

  #statistics
  if (do.exact && n <= max.exact.cases){
    permfrom <- s * multiplier
    permfrom <- sort(permfrom)
    permsums <- rep(0,sum(permfrom) + 1)
    permsums[1] <- 1
    current.max <- 0
    for (i in 1:length(permfrom)){
      permsumsnow <- permsums
      to.add <- permfrom[i]
      j <- 1:(current.max + 1)
      k <- (j - 1) + to.add + 1
      gtzero <- permsumsnow[j] > 0
      permsums[k][gtzero] <- permsums[k][gtzero] + permsumsnow[j][gtzero]
      current.max <- current.max + to.add
    }
    permsums <- data.frame(0:(length(permsums) - 1), permsums)
  }

  #asymptotic p-value (with/without continuity correction)
  if (do.asymp){
    S <- min(ranksumminus, ranksumplus) / multiplier
    #with ties
    if (multiplier == 2){
      pval.asymp.stat <- (S - 0.5 * sum(abs(s))) / (0.5 * sqrt(sum(s ** 2)))
      pval.asymp.less <-
        pnorm((ranksumminus / 2 - 0.5 * sum(abs(s))) / (0.5 * sqrt(sum(s ** 2))),
              lower.tail = FALSE)
      pval.asymp.greater <-
        pnorm((ranksumplus / 2 - 0.5 * sum(abs(s))) / (0.5 * sqrt(sum(s ** 2))),
              lower.tail = FALSE)
    }else{
    #without ties
      pval.asymp.stat <- ((S + 0.5 * (cont.corr == TRUE)) - n * (n + 1) / 4) /
        sqrt(n * (n + 1) * (2 * n + 1) / 24)
      pval.asymp.less <-
        pnorm(((ranksumminus + (0.5 - (ranksumminus > ranksumplus)) *
                  (cont.corr == TRUE)) - n * (n + 1) / 4) /
                sqrt(n * (n + 1) * (2 * n + 1) / 24), lower.tail = FALSE)
      pval.asymp.greater <-
        pnorm(((ranksumplus + (0.5 - (ranksumminus < ranksumplus)) *
                  (cont.corr == TRUE)) - n * (n + 1) / 4) /
                sqrt(n * (n + 1) * (2 * n + 1) / 24), lower.tail = FALSE)
    }
    if (alternative=="two.sided"){
      pval.asymp <- min(pval.asymp.less, pval.asymp.greater) * 2
    }else if (alternative == "less"){
      pval.asymp <- pval.asymp.less
    }else if (alternative == "greater"){
      pval.asymp <- pval.asymp.greater
    }
    if (n < 20){
      pval.asymp.note <-
        pval.asymp.note <- paste("(WARNING: n is less than 20 so asymptotic",
                                 "test is not recommended)")
    }
  }

  #exact p-value
  if (do.exact && n <= max.exact.cases){
    pval.exact.stat <- min(ranksumminus, ranksumplus)
    pval.exact.less <-
      sum(permsums[permsums[, 1] >= ranksumminus, 2]) / sum(permsums[, 2])
    pval.exact.greater <-
      sum(permsums[permsums[, 1] >= ranksumplus, 2]) / sum(permsums[, 2])
    if (alternative=="two.sided"){
      pval.exact <- min(pval.exact.less, pval.exact.greater) * 2
    }else if (alternative == "less"){
      pval.exact <- pval.exact.less
    }else if (alternative == "greater"){
      pval.exact <- pval.exact.greater
    }
  }

  #CI
  if (do.CI && n <= max.exact.cases){
    Walsh.averages <- NULL
    for (i in 1:length(x)){
      for (j in i:length(x)){
        Walsh.averages <- c(Walsh.averages, (x[i] + x[j]) / 2)
      }
    }
    i <- 0
    repeat{
      pval.tmp <-
        sum(permsums[permsums[, 1] <= i, 2]) / sum(permsums[, 2]) * 2
      if (pval.tmp > (1 - CI.width)) {break}
      i <- i + 1
    }
    CI.lower <- sort(Walsh.averages, decreasing = FALSE)[i]
    CI.upper <- sort(Walsh.averages, decreasing = TRUE)[i]
    actualCIwidth <- 1 - sum(permsums[permsums[, 1] <= i - 1, 2]) /
      sum(permsums[, 2]) * 2
    if (is.null(CI.lower) | is.null(CI.upper) | is.null(actualCIwidth)){
      actualCIwidth <- NULL
      CI.lower <- NULL
      CI.upper <- NULL
    }
  }

  #check if message needed
  if (!do.asymp && !do.exact && !do.CI) {
    test.note <- paste("Neither exact test, nor asymptotic test nor",
                       "confidence interval requested")
  }else if (multiplier == 2 && do.asymp){
      test.note <- paste("NOTE: Ties exist in data so mid-ranks used for",
                          "asymptotic test")
  }else if (n > max.exact.cases) {
    affected <- NULL
    if (do.exact && do.CI){
      affected <- "exact test and confidence interval"
    }else if (do.exact) {
      affected <- "exact test"
    }else if (do.CI){
      affected <- "confidence interval"
    }
    if (!is.null(affected)){
      test.note <- paste0("NOTE: Number of useful cases greater than current ",
                           "maximum allowed for exact\ncalculations required ",
                           "for ", affected, " (max.exact.cases = ",
                          max.exact.cases, ")")
    }
  }

  #return
  result <- list(title = "Wilcoxon signed-rank test", varname = varname,
                 H0 = H0, alternative = alternative, cont.corr = cont.corr,
                 pval = pval, pval.stat = pval.stat, pval.note = pval.note,
                 pval.asymp = pval.asymp, pval.asymp.stat = pval.asymp.stat,
                 pval.asymp.note = pval.asymp.note, pval.exact = pval.exact,
                 pval.exact.stat = pval.exact.stat,
                 pval.exact.note = pval.exact.note, targetCIwidth = CI.width,
                 actualCIwidth = actualCIwidth, CI.lower = CI.lower,
                 CI.upper = CI.upper, CI.note = CI.note, test.note = test.note)
  class(result) <- "ANSMtest"
  return(result)
}
