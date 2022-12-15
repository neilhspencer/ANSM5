wilcoxon.signed.rank.test <-
  function(x, H0, alternative=c("two.sided", "less", "greater"),
           cont.corr = TRUE, CI.width = 0.95, max.exact.cases = 1000,
           do.approx = TRUE, do.exact = TRUE, do.CI = TRUE) {
  stopifnot(is.vector(x), is.numeric(x), is.numeric(H0), length(H0) == 1,
            is.logical(cont.corr) == TRUE, CI.width > 0, CI.width < 1,
            is.logical(do.approx) == TRUE, is.logical(do.exact) == TRUE,
            is.logical(do.CI) == TRUE)
  alternative <- match.arg(alternative)

  #labels
  varname <- deparse(substitute(x))

  #defaults
  pval <- NULL
  pval.stat <- NULL
  pval.note <- NULL
  pval.approx <- NULL
  pval.approx.stat <- NULL
  pval.approx.note <- NULL
  pval.exact <- NULL
  pval.exact.stat <- NULL
  pval.exact.note <- NULL
  actualCIwidth <- NULL
  CI.lower <- NULL
  CI.upper <- NULL
  CI.note <- NULL
  test.note <- NULL

  #statistics
  x <- x[complete.cases(x)] #remove missing cases
  x <- x[x != H0] #remove cases equal to H0
  ranksumplus <- sum(rank(abs(x - H0))[sign(x - H0) == 1])
  ranksumminus <- sum(rank(abs(x - H0))[sign(x - H0) == -1])
  n <- length(x)
  if (n < max.exact.cases){
    permsums <- rep(0,sum(seq(1, n)) + 1)
    permsums[1] <- 1
    for (i in 1:n){
      permsumsnow <- permsums
      for (j in 1:length(permsums)){
        if (permsumsnow[j] > 0){
          permsums[(j - 1) + i + 1] <- permsums[(j - 1) + i + 1] + permsumsnow[j]
        }
      }
    }
    permsums <- data.frame(0:(length(permsums) - 1), permsums)
  }

  #approx p-value (with/without continuity correction)
  if (do.approx){
    S <- min(ranksumminus, ranksumplus)
    pval.approx.stat <- ((S + 0.5 * (cont.corr == TRUE)) - n * (n + 1) / 4) /
      sqrt(n * (n + 1) * (2 * n + 1) / 24)
    pval.approx.less <-
      pnorm(((ranksumminus + (0.5 - (ranksumminus > ranksumplus)) *
                (cont.corr == TRUE)) - n * (n + 1) / 4) /
              sqrt(n * (n + 1) * (2 * n + 1) / 24), lower.tail = FALSE)
    pval.approx.greater <-
      pnorm(((ranksumplus + (0.5 - (ranksumminus < ranksumplus)) *
                (cont.corr == TRUE)) - n * (n + 1) / 4) /
              sqrt(n * (n + 1) * (2 * n + 1) / 24), lower.tail = FALSE)
    if (alternative=="two.sided"){
      pval.approx <- min(pval.approx.less, pval.approx.greater) * 2
    }else if (alternative == "less"){
      pval.approx <- pval.approx.less
    }else if (alternative == "greater"){
      pval.approx <- pval.approx.greater
    }
    if (n < 12){
      pval.approx.note <-
        pval.approx.note <- paste("(WARNING: n is less than 12 so",
        "approximation is not recommended)")
    }
  }

  #exact p-value
  if (do.exact && max(table(x)) == 1 && n <= max.exact.cases){
    pval.exact.stat <- min(ranksumminus, ranksumplus)
    pval.exact.less <-
      sum(permsums[permsums[, 1] >= ranksumminus, 2]) / sum(permsums[, 2])
    pval.exact.greater <-
      sum(permsums[permsplus[, 1] >= ranksumplus, 2]) / sum(permsplus[, 2])
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
        sum(permsplus[permsplus[, 1] <= i, 2]) / sum(permsplus[, 2]) * 2
      if (pval.tmp > (1 - CI.width)) {break}
      i <- i + 1
    }
    CI.lower <- sort(Walsh.averages, decreasing = FALSE)[i]
    CI.upper <- sort(Walsh.averages, decreasing = TRUE)[i]
    actualCIwidth <- 1 - sum(permsplus[permsplus[, 1] <= i - 1, 2]) /
      sum(permsplus[, 2]) * 2
    if (is.null(CI.lower) | is.null(CI.upper) | is.null(actualCIwidth)){
      actualCIwidth <- NULL
      CI.lower <- NULL
      CI.upper <- NULL
    }
  }

  #check if message needed
  if (!do.approx && !do.exact && !do.CI) {
    test.note <- paste("Neither exact test, nor approximation test nor",
                       "confidence interval requested")
  }else if (max(table(x)) > 1){
    affected <- NULL
    if (do.exact && do.CI){
      affected <- "exact test and confidence interval"
    }else if (do.exact) {
      affected <- "exact test"
    }else if (do.CI){
      affected <- "confidence interval"
    }
    if (do.approx && !is.null(affected)){
      test.note <- paste("NOTE: Ties exist in data so", affected, "not",
                          "calculated\nand mid-ranks used for approximation",
                          "test")
    }else if (!do.approx && !is.null(affected)){
      test.note <- paste("NOTE: Ties exist in data so", affected, "not",
                          "calculated")
    }else if (do.approx && is.null (affected)){
      test.note <- paste("NOTE: Ties exist in data so mid-ranks used for",
                          "approximation test")
    }
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
                           "for ", affected, " (", max.exact.cases, ")")
    }
  }

  #return
  result <- list(title = "Wilcoxon signed-rank test", varname = varname,
                 H0 = H0, alternative = alternative, cont.corr = cont.corr,
                 pval = pval, pval.stat = pval.stat, pval.note = pval.note,
                 pval.approx = pval.approx, pval.approx.stat = pval.approx.stat,
                 pval.approx.note = pval.approx.note, pval.exact = pval.exact,
                 pval.exact.stat = pval.exact.stat,
                 pval.exact.note = pval.exact.note, targetCIwidth = CI.width,
                 actualCIwidth = actualCIwidth, CI.lower = CI.lower,
                 CI.upper = CI.upper, CI.note = CI.note, test.note = test.note)
  class(result) <- "ANSMtest"
  return(result)
}
