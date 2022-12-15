sign.test <- function(x, H0, alternative=c("two.sided", "less", "greater"),
                      cont.corr = TRUE, CI.width = 0.95,
                      max.exact.cases = 1000000, do.approx = TRUE,
                      do.exact = TRUE, do.CI = TRUE) {
  stopifnot(is.vector(x), is.numeric(x), is.numeric(H0), length(H0) == 1,
            is.numeric(max.exact.cases), length(max.exact.cases) == 1,
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
  pluses <- sum(x > H0)
  n <- length(x)

  #approx p-value (with/without continuity correction)
  if (do.approx){
    if (alternative=="two.sided"){
      pval.approx <- pnorm((pluses - n * 0.5 + (0.5 - (pluses > n * 0.5)) *
                              (cont.corr == TRUE))/(0.5 * sqrt(n)),
                           lower.tail = pluses < n * 0.5) * 2
    }else if (alternative == "less"){
      pval.approx <- pnorm((pluses - n * 0.5 + (0.5 - (pluses > n * 0.5)) *
                              (cont.corr == TRUE))/(0.5 * sqrt(n)),
                           lower.tail = TRUE)
    }else if (alternative == "greater"){
      pval.approx <- pnorm((pluses - n * 0.5 + (0.5 - (pluses > n * 0.5)) *
                              (cont.corr == TRUE))/(0.5 * sqrt(n)),
                           lower.tail = FALSE)
    }
    if (pluses < 5 | n - pluses < 5){
      pval.approx.note <- paste("(WARNING: n * p and/or n * (1 - p) are less",
                                "than 5 so approximation is not recommended)")
    }
  }

  #exact p-value
  if (do.exact && n <= max.exact.cases){
    pval.exact.less <- pbinom(pluses, n, 0.5)
    pval.exact.greater <- pbinom(n-pluses-1, n, 0.5)
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
    lower <- 0
    lower.cumu <- 0
    repeat{
      if (lower.cumu + dbinom(lower, n, 0.5) > (1 - CI.width) / 2) {break}
      lower.cumu <- lower.cumu + dbinom(lower, n, 0.5)
      lower <- lower + 1
    }
    if (lower == 0){
      CI.lower = Inf
    }else{
      CI.lower <- sort(x)[lower]
    }
    upper <- n
    upper.cumu <- 0
    repeat{
      if (upper.cumu + dbinom(upper, n, 0.5) > (1 - CI.width) / 2) {break}
      upper.cumu <- upper.cumu + dbinom(upper, n, 0.5)
      upper <- upper - 1
    }
    if (upper == n){
      CI.upper = Inf
    }else{
      CI.upper <- sort(x)[upper+1]
    }
    actualCIwidth <- 1 - lower.cumu - upper.cumu
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
  }else if ((do.CI | do.exact) && n > max.exact.cases) {
    test.note <- paste0("NOTE: Number of useful cases greater than current ",
                        "maximum allowed for exact calculations\nrequired for ",
                        "exact test or confidence interval (", max.exact.cases,
                        ")")
  }

  #return
  result <- list(title = "Sign test", varname = varname, H0 = H0,
                 alternative = alternative, cont.corr = cont.corr, pval = pval,
                 pval.stat = pval.stat, pval.note = pval.note,
                 pval.approx = pval.approx, pval.approx.stat = pval.approx.stat,
                 pval.approx.note = pval.approx.note, pval.exact = pval.exact,
                 pval.exact.stat = pval.exact.stat,
                 pval.exact.note = pval.exact.note, targetCIwidth = CI.width,
                 actualCIwidth = actualCIwidth, CI.lower = CI.lower,
                 CI.upper = CI.upper, CI.note = CI.note, test.note = test.note)
  class(result) <- "ANSMtest"
  return(result)
}
