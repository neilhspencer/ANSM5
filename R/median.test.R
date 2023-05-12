#' @importFrom stats complete.cases median
median.test <-
  function(x, y, H0 = NULL, alternative=c("two.sided", "less", "greater"),
           CI.width = 0.95, max.exact.cases = 1000,
           do.exact = TRUE, do.CI = TRUE) {
    stopifnot(is.vector(x), is.numeric(x), is.vector(y), is.numeric(y),
              ((is.numeric(H0) && length(H0) == 1) | is.null(H0)),
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              CI.width > 0, CI.width < 1, is.logical(do.exact) == TRUE,
              is.logical(do.CI) == TRUE)
    alternative <- match.arg(alternative)

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(y))

    #default outputs
    do.asymp <- FALSE
    cont.corr <- NULL
    pval <- NULL
    pval.stat <- NULL
    pval.note <- NULL
    pval.asymp <- NULL
    pval.asymp.stat <- NULL
    pval.asymp.note <- NULL
    pval.exact <- NULL
    pval.exact.stat <- NULL
    pval.exact.note <- NULL
    pval.mc <- NULL
    pval.mc.stat <- NULL
    nsims.mc <- NULL
    pval.mc.note <- NULL
    actualCIwidth.exact <- NULL
    CI.exact.lower <- NULL
    CI.exact.upper <- NULL
    CI.exact.note <- NULL
    CI.asymp.lower <- NULL
    CI.asymp.upper <- NULL
    CI.asymp.note <- NULL
    CI.mc.lower <- NULL
    CI.mc.upper <- NULL
    CI.mc.note <- NULL
    test.note <- NULL

    #prepare
    x <- x[complete.cases(x)] #remove missing cases
    y <- y[complete.cases(y)] #remove missing cases
    if (!is.null(H0)) {
      xy <- c(x - H0, y)
      varname1 <- paste0(varname1, " - ", H0)
    }else{
      H0 <- 0
      xy <- c(x, y)
    }
    med <-median(xy)
    x.gt <- sum(x > med)
    y.gt <- sum(y > med)
    nx <- x.gt + sum(x < med) #ignoring x == med
    ny <- y.gt + sum(y < med) #ignoring y == med
    propdiff <- x.gt / nx - y.gt / ny
    pbase <- factorial(nx) * factorial(ny) * factorial(x.gt + y.gt) *
      factorial(nx + ny - x.gt - y.gt) / factorial(nx + ny)

    #exact p-value
    if (do.exact && nx + ny <= max.exact.cases){
      pval.exact <- 0
      for (x.gt.2 in 0:nx){
        y.gt.2 <- x.gt + y.gt - x.gt.2
        propdiffi <- x.gt.2 / nx - y.gt.2 / ny
        if (alternative == "less" && propdiffi <= propdiff){
          pval.exact <-
            pval.exact + pbase /
            (factorial(x.gt.2) * factorial(y.gt.2) * factorial(nx - x.gt.2) *
               factorial(ny - y.gt.2))
        }else if (alternative == "greater" && propdiffi >= propdiff){
          pval.exact <-
            pval.exact + pbase /
            (factorial(x.gt.2) * factorial(y.gt.2) * factorial(nx - x.gt.2) *
               factorial(ny - y.gt.2))
        }else if (alternative == "two.sided" &&
                  (abs(propdiffi) >= abs(propdiff) |
                   abs(abs(propdiffi) - abs(propdiff)) <
                   .Machine$double.eps ^ 0.5)){
          pval.exact <-
            pval.exact + pbase /
            (factorial(x.gt.2) * factorial(y.gt.2) * factorial(nx - x.gt.2) *
               factorial(ny - y.gt.2))
        }
      }
    }

    #exact CI
    if (do.CI && do.exact && nx + ny <= max.exact.cases){
      #prepare
      if (median(x) > median(y)){
        s1 <- x
        s2 <- y
      }else{
        s1 <- y
        s2 <- x
      }
      med <-median(c(s1, s2))
      s1.gt <- sum(s1 > med)
      s2.gt <- sum(s2 > med)
      ns1 <- s1.gt + sum(s1 < med) #ignoring s1 == med
      ns2 <- s2.gt + sum(s2 < med) #ignoring s2 == med
      #get increment
      increment <- min(abs(diff(sort(unique(c(s1, s2))))))
      #get range to try
      largest <- max(c(abs(s1), abs(s2)))
      #Lower limit
      for (i in seq(-largest, largest, increment)){
        s1a <- s1 + i
        median_tmp <- median(c(s1a, s2))
        a <- sum(s1a > median_tmp)
        b <- ns1 - a
        c <- s1.gt + s2.gt - a
        d <- ns2 - c
        mat <- matrix(c(a, b, c, d), nrow = 2, ncol = 2, byrow = TRUE)
        pval.tmp <- fisher.test(mat)$p.value
        if (pval.tmp > (1 - CI.width)){break}
        CI.exact.lower <- i
        pval.lower <- pval.tmp / 2
      }
      #Upper limit
      for (i in seq(largest, CI.exact.lower + increment, -increment)){
        s1a <- s1 + i
        median_tmp <- median(c(s1a, s2))
        a <- sum(s1a > median_tmp)
        b <- ns1 - a
        c <- s1.gt + s2.gt - a
        d <- ns2 - c
        mat <- matrix(c(a, b, c, d), nrow = 2, ncol = 2, byrow = TRUE)
        pval.tmp <- fisher.test(mat)$p.value
        if (pval.tmp > (1 - CI.width)){
          CI.exact.upper <- i
          break
        }
        pval.upper <- pval.tmp / 2
      }
      #Actual CI width
      actualCIwidth.exact <- 1 - pval.lower - pval.upper
    }

    #check if message needed
    if (!do.asymp && !do.exact) {
      test.note <- paste("Neither exact nor asymptotic test/confidence ",
                         "interval requested")
    }else if (nx + ny > max.exact.cases) {
      affected <- NULL
      if (do.exact && do.CI){
        affected <- "exact test and confidence interval"
      }else if (do.exact) {
        affected <- "exact test"
      }
      if (!is.null(affected)){
        test.note <- paste0("NOTE: Number of useful cases greater than ",
                            "current maximum allowed for exact\ncalculations ",
                            "required for ", affected, " (max.exact.cases = ",
                            sprintf("%1.0f", max.exact.cases), ")")
      }
    }

    #define hypotheses
    if (alternative == "two.sided"){
      H0 <- paste0("H0: samples are from populations with the same median\n",
                   "H1: samples are from populations with different medians\n")
    }else if (alternative == "less"){
      H0 <- paste0("H0: samples are from populations with the same median\n",
                   "H1: median of ", varname1, " is less than median of ",
                   varname2, "\n")
    }else if (alternative == "greater"){
      H0 <- paste0("H0: samples are from populations with the same median\n",
                   "H1: median of ", varname1, " is greater than median of ",
                   varname2, "\n")
    }

    #create title
    title <- "Median test"
    if (do.exact && !is.null(pval.exact)){
      title <- "Median test (Fisher's Exact Test)"
    }

    #return
    result <- list(title = title, varname1 = varname1,
                   varname2 = varname2, H0 = H0,
                   alternative = alternative, cont.corr = cont.corr, pval = pval,
                   pval.stat = pval.stat, pval.note = pval.note,
                   pval.exact = pval.exact, pval.exact.stat = pval.exact.stat,
                   pval.exact.note = pval.exact.note, targetCIwidth = CI.width,
                   actualCIwidth.exact = actualCIwidth.exact,
                   CI.exact.lower = CI.exact.lower,
                   CI.exact.upper = CI.exact.upper, CI.exact.note = CI.exact.note,
                   pval.asymp = pval.asymp, pval.asymp.stat = pval.asymp.stat,
                   pval.asymp.note = pval.asymp.note,
                   CI.asymp.lower = CI.asymp.lower,
                   CI.asymp.upper = CI.asymp.upper, CI.asymp.note = CI.asymp.note,
                   pval.mc = pval.mc, pval.mc.stat = pval.mc.stat,
                   nsims.mc = nsims.mc, pval.mc.note = pval.mc.note,
                   CI.mc.lower = CI.mc.lower, CI.mc.upper = CI.mc.upper,
                   CI.mc.note = CI.mc.note,
                   test.note = test.note)
    class(result) <- "ANSMtest"
    return(result)
  }
