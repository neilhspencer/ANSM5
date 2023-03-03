binom.CI <-
  function(r, n, CI.width = 0.95, do.asymp = FALSE, do.exact = TRUE,
           do.CI = TRUE) {
    stopifnot(is.numeric(r), length(r) == 1,
              is.numeric(n), length(n) == 1, r < n,
              CI.width > 0, CI.width < 1,
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE,
              is.logical(do.CI) == TRUE)

    #labels
    varname <- paste0("r = ", r, ", n = ", n)

    #default outputs
    H0 <- NULL
    alternative <- NULL
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

    #exact CI
    if (do.exact && do.CI){
      CI.exact.lower <- NULL
      p <- 1
      repeat{
        if (1 - pbinom(r - 1, n, p) < (1 - CI.width) / 2) {
          CI.exact.lower <- p
          break
        }
        p <- p - 0.00001
      }
      CI.exact.upper <- NULL
      p <- 0
      repeat{
        if (pbinom(r, n, p) < (1 - CI.width) / 2) {
          CI.exact.upper <- p
          break
        }
        p <- p + 0.00001
      }
      actualCIwidth.exact <- 1 - pbinom(r, n, CI.exact.upper) - (1 - pbinom(r - 1, n, CI.exact.lower))
      if (is.null(CI.exact.lower) | is.null(CI.exact.upper) | is.null(actualCIwidth.exact)){
        actualCIwidth.exact <- NULL
        CI.exact.lower <- NULL
        CI.exact.upper <- NULL
      }
    }

    #asymptotic CI
    if (do.asymp && do.CI){
      phat <- r / n
      z1 <- qnorm((1 - CI.width) / 2)
      z2 <- qnorm(1 - (1 - CI.width) / 2)
      CI.asymp.lower <- phat + z1 * sqrt(phat * (1 - phat) / n)
      CI.asymp.upper <- phat + z2 * sqrt(phat * (1 - phat) / n)
      if (n < 20){
        CI.asymp.note <- paste("(WARNING: n is less than 20 so asymptotic",
                               "CI is not recommended)")
      }
    }

    #check if message needed
    if ((!do.asymp && !do.exact) | !do.CI) {
      test.note <- paste("Neither exact nor asymptotic confidence interval ",
                         "requested")
    }

    #return
    result <- list(title = "Binomial confidence interval", varname = varname,
                   H0 = H0, alternative = alternative, cont.corr = cont.corr,
                   pval = pval, pval.stat = pval.stat, pval.note = pval.note,
                   pval.exact = pval.exact, pval.exact.stat = pval.exact.stat,
                   pval.exact.note = pval.exact.note, targetCIwidth = CI.width,
                   actualCIwidth.exact = actualCIwidth.exact,
                   CI.exact.lower = CI.exact.lower,
                   CI.exact.upper = CI.exact.upper, CI.exact.note = CI.exact.note,
                   pval.asymp = pval.asymp, pval.asymp.stat = pval.asymp.stat,
                   pval.asymp.note = pval.asymp.note,
                   CI.asymp.lower = CI.asymp.lower,
                   CI.asymp.upper = CI.asymp.upper, CI.asymp.note = CI.asymp.note,
                   pval.mc = pval.mc, nsims.mc = nsims.mc,
                   pval.mc.note = pval.mc.note,
                   CI.mc.lower = CI.mc.lower, CI.mc.upper = CI.mc.upper,
                   CI.mc.note = CI.mc.note,
                   test.note = test.note)
    class(result) <- "ANSMtest"
    return(result)
  }
