#' @importFrom stats complete.cases chisq.test pchisq
chisq.test.ANSM <-
  function(x, y, cont.corr = TRUE, max.exact.cases = 10, nsims.mc = 1000000,
           seed = NULL, do.exact = TRUE, do.asymp = FALSE, do.mc = FALSE) {
    stopifnot(is.factor(x), is.factor(y),  nlevels(x) > 1, nlevels(y) > 1,
              length(x) == length(y), is.logical(cont.corr),
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              length(seed) == 1 | is.null(seed),
              is.logical(do.exact) == TRUE, is.logical(do.asymp) == TRUE,
              is.logical(do.mc) == TRUE)

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(y))

    #unused arguments
    H0 <- NULL
    alternative <- NULL
    CI.width <- NULL
    do.CI <- FALSE
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
    pval.mc <- NULL
    pval.mc.stat <- NULL
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
    complete.cases.id <- complete.cases(x, y)
    x <- x[complete.cases.id] #remove missing cases
    y <- y[complete.cases.id] #remove missing cases
    x <- droplevels(x)
    y <- droplevels(y)
    n <- length(x)
    suppressWarnings({
      chisq.test.out <- chisq.test(x, y, correct = cont.corr)
    })
    stat <- chisq.test.out$statistic[[1]]

    #give mc output if exact not possible
    if (do.exact && n > max.exact.cases){
      do.mc <- TRUE
    }

    #exact p-value
    if(do.exact && n <= max.exact.cases){
      pval.exact.stat <- stat
      permutations <- perms(n)
      n.perms <- dim(permutations)[1]
      pval.exact <- 0
      for (i in 1:n.perms){
        suppressWarnings({
          chisq.tmp <- chisq.test(x[permutations[i,]], y,
                                  correct = cont.corr)$statistic[[1]]
        })
        if (chisq.tmp >= pval.exact.stat){
          pval.exact <- pval.exact + 1 / n.perms
        }
      }
    }

    #Monte Carlo p-value
    if (do.mc){
      pval.mc.stat <- stat
      if (!is.null(seed)){set.seed(seed)}
      suppressWarnings({
        pval.mc <- chisq.test(x, y, correct = cont.corr,
                              simulate.p.value = TRUE, B = nsims.mc)$p.value
      })
    }

    #asymptotic p-value
    if (do.asymp){
      pval.asymp.stat <- stat
      pval.asymp <- chisq.test.out$p.value
    }

    #check if message needed
    if (!do.exact && !do.mc && !do.asymp) {
      test.note <- paste("Neither exact, asymptotic nor Monte Carlo test requested")
    }else if (do.exact && n > max.exact.cases) {
      test.note <- paste0("NOTE: Number of useful cases greater than current ",
                          "maximum allowed for exact calculations\nrequired for ",
                          "exact test (max.exact.cases = ",
                          sprintf("%1.0f", max.exact.cases), ") so Monte ",
                          "Carlo p-value given")
    }

    #define hypotheses
    H0 <- paste0("H0: ", varname1, " and ", varname2, " are independent\n",
                 "H1: ", varname1, " and ", varname2, " are not independent\n")

    #return
    result <- list(title = "Chi-squared test", varname1 = varname1,
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
