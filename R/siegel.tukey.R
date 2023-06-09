#' @importFrom stats complete.cases median
siegel.tukey <-
  function(x, y, H0 = NULL, alternative=c("two.sided", "less", "greater"),
           cont.corr = TRUE, CI.width = 0.95, max.exact.cases = 1000,
           do.asymp = FALSE, do.exact = TRUE, do.CI = TRUE) {
    stopifnot(is.vector(x), is.numeric(x), is.vector(y), is.numeric(y),
              ((is.numeric(H0) && length(H0) == 1) | is.null(H0)),
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              is.logical(cont.corr) == TRUE, CI.width > 0, CI.width < 1,
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE,
              is.logical(do.CI) == TRUE)
    alternative <- match.arg(alternative)

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(y))

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
    x <- x + (median(y) - median(x)) #equalise medians
    lenx <- length(x)
    if (!is.null(H0)) {
      xy <- c(x - H0, y)
      varname1 <- paste0(varname1, " - ", H0)
    }else{
      H0 <- 0
      xy <- c(x, y)
    }
    lenxy <- length(xy)
    #allocate ranks
    rankstoallocate <- c(1, rep(NA, lenxy - 1))
    slbin <- 2
    slbincount <- 0
    ns <- 1
    nl <- 0
    i <- 1
    repeat{
      i <- i + 1
      if (slbincount ==2){ #swap between smallest and largest and reset count
        slbincount <- 0
        if (slbin == 1){
          slbin <- 2
        }else{
          slbin <- 1
        }
      }
      #allocate next rank
      slbincount <- slbincount + 1
      if (slbin == 1){
        ns <- ns + 1
        rankstoallocate[ns] <- i
      }else{
        nl <- nl + 1
        rankstoallocate[lenxy - nl + 1] <- i
      }
      if (i == lenxy){break}
    }
    allocatedranks <- rankstoallocate[rank(xy)]
    allocatedranksx <- allocatedranks[1:lenx]
    allocatedranksy <- allocatedranks[(lenx + 1):lenxy]

    #carry out test
    res <- wilcoxon.mann.whitney(x = allocatedranksx, y = allocatedranksy,
                                 H0 = NULL, alternative=alternative,
                                 cont.corr = cont.corr, CI.width = CI.width,
                                 max.exact.cases = max.exact.cases,
                                 do.asymp = do.asymp, do.exact = do.exact,
                                 do.CI = do.CI)
    pval <- res$pval
    pval.stat <- res$pval.stat
    pval.note <- res$pval.note
    pval.exact <- res$pval.exact
    pval.exact.stat <- res$pval.exact.stat
    pval.exact.stat <- gsub("allocatedranksx", varname1, pval.exact.stat)
    pval.exact.stat <- gsub("allocatedranksy", varname2, pval.exact.stat)
    pval.exact.note <- res$pval.exact.note
    targetCIwidth <- res$CI.width
    actualCIwidth.exact <- res$actualCIwidth.exact
    CI.exact.lower <- res$CI.exact.lower
    CI.exact.upper <- res$CI.exact.upper
    CI.exact.note <- res$CI.exact.note
    pval.asymp <- res$pval.asymp
    pval.asymp.stat <- res$pval.asymp.stat
    pval.asymp.stat <- gsub("allocatedranksx", varname1, pval.asymp.stat)
    pval.asymp.stat <- gsub("allocatedranksy", varname2, pval.asymp.stat)
    pval.asymp.note <- res$pval.asymp.note
    CI.asymp.lower <- res$CI.asymp.lower
    CI.asymp.upper <- res$CI.asymp.upper
    CI.asymp.note <- res$CI.asymp.note
    pval.mc <- res$pval.mc
    pval.mc.stat <- res$pval.mc.stat
    nsims.mc <- res$nsims.mc
    pval.mc.note <- res$pval.mc.note
    CI.mc.lower <- res$CI.mc.lower
    CI.mc.upper <- res$CI.mc.upper
    CI.mc.note <- res$CI.mc.note
    test.note <- res$test.note

    #define hypotheses
    if (alternative == "two.sided"){
      H0 <- paste0("H0: samples are from the same population\n",
                   "H1: samples differ in location\n")
    }else if (alternative == "less"){
      H0 <- paste0("H0: samples are from the same population\n",
                   "H1: location of ", varname1, " is less than location of ",
                   varname2, "\n")
    }else if (alternative == "greater"){
      H0 <- paste0("H0: samples are from the same population\n",
                   "H1: location of ", varname1, " is greater than location of ",
                   varname2, "\n")
    }

    #return
    result <- list(title = "Siegel-Tukey test", varname1 = varname1,
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