#' @importFrom stats complete.cases ks.test
ks.test.ANSM <-
  function(x, testdistn, ..., alternative = c("two.sided", "less", "greater"),
           max.exact.cases = 1000, do.asymp = FALSE, do.exact = TRUE) {
    stopifnot(is.vector(x), is.numeric(x), is.character(testdistn),
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE)
    alternative <- match.arg(alternative)

    #labels
    varname <- deparse(substitute(x))

    #default outputs
    cont.corr <- NULL
    CI.width <- NULL
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
    n <- length(x)

    #give asymptotic output if exact not possible
    if (do.exact && n > max.exact.cases){
      do.asymp <- TRUE
    }

    #create hypotheses
    H0 <- paste0("H0: distribution of ", varname, " matches that of ",
                 shQuote(testdistn))
    if (alternative == "two.sided"){
      H0 <- paste0(H0, "\nH1: distributions differ")
    }else if(alternative == "greater"){
      H0 <- paste0(H0, "\nH1: distribution of ", varname,
                   " lies above that of ", shQuote(testdistn))
    }else{
      H0 <- paste0(H0, "\nH1: distribution of ", varname,
                   " lies below that of ", shQuote(testdistn))
    }
    H0 <- paste0(H0, "\n")

    #exact p-value
    if(do.exact && n <= max.exact.cases){
      test.result <- ks.test(x = x, y = testdistn, ...,
                             alternative = alternative, exact = TRUE)
      if (test.result$exact){
        pval.exact.stat <- test.result$statistic[[1]]
        pval.exact <- test.result$p.value
      }
    }

    #asymptotic p-value
    if(do.asymp){
      test.result <- ks.test(x = x, y = testdistn, ...,
                             alternative = alternative, exact = FALSE)
      if (is.numeric(test.result$p.value)){
        pval.asymp.stat <- test.result$statistic[[1]]
        pval.asymp <- test.result$p.value
      }
    }

    #check if message needed
    if (!do.asymp && !do.exact) {
      test.note <- paste("Neither exact nor asymptotic test requested")
    }else if (do.exact && n > max.exact.cases) {
      test.note <- paste0("NOTE: Number of useful cases greater than current ",
                          "maximum allowed for exact calculations\nrequired for ",
                          "exact test (max.exact.cases = ",
                          sprintf("%1.0f", max.exact.cases), ")")
    }

    #return
    result <- list(title = paste0("Kolmogorov test - comparison with ",
                                  shQuote(testdistn)),
                   varname = varname, H0 = H0,
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
