#' @importFrom stats complete.cases
cox.stuart <-
  function(x, cont.corr = TRUE, max.exact.cases = 10000000, do.asymp = FALSE,
           do.exact = TRUE) {
    stopifnot(is.vector(x), is.numeric(x), is.logical(cont.corr) == TRUE,
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE)

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
    pval.mc <- NULL
    nsims.mc <- NULL
    pval.mc.note <- NULL
    CI.width <- NULL
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
    if (length(x) %% 2 == 1){ #odd so remove middle value
      x <- c(x[1:floor(length(x)/2)], x[(ceiling(length(x)/2) +  1):length(x)])
    }
    diffs <- x[1:(length(x)/2)] - x[(length(x)/2 + 1):length(x)]

    #give asymptotic output if exact not possible
    if (do.exact && length(x) > max.exact.cases){
      do.asymp <- TRUE
    }

    #carry out test
    H0 <- 0
    alternative <- "two.sided"
    do.CI <- FALSE
    result <- sign.test(diffs, H0 = H0, alternative = alternative,
                        cont.corr = cont.corr,
                        max.exact.cases = max.exact.cases, do.asymp = do.asymp,
                        do.exact = do.exact, do.CI = do.CI)
    H0 <- paste0("H0: no monotonic trend\n",
                 "H1: montonic trend exists\n")
    pval.exact <- result$pval.exact
    pval.asymp <- result$pval.asymp
    pval.asymp.note <- result$pval.asymp.note
    test.note <- result$test.note

    #return
    result <- list(title = "Cox-Stuart test", varname = varname,
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
