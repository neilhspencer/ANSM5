#' @importFrom stats complete.cases pf pchisq
friedman <-
  function(y, groups, blocks, ...,
           max.exact.cases = 1000, do.asymp = FALSE, do.exact = TRUE) {
    stopifnot(is.vector(y), is.numeric(y), is.factor(groups), is.factor(blocks),
              length(y) == length(groups), length(groups) == length(blocks),
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE)

    #labels
    varname1 <- deparse(substitute(y))
    varname2 <- paste0(deparse(substitute(groups)), " (as groups) with ",
                       deparse(substitute(blocks)), " (as blocks)")

    #unused arguments
    alternative <- NULL
    cont.corr <- NULL
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
    y <- y[complete.cases(y)] #remove missing cases
    n <- length(y)
    rank.tab <- simplify2array(by(y, blocks, rank, simplify = TRUE))
    b <- nlevels(blocks)
    t <- nlevels(groups)
    Sr <- sum(rank.tab ** 2)
    St <- sum(rowSums(rank.tab) ** 2) / b
    C <- b * t * (t + 1) ** 2 / 4
    Tstat <- b * (t - 1) * (St - C) / (Sr - C)
    T1stat <- (b - 1) * (St - C) / (Sr - St)

    #check for ties
    tiesexist = !all(rank(y) == round(rank(y),0)) # TRUE if ties exist

    #give asymptotic output if exact not possible
    if (do.exact && n > max.exact.cases){
      do.asymp <- TRUE
    }

    #exact p-value
    if(do.exact && n <= max.exact.cases){

    }

    #asymptotic p-value
    if(do.asymp){
      if (!tiesexist){
        pval.asymp.stat <- Tstat
        pval.asmp <- pchisq(Tstat, t - 1, lower.tail = FALSE)
      }else{
        pval.asymp.stat <- T1stat
        pval.asymp <- pf(T1stat, t - 1, (b - 1) * (t - 1), lower.tail = FALSE)
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

    #create hypotheses
    H0 <- paste0("H0: distributions of ", varname1, " are identical\n")
    H0 <- paste0(H0, "H1: distributions differ")
    H0 <- paste0(H0, "\n")

    #return
    result <- list(title = "Friedman test",
                   varname1 = varname1, varname2 = varname2, H0 = H0,
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
