#' @importFrom stats complete.cases r2dtable pnorm
linear.by.linear <-
  function(x, y, u = NULL, v = NULL, nsims.mc = 100000, seed = NULL,
           do.asymp = FALSE, do.mc = TRUE) {
    stopifnot(is.factor(x), is.factor(y), length(x) == length(y),
              (is.vector(u) && length(u) == nlevels(x)) | is.null(u),
              (is.vector(v) && length(v) == nlevels(x)) | is.null(v),
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              length(seed) == 1 | is.null(seed),
              is.logical(do.asymp) == TRUE, is.logical(do.mc) == TRUE)

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(y))

    #unused arguments
    H0 <- NULL
    cont.corr <- NULL
    alternative <- NULL
    do.exact <- FALSE
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
    complete.cases.id <- complete.cases(x, y)
    x <- x[complete.cases.id] #remove missing cases
    y <- y[complete.cases.id] #remove missing cases
    x <- droplevels(x)
    y <- droplevels(y)
    if (is.null(u)){
      u <- 1:nlevels(x)
    }
    if (is.null(v)){
      v <- 1:nlevels(y)
    }
    tab <- table(x, y)
    tab.rtots <- rowSums(tab)
    tab.ctots <- colSums(tab)
    n <- sum(tab)
    score.vec <- as.vector(as.matrix(u) %*% t(as.matrix(v)))
    stat <- sum(tab * score.vec)

    #Monte Carlo p-value
    if (do.mc){
      pval.mc.stat <- stat
      if (!is.null(seed)){set.seed(seed)}
      pval.mc <- 0
      for (i in 1:nsims.mc){
        tab.tmp <- r2dtable(1, tab.rtots, tab.ctots)
        S.tmp <- sum(unlist(tab.tmp) * score.vec)
        if (S.tmp >= stat){
          pval.mc <- pval.mc + 1 / nsims.mc
        }
      }
    }

    #asymptotic p-value
    if (do.asymp){
      E.S <- sum(u * tab.rtots) * sum(v * tab.ctots) / n
      Var.S <- (sum(u ^ 2 * tab.rtots) - sum(u * tab.rtots) ^ 2) *
        (sum(v ^ 2 * tab.ctots) - sum(v * tab.ctots) ^ 2) / (n ^ 2 * (n - 1))
      Var.S <- (sum(u ^ 2 * tab.rtots) - (sum(u * tab.rtots) ^ 2 / n)) *
        (sum(v ^ 2 * tab.ctots) - (sum(v * tab.ctots) ^ 2 / n)) / (n - 1)
      pval.asymp.stat <- abs(stat - E.S) / sqrt(Var.S)
      pval.asymp <- pnorm(pval.asymp.stat, lower.tail = FALSE)
    }

    #check if message needed
    if (!do.mc && !do.asymp) {
      test.note <- paste("Neither Asymptotic nor Monte Carlo test requested")
    }

    #define hypotheses
    H0 <- paste0("H0: There is no linear relationship between the levels of ",
                 varname1, " and ", varname2, "\n",
                 "H1: There is a positive linear relationship between the ",
                 "levels of ", varname1, " and ", varname2, "\n")

    #add note regarding two-sided test
    test.note <- paste0("For a two-sided hypothesis test, double the reported ",
                        "one-sided p-value.\n", "For a one-sided test of a ",
                        "negative linear relationship, reverse the factor\n",
                        "level order for one of the variables.")

    #return
    result <- list(title = "Linear by linear association test",
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
