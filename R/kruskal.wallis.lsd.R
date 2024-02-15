#' @importFrom stats complete.cases pt
kruskal.wallis.lsd <-
  function(x, g, ids) {
    stopifnot(is.vector(x), is.numeric(x), is.factor(g), length(x) == length(g),
              length(x[complete.cases(x)]) == length(g[complete.cases(g)]),
              length(ids) == 2, all(ids %in% levels(g)))

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(g))

    #unused arguments
    alternative <- NULL
    cont.corr <- NULL
    CI.width <- NULL
    do.CI <- FALSE
    max.exact.cases <- NULL
    nsims.mc <- NULL
    do.asymp <- TRUE
    do.exact <- FALSE
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
    x <- x[complete.cases(x)] #remove missing cases
    x <- round(x, -floor(log10(sqrt(.Machine$double.eps)))) #handle floating point issues
    g <- g[complete.cases(g)] #remove missing cases
    n <- length(x)
    rank.x <- rank(x)
    table_g <- table(g)
    divide_by <- (n * (n + 1))
    subtract <- 3 * (n + 1)
    c <- (n * ((n + 1) ** 2)) / 4
    sr <- sum(rank.x ** 2)
    t <- nlevels(g)
    mean.rank1 <- mean(rank.x[g == ids[1]])
    mean.rank2 <- mean(rank.x[g == ids[2]])
    n1 <- sum(g == ids[1])
    n2 <- sum(g == ids[2])

    #check for ties
    tiesexist = !all(rank.x == round(rank.x,0)) # TRUE if ties exist

    #calculate test statistic
    if (!tiesexist){
      T <- (12 * sum(by(rank.x, g, sum) ** 2 / table_g)) /
        divide_by - subtract
    }else{
      T <- ((n - 1) * (sum(by(rank.x, g, sum) ** 2 / table_g) - c)) / (sr - c)
    }

    #calculate lsd test statistic and asymptotic p-value
    pval.asymp.stat <- abs(mean.rank1 - mean.rank2) /
      sqrt((sr - c) * (n - 1 - T) * (n1 + n2) / (n1 * n2 * (n - t) * (n - 1)))
    pval.asymp <- pt(pval.asymp.stat, n - t, lower.tail = FALSE)

    #check if message needed
    if (tiesexist){
      test.note <- paste0(test.note, "NOTE: Ties exist in data so mid-ranks ",
                          "used")
    }

    #define hypotheses
    H0 <- paste0("H0: ", ids[1], " and ", ids[2], " are from the same population\n",
                 "H1: samples differ in location\n")

    #return
    result <- list(title = paste0("Least Significant Differences test after ",
                                  "Kruskal-Wallis test"),
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
