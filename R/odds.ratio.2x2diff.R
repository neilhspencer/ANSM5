#' @importFrom stats complete.cases r2dtable quantile pnorm qnorm
odds.ratio.2x2diff <-
  function(x, y, z, CI.width = 0.95, nsims.mc = 100000, seed = NULL,
           do.asymp = FALSE, do.mc = TRUE, do.CI = TRUE) {
    stopifnot(is.factor(x), is.factor(y), is.factor(z),
              nlevels(x) == 2, nlevels(y) == 2, nlevels(z) == 2,
              length(x) == length(y), length(y) == length(z),
              length(CI.width) == 1, CI.width > 0, CI.width < 1,
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              length(seed) == 1 | is.null(seed),
              is.logical(do.asymp) == TRUE, is.logical(do.mc) == TRUE,
              is.logical(do.CI) == TRUE)

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(y))
    varname3 <- deparse(substitute(z))

    #unused arguments
    H0 <- NULL
    cont.corr <- NULL
    alternative <- NULL
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
    complete.cases.id <- complete.cases(x, y, z)
    x <- x[complete.cases.id] #remove missing cases
    y <- y[complete.cases.id] #remove missing cases
    z <- z[complete.cases.id] #remove missing cases
    x <- droplevels(x)
    y <- droplevels(y)
    z <- droplevels(z)
    tab.z1 <- table(x[z == levels(z)[1]], y[z == levels(z)[1]])
    rtot.z1 <- rowSums(tab.z1)
    ctot.z1 <- colSums(tab.z1)
    phi.star.1 <- log((tab.z1[1, 1] * tab.z1[2, 2]) /
                        (tab.z1[1, 2] * tab.z1[2, 1]))
    var.phi.star.1 <- sum(1 / tab.z1)
    tab.z2 <- table(x[z == levels(z)[2]], y[z == levels(z)[2]])
    rtot.z2 <- rowSums(tab.z2)
    ctot.z2 <- colSums(tab.z2)
    phi.star.2 <- log((tab.z2[1, 1] * tab.z2[2, 2]) /
                        (tab.z2[1, 2] * tab.z2[2, 1]))
    var.phi.star.2 <- sum(1 / tab.z2)
    stat <- phi.star.1 - phi.star.2

    #Monte Carlo p-value
    if (do.mc){
      pval.mc.stat <- stat
      if (!is.null(seed)){set.seed(seed)}
      pval.mc <- 0
      stat.tmp <- rep(NA, nsims.mc)
      for (i in 1:nsims.mc){
        #simulate tables
        tab.tmp1 <- r2dtable(1, rtot.z1, ctot.z1)[[1]]
        tab.tmp2 <- r2dtable(1, rtot.z2, ctot.z2)[[1]]
        #create z1 stat
        phi.star.tmp1 <- log((tab.tmp1[1, 1] * tab.tmp1[2, 2]) /
                               (tab.tmp1[1, 2] * tab.tmp1[2, 1]))
        var.phi.star.tmp1 <- sum(1 / tab.tmp1)
        #create z2 stat
        phi.star.tmp2 <- log((tab.tmp2[1, 1] * tab.tmp2[2, 2]) /
                               (tab.tmp2[1, 2] * tab.tmp2[2, 1]))
        var.phi.star.tmp2 <- sum(1 / tab.tmp2)
        #compare with observed statistic
        stat.tmp[i] <- phi.star.tmp1 - phi.star.tmp2
        if (abs(stat.tmp[i]) >= abs(pval.mc.stat)){
          pval.mc <- pval.mc + 1 / nsims.mc
        }
      }
      if (do.CI){
        CI.mc.lower <- quantile(stat.tmp, (1 - CI.width) / 2)[[1]]
        CI.mc.upper <- quantile(stat.tmp, 1 - (1 - CI.width) / 2)[[1]]
      }
    }

    #asymptotic p-value
    if (do.asymp){
      pval.asymp.stat <- (phi.star.1 - phi.star.2) /
        sqrt(var.phi.star.1 + var.phi.star.2)
      pval.asymp <- pnorm(abs(pval.asymp.stat), lower.tail = FALSE) * 2
      #confidence intervals
      if (do.CI){
        CI.asymp.lower <- phi.star.1 - phi.star.2 -
          qnorm((1 - CI.width) / 2, lower.tail = FALSE) *
          sqrt(var.phi.star.1 + var.phi.star.2)
        CI.asymp.upper <- phi.star.1 - phi.star.2 +
          qnorm((1 - CI.width) / 2, lower.tail = FALSE) *
          sqrt(var.phi.star.1 + var.phi.star.2)
      }
    }

    #check if message needed
    if (!do.mc && !do.asymp) {
      test.note <- paste("Neither Asymptotic nor Monte Carlo test requested")
    }

    #define hypotheses
    H0 <- paste0("H0: Odds ratios for tables of ", varname1, " and ", varname2,
                 " by level of ", varname3, " are equal\n",
                 "H1: Odds ratios for tables of ", varname1, " and ", varname2,
                 " by level of ", varname3, " are not equal\n")

    #return
    result <- list(title = "Test for difference in odds ratios",
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
