#' @importFrom stats complete.cases median
#' @importFrom utils combn
control.median <-
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
    varname1 <- paste0(deparse(substitute(x)), " (Control)")
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
    n <- length(x) + length(y)
    if (!is.null(H0)) {
      x <- x - H0
      varname1 <- paste0(varname1, " - ", H0)
    }else{
      H0 <- 0
    }
    med_x <- median(x)

    #give asymptotic output if exact not possible
    if (do.exact && n > max.exact.cases){
      do.asymp <- TRUE
    }

    #exact p-value
    if (do.exact && n <= max.exact.cases){
      #test statistic
      if (alternative == "two.sided"){
        pval.exact.stat <- min(sum(y < med_x), sum(y > med_x))
      }else if (alternative == "greater"){
        pval.exact.stat <- sum(y < med_x)
      }else{
        pval.exact.stat <- sum(y > med_x)
      }
      #calculate
      m2 <- sum(x != med_x)
      n2 <- sum(y != med_x)
      r <- sum(x < med_x)
      pval.exact <- 0
      if (alternative == "two.sided" | alternative == "greater"){
        for (i in 0:pval.exact.stat){
          pval.exact <- pval.exact +
            choose(r + i, i) * choose(m2 - r + n2 - i, n2 - i) /
            choose(m2 + n2, n2)
          message(choose(r + i, i) * choose(m2 - r + n2 - i, n2 - i) /
                    choose(m2 + n2, n2))
        }
      }
      if (alternative == "two.sided" | alternative == "less"){
        for (i in seq(n2, n2 - pval.exact.stat, -1)){
          pval.exact <- pval.exact +
            choose(r + i, i) * choose(m2 - r + n2 - i, n2 - i) /
            choose(m2 + n2, n2)
          message(choose(r + i, i) * choose(m2 - r + n2 - i, n2 - i) /
                    choose(m2 + n2, n2))
        }
      }
    }



    for (i in 0:n2){
      pval.exact <- pval.exact +
        choose(r + i, i) * choose(m2 - r + n2 - i, n2 - i) /
        choose(m2 + n2, n2)
      message(choose(r + i, i) * choose(m2 - r + n2 - i, n2 - i) /
                choose(m2 + n2, n2))
    }

    i=11
    choose(16, 11) * choose(22, 17) / choose(38, 28)



    #check for ties
    tiesexist = !all(s == round(s,0)) # TRUE if ties exist

    #exact p-value & CI
    OverflowState <- FALSE
    if (do.exact && tiesexist){
      try_result <- suppressWarnings(try(
        combins <- combn(length(xy), min(length(x), length(y))), silent = TRUE)
        )
      if (any(class(try_result) == "try-error")){
        OverflowState <- TRUE
      }
        if (OverflowState){
        do.asymp <- TRUE
      }
    }
    if (do.exact && ((!tiesexist && n <= max.exact.cases) |
                     (tiesexist && !OverflowState))){
      pval.exact.stat <- stat
      if (!tiesexist){
        wilcox.test.output <- wilcox.test(x, y, alternative = alternative,
                                          mu = H0, exact = TRUE,
                                          conf.int = do.CI,
                                          conf.level = CI.width)
        pval.exact <- wilcox.test.output$p.value
        if (do.CI){
          CI.exact.lower <- wilcox.test.output$conf.int[1]
          CI.exact.upper <- wilcox.test.output$conf.int[2]
        }
      }else{
        nfrom <- min(length(x), length(y))
        permfrom <- s * 2
        permfrom <- sort(permfrom)
        permsums <- rep(0,sum(tail(permfrom, nfrom)))
        for (i in 1:dim(combins)[2]){
          tmpsum <- sum(permfrom[combins[,i]])
          permsums[tmpsum] <- permsums[tmpsum] + 1
        }
        ranksum <- ifelse(length(x) < length(y), ranksumx * 2, ranksumy * 2)
        if (length(x) < length(y) && alternative == "less"){
          pval.exact.less <-
            sum(permsums[1:ranksum]) / sum(permsums)
          pval.exact.greater <-
            sum(permsums[ranksum:length(permsums)]) / sum(permsums)
        }else if (length(x) < length(y) && alternative == "greater"){
          pval.exact.less <-
            sum(permsums[ranksum:length(permsums)]) / sum(permsums)
          pval.exact.greater <-
            sum(permsums[1:ranksum]) / sum(permsums)
        }else if (length(x) >= length(y) && alternative == "less"){
          pval.exact.less <-
            sum(permsums[ranksum:length(permsums)]) / sum(permsums)
          pval.exact.greater <-
            sum(permsums[1:ranksum]) / sum(permsums)
        }else if (length(x) >= length(y) && alternative == "greater"){
          pval.exact.less <-
            sum(permsums[1:ranksum]) / sum(permsums)
          pval.exact.greater <-
            sum(permsums[ranksum:length(permsums)]) / sum(permsums)
        }else{
          pval.exact.less <-
            sum(permsums[1:ranksum]) / sum(permsums)
          pval.exact.greater <-
            sum(permsums[ranksum:length(permsums)]) / sum(permsums)
        }
        if (alternative=="two.sided"){
          pval.exact <- min(pval.exact.less, pval.exact.greater) * 2
        }else if (alternative == "less"){
          pval.exact <- pval.exact.less
        }else if (alternative == "greater"){
          pval.exact <- pval.exact.greater
        }
        if (do.CI){
          CI.exact.lower <- NULL
          CI.exact.upper <- NULL
        }
      }
    }

    #asymptotic p-value and CI (with/without continuity correction)
    if (do.asymp){
      wilcox.test.output <- wilcox.test(x, y, alternative = alternative,
                                        mu = H0, exact = FALSE,
                                        correct = cont.corr, conf.int = do.CI,
                                        conf.level = CI.width)
      pval.asymp.stat <- stat
      pval.asymp <- wilcox.test.output$p.value
      if (do.CI){
        CI.asymp.lower <- wilcox.test.output$conf.int[1]
        CI.asymp.upper <- wilcox.test.output$conf.int[2]
      }
    }

    #check if message needed
    if (!do.asymp && !do.exact) {
      test.note <- paste("Neither exact nor asymptotic test/confidence interval ",
                         "requested")
    }else if (n > max.exact.cases) {
      affected <- NULL
      if (do.exact && do.CI){
        affected <- "exact test and confidence interval"
      }else if (do.exact) {
        affected <- "exact test"
      }
      if (!is.null(affected)){
        test.note <- paste0("NOTE: Number of useful cases greater than current ",
                            "maximum allowed for exact\ncalculations required ",
                            "for ", affected, " (max.exact.cases = ",
                            sprintf("%1.0f", max.exact.cases), ")")
      }
    }
    if (tiesexist && OverflowState){
      if (!is.null(test.note)){
        test.note <- paste0(test.note, "\n")
      }
      test.note <- paste0(test.note, "NOTE: Ties exist in data and sample ",
                          "too large for exact\ncalculations required ",
                          "for exact test")
    }
    if (tiesexist && !OverflowState && do.exact && do.CI){
      if (!is.null(test.note)){
        test.note <- paste0(test.note, "\n")
      }
      if (do.CI){
        test.note <- paste0(test.note, "NOTE: Ties exist in data so exact ",
                            "confidence interval\nnot available")
      }else if (!do.CI){
        test.note <- paste0(test.note, "NOTE: Ties exist in data so mid-ranks ",
                            "used for asymptotic test")
      }
    }
    if (tiesexist && do.asymp){
      if (!is.null(test.note)){
        test.note <- paste0(test.note, "\n")
      }
      if (do.CI){
        test.note <- paste0(test.note, "NOTE: Ties exist in data so mid-ranks ",
                            "used for asymptotic\ntest and confidence interval")
      }else if (!do.CI){
        test.note <- paste0(test.note, "NOTE: Ties exist in data so mid-ranks ",
                            "used for asymptotic test")
      }
    }

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
    result <- list(title = "Wilcoxon-Mann-Whitney test", varname1 = varname1,
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
