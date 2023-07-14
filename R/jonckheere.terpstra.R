#' @importFrom stats complete.cases
#' @importFrom utils combn
jonckheere.terpstra <-
  function(x, g, max.exact.cases = 15, nsims.mc = 10000, seed = NULL,
           do.asymp = FALSE, do.exact = TRUE) {
    stopifnot(is.vector(x), is.numeric(x), is.factor(g), length(x) == length(g),
              length(x[complete.cases(x)]) == length(g[complete.cases(g)]),
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              length(seed) == 1 | is.null(seed),
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE)

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(g))

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
    g <- g[complete.cases(g)] #remove missing cases
    n <- length(x)
    table_g <- table(g)

    #check for ties
    tiesexist = !all(rank(x) == round(rank(x),0)) # TRUE if ties exist

    #calculate test statistic
    U <- 0
    for (i in 1:(nlevels(g) - 1)){
      for (j in (i + 1):nlevels(g)){
        x1 <- x[g == levels(g)[i]]
        x2 <- x[g == levels(g)[j]]
        U <- U + sum(sapply(x1, function(z) sum(x2 > z) + 0.5 * sum(x2 == z)))
      }
    }

    #exact p-value
    if (do.exact && n <= max.exact.cases){
      combins <- NULL
      for (ig in 1:(nlevels(g) - 1)){
        if (ig == 1){
          combins <- t(combn(n, table_g[ig]))
        }else{
          combins2 <- NULL
          for (i in 1:dim(combins)[1]){
            combins2 <- rbind(combins2,
                              cbind(matrix(rep(combins[i,],
                                               choose(n - dim(combins)[2],
                                                      table_g[ig])),
                                           ncol = dim(combins)[2],
                                           byrow = TRUE),
                                    t(combn(setdiff(seq(1:n), combins[i,]),
                                            table_g[ig]))))
          }
          combins <- combins2
        }
      }
      combins <- matrix(combins, ncol = dim(combins)[2])
      last.group <- apply(combins, 1, function(z) setdiff(1:n, z))
      last.group <- matrix(t(last.group), ncol = n - dim(combins)[2])
      combins <- cbind(combins, last.group)
      combins <- data.frame(matrix(x[combins], ncol=dim(combins)[2]))
      #calculate U for each combination
      combins.U <- rep(0, dim(combins)[1])
      for (i in 1:(nlevels(g) - 1)){
        for (j in (i + 1):nlevels(g)){
          x1 <- combins[,g == levels(g)[i]]
          x2 <- combins[,g == levels(g)[j]]
          for (k in 1:length(x1)){
            combins.U <- combins.U +
              rowSums(x2 > x1[,k]) + 0.5 * rowSums(x2 == x1[,k])
          }
        }
      }
      pval.exact <- sum(combins.U <= U) / dim(combins)[1]
      pval.exact.stat <- U
    }

    #Monte Carlo p-value
    if (do.exact && n > max.exact.cases){
      if (!is.null(seed)){set.seed(seed)}
      U.sim <- NULL
      for (i in 1:nsims.mc){
        x.sim <- sample(x, n, replace = FALSE)
        Ui <- 0
        for (i in 1:(nlevels(g) - 1)){
          for (j in (i + 1):nlevels(g)){
            x1 <- x.sim[g == levels(g)[i]]
            x2 <- x.sim[g == levels(g)[j]]
            Ui <- Ui +
              sum(sapply(x1, function(z) sum(x2 > z) + 0.5 * sum(x2 == z)))
          }
        }
        U.sim <- c(U.sim, Ui)
      }
      pval.mc <- sum(U.sim <= U) / nsims.mc
      pval.mc.stat <- U
    }

    #asymptotic p-value
    if (do.asymp){
      U.Exp <- (n ** 2 - sum(table_g ** 2)) / 4
      U.Var <- (n ** 2 * (2 * n + 3) - sum(table_g ** 2 * (2 * table_g + 3))) / 72
      pval.asymp.stat <- (U - U.Exp) / sqrt(U.Var)
      pval.asymp <- pnorm(pval.asymp.stat)
    }

    #check if message needed
    if (!do.asymp && !do.exact) {
      test.note <- paste("Neither exact nor asymptotic test requested")
    }else if (n > max.exact.cases) {
      test.note <- paste0("NOTE: Number of useful cases greater than current ",
                          "maximum allowed for exact\ncalculations required ",
                          "for exact test (max.exact.cases = ",
                          sprintf("%1.0f", max.exact.cases), ")\nso Monte ",
                          "Carlo p-value given")
    }
    if (do.asymp & tiesexist){
      if (!is.null(test.note)){
        test.note <- paste0(test.note, "\n")
      }
      test.note <- paste0(test.note, "NOTE: Asymptotic p-value not adjusted ",
                          "for ties in data")
    }

    #define hypotheses
    H0 <- paste0("H0: samples are from the same population\n",
                 "H1: samples differ with median of '", levels(g)[1], "'\n")
    for (i in 2:nlevels(g)){
      H0 <- paste0(H0, "    > median of '", levels(g)[i], "'\n")
    }

    #return
    result <- list(title = "Jonckheere-Terpstra test", varname1 = varname1,
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