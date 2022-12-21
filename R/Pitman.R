Pitman.test <-
  function(x, H0, alternative=c("two.sided", "less", "greater"),
           max.exact.cases = 1000) {
    stopifnot(is.vector(x), is.numeric(x), is.numeric(H0), length(H0) == 1,
              is.numeric(max.exact.cases), length(max.exact.cases) == 1)
    alternative <- match.arg(alternative)

    #labels
    varname <- deparse(substitute(x))

    #unused arguments
    cont.corr <- NULL
    CI.width <- NULL
    do.asymp <- NULL
    do.exact <- NULL
    do.CI <- NULL
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
    actualCIwidth <- NULL
    CI.lower <- NULL
    CI.upper <- NULL
    CI.note <- NULL
    test.note <- NULL

    #statistics
    x <- x[complete.cases(x)] #remove missing cases
    x <- x[x != H0] #remove cases equal to H0
    n <- length(x)
    if (n <= max.exact.cases){
      permfrom <- abs(x - H0)
      permfrom <- sort(permfrom)
      permsums <- rep(0,sum(permfrom) + 1)
      permsums[1] <- 1
      current.max <- 0
      for (i in 1:length(permfrom)){
        permsumsnow <- permsums
        to.add <- permfrom[i]
        j <- 1:(current.max + 1)
        k <- (j - 1) + to.add + 1
        gtzero <- permsumsnow[j] > 0
        permsums[k][gtzero] <- permsums[k][gtzero] + permsumsnow[j][gtzero]
      current.max <- current.max + to.add
      }
      permsums <- data.frame(0:(length(permsums) - 1), permsums)
    }

    #p-value
    if (n <= max.exact.cases){
      pval.stat.greater <- sum((x - H0)[(x - H0) > 0])
      pval.greater <- sum(permsums[permsums[,1] >= pval.stat.greater, 2]) / sum(permsums[,2])
      pval.stat.less <- -sum((x - H0)[(x - H0) < 0])
      pval.less <- sum(permsums[permsums[,1] >= pval.stat.less, 2]) / sum(permsums[,2])
      if (alternative=="two.sided"){
        pval.stat <- pval.stat.greater
        pval <- min(pval.less, pval.greater) * 2
      }else if (alternative == "less"){
        pval.stat <- pval.stat.less
        pval <- pval.less
      }else if (alternative == "greater"){
        pval.stat <- pval.stat.greater
        pval <- pval.greater
      }
    }

    #check if message needed
    if (n > max.exact.cases) {
      test.note <- paste0("NOTE: Number of useful cases greater than current ",
                          "maximum allowed for\ncalculations required ",
                          "(max.exact.cases = ", max.exact.cases, ")")
    }

    #return
    result <- list(title = "Pitman test", varname = varname,
                   H0 = H0, alternative = alternative, cont.corr = cont.corr,
                   pval = pval, pval.stat = pval.stat, pval.note = pval.note,
                   pval.asymp = pval.asymp, pval.asymp.stat = pval.asymp.stat,
                   pval.asymp.note = pval.asymp.note, pval.exact = pval.exact,
                   pval.exact.stat = pval.exact.stat,
                   pval.exact.note = pval.exact.note, targetCIwidth = CI.width,
                   actualCIwidth = actualCIwidth, CI.lower = CI.lower,
                   CI.upper = CI.upper, CI.note = CI.note, test.note = test.note)
    class(result) <- "ANSMtest"
    return(result)
  }
