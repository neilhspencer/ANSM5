#' @importFrom stats complete.cases cor pnorm
kendall.tau <-
  function(x, y, alternative = c("two.sided", "less", "greater"),
           max.exact.cases = 10, nsims.mc = 100000, seed = NULL,
           do.asymp = FALSE, do.exact = TRUE, do.mc = FALSE) {
    stopifnot(is.vector(x), is.numeric(x), is.vector(y), is.numeric(y),
              length(x) == length(y),
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE,
              is.logical(do.mc) == TRUE)
    alternative <- match.arg(alternative)

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(y))
    varname3 <- NULL

    #unused arguments
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
    CI.sample.lower <- NULL
    CI.sample.upper <- NULL
    CI.sample.note <- NULL
    stat.note <- NULL

    #prepare
    x <- x[complete.cases(x)] #remove missing cases
    y <- y[complete.cases(y)] #remove missing cases
    n <- length(x)
    stat <- cor(x, y, method = "kendall")
    statlabel <- "Kendall's tau"

    #give mc output if exact not possible
    if (do.exact && n > max.exact.cases){
      do.mc <- TRUE
    }

    #exact p-value
    if(do.exact && n <= max.exact.cases){
      permutations <- perms(n)
      n.perms <- dim(permutations)[1]
      pval.exact <- 0
      for (i in 1:n.perms){
        cor.tmp <- cor(x[permutations[i,]], y, method = "kendall")
        if (alternative == "two.sided"){
          if (abs(cor.tmp) >= abs(stat)){
            pval.exact <- pval.exact + 1 / n.perms
          }
        }else if (alternative == "less"){
          if (cor.tmp <= stat){
            pval.exact <- pval.exact + 1 / n.perms
          }
        }else if (alternative == "greater"){
          if (cor.tmp >= stat){
            pval.exact <- pval.exact + 1 / n.perms
          }
        }
      }
    }

    #Monte Carlo p-value
    if(do.mc){
      if (!is.null(seed)){set.seed(seed)}
      pval.mc <- 0
      for (i in 1:nsims.mc){
        cor.tmp <- cor(x[sample(n, n, replace = FALSE)], y, method = "kendall")
        if (alternative == "two.sided"){
          if (abs(cor.tmp) >= abs(stat)){
            pval.mc <- pval.mc + 1 / nsims.mc
          }
        }else if (alternative == "less"){
          if (cor.tmp <= stat){
            pval.mc <- pval.mc + 1 / nsims.mc
          }
        }else if (alternative == "greater"){
          if (cor.tmp >= stat){
            pval.mc <- pval.mc + 1 / nsims.mc
          }
        }
      }
    }

    #asymptotic p-value
    if(do.asymp){
      y.ordered <- y[order(x)]
      nc <- 0
      nd <- 0
      for (i in 1:(n - 1)){
        nc <- nc + sum(y.ordered[(i + 1):n] > y.ordered[i])
        nd <- nd + sum(y.ordered[(i + 1):n] < y.ordered[i])
      }
      v0 <- n * (n - 1) * (2 * n + 5)
      ti <- table(x)[table(x) > 1]
      vt <- sum(ti * (ti - 1) * (2 * ti + 5))
      uj <- table(y.ordered)[table(y.ordered) > 1]
      vu <- sum(uj * (uj - 1) * (2 * uj + 5))
      v1 <- sum(ti * (ti - 1)) * sum(uj * (uj - 1)) / (2 * n * (n - 1))
      v2 <- sum(ti * (ti - 1) * (ti - 2)) * sum(uj * (uj - 1) * (uj - 2)) /
        (9 * n * (n - 1) * (n - 2))
      v <- (v0 - vt - vu) / 18 + v1 + v2
      zB <- (nc - nd) / sqrt(v)
      if (alternative == "two.sided"){
        pval.asymp <- pnorm(zB, lower.tail = FALSE) * 2
      }else if (alternative == "less"){
        if (stat > 0){
          pval.asymp <- 1
        }else{
          pval.asymp <- pnorm(abs(zB), lower.tail = FALSE)
        }
      }else if (alternative == "greater"){
        if (stat < 0){
          pval.asymp <- 1
        }else{
          pval.asymp <- pnorm(zB, lower.tail = FALSE)
        }
      }
    }

    #check if message needed
    if (!do.asymp && !do.exact && !do.mc) {
      stat.note <- paste("Neither exact, Monte Carlo nor asymptotic test",
                         "requested")
    }else if (do.exact && n > max.exact.cases) {
      stat.note <- paste0("NOTE: Number of useful cases greater than current ",
                          "maximum allowed for exact calculations\nrequired for ",
                          "exact test (max.exact.cases = ",
                          sprintf("%1.0f", max.exact.cases), ") so Monte ",
                          "Carlo p-value given")
    }

    #create hypotheses
    H0 <- paste0("H0: Kendall's tau for ", varname1, " and ",
                 varname2, " is 0")
    if (alternative == "two.sided"){
      H0 <- paste0(H0, "\nH1: Kendall's tau for ", varname1, " and ",
                   varname2, " is not 0")
    }else if(alternative == "greater"){
      H0 <- paste0(H0, "\nH1: Kendall's tau for ", varname1, " and ",
                   varname2, " is greater than 0")
    }else{
      H0 <- paste0(H0, "\nH1: Kendall's tau for ", varname1, " and ",
                   varname2, " is less than 0")
    }
    H0 <- paste0(H0, "\n")

    #return
    result <- list(title = "Kendall's tau", varname1 = varname1,
                   varname2 = varname2, varname3 = varname3, stat = stat,
                   statlabel = statlabel, H0 = H0,
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
                   CI.mc.note = CI.mc.note, CI.sample.lower = CI.sample.lower,
                   CI.sample.upper = CI.sample.upper, CI.sample.note = CI.sample.note,
                   stat.note = stat.note)
    class(result) <- "ANSMstat"
    return(result)
  }
