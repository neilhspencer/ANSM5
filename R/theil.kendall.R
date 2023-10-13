#' @importFrom stats complete.cases lm median pnorm quantile
theil.kendall <-
  function(yx.formula, H0 = NULL,
           alternative = c("two.sided", "less", "greater"), CI.width = 0.95,
           max.exact.cases = 10, nsims.mc = 100000, seed = NULL,
           do.asymp = FALSE, do.exact = TRUE, do.CI = FALSE, do.mc = FALSE) {
    stopifnot(inherits(yx.formula,"formula"), length(all.vars(yx.formula)) == 2,
              exists(all.vars(yx.formula)[1]), exists(all.vars(yx.formula)[2]),
              is.vector(get(all.vars(yx.formula)[1])),
              is.numeric(get(all.vars(yx.formula)[1])),
              is.vector(get(all.vars(yx.formula)[2])),
              is.numeric(get(all.vars(yx.formula)[2])),
              length(get(all.vars(yx.formula)[1])) == length(get(all.vars(yx.formula)[2])),
              ((is.numeric(H0) && length(H0) == 1) | is.null(H0)),
              is.numeric(CI.width), length(CI.width) == 1,
              CI.width > 0, CI.width < 1,
              is.numeric(max.exact.cases), length(max.exact.cases) == 1,
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE,
              is.logical(do.CI) == TRUE, is.logical(do.mc) == TRUE)
    alternative <- match.arg(alternative)

    #labels
    varname1 <- Reduce(paste, deparse(yx.formula))
    varname2 <- NULL
    varname3 <- NULL

    #unused arguments
    cont.corr <- NULL
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
    stat.note <- NULL

    #prepare
    model <- lm(yx.formula)
    y <- model$model[, 1]
    x <- model$model[, 2]
    x <- x - median(x)
    y <- y[complete.cases(y)] #remove missing cases
    x <- x[complete.cases(x)] #remove missing cases
    n <- length(y)

    #calculate estimate of beta
    outer.y <- outer(y, y, "-")
    outer.x <- outer(x, x, "-")
    bvals <- outer.y / outer.x
    bvals <- bvals[upper.tri(bvals)]
    stat <- median(bvals)
    statlabel <- "Theil-Kendall beta"
    if (!is.null(H0)){
      Tt <- diff(table(sign(bvals - H0)))[[1]]
    }

    #give mc output if exact not possible
    if (do.exact && n > max.exact.cases){
      do.mc <- TRUE
    }

    #exact p-value
    if(do.exact && n <= max.exact.cases && !is.null(H0)){
      outer.y <- outer(y, y, "-")
      permutations <- perms(n)
      n.perms <- dim(permutations)[1]
      pval.exact <- 0
      for (i in 1:n.perms){
        outer.x <- outer(x[permutations[i,]], x[permutations[i,]], "-")
        bvals <- outer.y / outer.x
        bvals <- bvals[upper.tri(bvals)]
        beta.tmp <- median(bvals)
        if (alternative == "two.sided"){
          if (abs(beta.tmp) >= abs(stat)){
            pval.exact <- pval.exact + 1 / n.perms
          }
        }else if (alternative == "less"){
          if (beta.tmp <= stat){
            pval.exact <- pval.exact + 1 / n.perms
          }
        }else if (alternative == "greater"){
          if (beta.tmp >= stat){
            pval.exact <- pval.exact + 1 / n.perms
          }
        }
      }
    }

    #Monte Carlo p-value
    if(do.mc && !is.null(H0)){
      if (!is.null(seed)){set.seed(seed)}
      outer.y <- outer(y, y, "-")
      pval.mc <- 0
      for (i in 1:nsims.mc){
        x.tmp <- x[sample(n, n, replace = FALSE)]
        outer.x <- outer(x.tmp, x.tmp, "-")
        bvals <- outer.y / outer.x
        bvals <- bvals[upper.tri(bvals)]
        beta.tmp <- median(bvals)
        if (alternative == "two.sided"){
          if (abs(beta.tmp) >= abs(stat)){
            pval.mc <- pval.mc + 1 / nsims.mc
          }
        }else if (alternative == "less"){
          if (beta.tmp <= stat){
            pval.mc <- pval.mc + 1 / nsims.mc
          }
        }else if (alternative == "greater"){
          if (beta.tmp >= stat){
            pval.mc <- pval.mc + 1 / nsims.mc
          }
        }
      }
    }

    #asymptotic p-value
    if(do.asymp && !is.null(H0)){
      var <- n * (n - 1) * (2 * n + 5) / 18
      z <- Tt / sqrt(var)
      if (alternative == "two.sided"){
        pval.asymp <- pnorm(abs(z), lower.tail = FALSE) * 2
      }else if (alternative == "less"){
        if (stat > 0){
          pval.asymp <- 1
        }else{
          pval.asymp <- pnorm(abs(z), lower.tail = FALSE)
        }
      }else if (alternative == "greater"){
        if (stat < 0){
          pval.asymp <- 1
        }else{
          pval.asymp <- pnorm(z, lower.tail = FALSE)
        }
      }
    }

    #create Monte Carlo confidence interval
    if (do.CI){
      if (!is.null(seed)){set.seed(seed)}
      beta.mc <- NA
      for (i in 1:nsims.mc){
        xy.sample <- sample(n, n, replace = TRUE)
        y.sample <- y[xy.sample]
        x.sample <- x[xy.sample] - median(x[xy.sample])
        outer.y <- outer(y.sample, y.sample, "-")
        outer.x <- outer(x.sample, x.sample, "-")
        bvals <- outer.y / outer.x
        bvals <- sort(bvals[upper.tri(bvals)])
        beta.mc[i] <- median(bvals)
      }
      CI.mc.lower <- quantile(beta.mc, (1 - CI.width) / 2, na.rm = TRUE)[[1]]
      CI.mc.upper <- quantile(beta.mc, 1 - (1 - CI.width) / 2, na.rm = TRUE)[[1]]
    }

    #check if message needed
    if ((!do.asymp && !do.exact && !do.mc) | is.null(H0)) {
      stat.note <- paste("Neither exact, asymptotic nor Monte Carlo test requested")
    }else if (do.exact && n > max.exact.cases) {
      stat.note <- paste0("NOTE: Number of cases greater than current maximum ",
                          "allowed for exact calculations\n",
                          "required for exact test (max.exact.cases = ",
                          sprintf("%1.0f", max.exact.cases), ") so Monte ",
                          "Carlo p-value given")
    }

    #create hypotheses
    if (!is.null(H0)){
      H0val <- H0
      H0 <- paste0("H0: Theil-Kendall beta for ",
                   Reduce(paste, deparse(yx.formula)), " is ", H0val)
      if (alternative == "two.sided"){
        H0 <- paste0(H0, "\nH1: Theil-Kendall beta for ",
                     Reduce(paste, deparse(yx.formula)), " is not ", H0val)
      }else if(alternative == "greater"){
        H0 <- paste0(H0, "\nH1: Theil-Kendall beta for ",
                     Reduce(paste, deparse(yx.formula)), " is greater than ",
                     H0val)
      }else{
        H0 <- paste0(H0, "\nH1: Theil-Kendall beta for ",
                     Reduce(paste, deparse(yx.formula)), " is less than ",
                     H0val)
      }
      H0 <- paste0(H0, "\n")
    }

    #return
    result <- list(title = "Theil-Kendall beta", varname1 = varname1,
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
                   CI.mc.note = CI.mc.note,
                   stat.note = stat.note)
    class(result) <- "ANSMstat"
    return(result)
  }
