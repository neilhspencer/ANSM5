#' @importFrom stats model.frame model.response model.matrix complete.cases lm median approx quantile
spearman.beta <-
  function(formula, data, H0 = NULL,
           alternative = c("two.sided", "less", "greater"), CI.width = 0.95,
           max.exact.cases = 10, nsims.mc = 100000, seed = NULL,
           do.asymp = FALSE, do.exact = TRUE, do.CI = FALSE, do.mc = FALSE) {
    stopifnot(inherits(formula,"formula"), length(all.vars(formula)) == 2,
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
    varname1 <- Reduce(paste, deparse(formula))
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
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    y <- model.response(mf, "numeric")
    mt <- attr(mf, "terms")
    x <- model.matrix(mt, mf)[, -1]
    complete.cases.id <- complete.cases(x, y)
    y <- y[complete.cases.id] #remove missing cases
    x <- x[complete.cases.id] #remove missing cases
    x <- x - median(x)
    n <- length(y)

    #calculate estimate of beta
    outer.y <- outer(y, y, "-")
    outer.x <- outer(x, x, "-")
    bvals <- outer.y / outer.x
    bvals <- sort(bvals[upper.tri(bvals)])
    mid.b <- c(NA, bvals[1:(length(bvals) - 1)] + diff(bvals) / 2, NA)
    T <- NULL
    for (i in 1:length(mid.b)){
      T[i] <- sum(x * rank(y - mid.b[i] * x))

    }
    stat <- approx(T[!duplicated(T)], mid.b[!duplicated(T)], xout=0)$y
    statlabel <- "Spearman beta"

    if (!is.null(H0)){
      spearman.test <- spearman(x, y - H0 * x, alternative = alternative,
                              max.exact.cases = max.exact.cases,
                              nsims.mc = nsims.mc, seed = seed,
                              do.asymp = do.asymp, do.exact = do.exact,
                              do.mc = do.mc)
      pval <- spearman.test$pval
      pval.stat <- spearman.test$pval.stat
      pval.note <- spearman.test$pval.note
      pval.asymp <- spearman.test$pval.asymp
      pval.asymp.stat <- spearman.test$pval.asymp.stat
      pval.asymp.note <- spearman.test$pval.asymp.note
      pval.exact <- spearman.test$pval.exact
      pval.exact.stat <- spearman.test$pval.exact.stat
      pval.exact.note <- spearman.test$pval.exact.note
      pval.mc <- spearman.test$pval.mc
      pval.mc.stat <- spearman.test$pval.mc.stat
      pval.mc.note <- spearman.test$pval.mc.note
      stat.note <- spearman.test$stat.note
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
        mid.b <- c(NA, bvals[1:(length(bvals) - 1)] + diff(bvals) / 2, NA)
        T <- NA
        for (j in 1:length(mid.b)){
          T[j] <- sum(x.sample * rank(y.sample - mid.b[j] * x.sample))

        }
        if (sum(!is.na(mid.b[!duplicated(T)])) > 1){
          beta.mc[i] <- approx(T[!duplicated(T)], mid.b[!duplicated(T)], xout=0)$y
        }
      }
      CI.mc.lower <- quantile(beta.mc, (1 - CI.width) / 2, na.rm = TRUE)[[1]]
      CI.mc.upper <- quantile(beta.mc, 1 - (1 - CI.width) / 2, na.rm = TRUE)[[1]]
    }

    #create hypotheses
    if (!is.null(H0)){
      H0val <- H0
      H0 <- paste0("H0: Spearman beta for ", Reduce(paste, deparse(formula)),
                   " is ", H0val)
      if (alternative == "two.sided"){
        H0 <- paste0(H0, "\nH1: Spearman beta for ",
                     Reduce(paste, deparse(formula)), " is not ", H0val)
      }else if(alternative == "greater"){
        H0 <- paste0(H0, "\nH1: Spearman beta for ",
                     Reduce(paste, deparse(formula)), " is greater than ",
                     H0val)
      }else{
        H0 <- paste0(H0, "\nH1: Spearman beta for ",
                     Reduce(paste, deparse(formula)), " is less than ",
                     H0val)
      }
      H0 <- paste0(H0, "\n")
    }

    #return
    result <- list(title = "Spearman beta", varname1 = varname1,
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
