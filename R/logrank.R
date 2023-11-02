#' @importFrom stats complete.cases
#' @importFrom utils combn
logrank <-
  function(x, censored, groups, score.censored = TRUE,
           max.exact.perms = 100000, nsims.mc = 10000, seed = NULL) {
    stopifnot(is.vector(x), is.numeric(x),
              is.vector(censored), is.numeric(censored),
              all(censored == 0 | censored == 1),
              is.factor(groups),
              length(x) == length(censored), length(censored) == length(groups),
              is.logical(score.censored),
              is.numeric(max.exact.perms), length(max.exact.perms) == 1,
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              length(seed) == 1 | is.null(seed))

    #labels
    varname1 <- deparse(substitute(x))
    varname2 <- deparse(substitute(groups))

    #unused arguments
    alternative <- "two.sided"
    cont.corr <- NULL
    CI.width <- NULL
    do.asymp <- NULL
    do.asymp <- FALSE
    do.exact <- TRUE
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
    complete.cases.ID <- complete.cases(x, censored, groups)
    x <- x[complete.cases.ID] #remove missing cases
    censored <- censored[complete.cases.ID] #remove missing cases
    groups <- groups[complete.cases.ID] #remove missing cases
    g <- nlevels(groups)
    n <- length(x)
    n.g1 <- table(groups)[1]
    n.g2 <- table(groups)[2]
    n.perms <- choose(n, n.g1)
    if (g == 3){
      n.perms <- n.perms * choose(n - n.g1, n.g2)
      n.g3 <- table(groups)[3]
    }
    #calculate statistics
    if (score.censored){
      x1 <- sort(x)
      c1 <- censored[order(x)]
      g1 <- groups[order(x)]
      score <- rep(NA, n)
      score.c0 <- 1
      for (i in 1:n){
        if (c1[i] == 0){
          score[i] <- score.c0 - sum(x1 == x1[i]) / (n - sum(x1 < x1[i]))
        }else{
          score[i] <- score.c0 - 1
        }
        #update retained scores
        if (i < n){
          if (c1[i] == 0 && x1[i] != x1[i + 1]){
            score.c0 <- score[i]
          }
        }
      }
      test.stat <- max(by(score, g1, sum))
    }else{
      obs.failures <- table(x[censored == 0], groups[censored ==0])
      tot.failures <- rowSums(obs.failures)
      for (i in 1:g){
        if (i == 1){
          in.sample <-
            as.numeric(lapply(as.numeric(row.names(obs.failures)), function(y)
              sum(y <= x[groups == levels(groups)[i]])))
        }else{
          in.sample <-
            cbind(in.sample,
                  as.numeric(lapply(as.numeric(row.names(obs.failures)),
                                    function(y)
                                    sum(y <= x[groups == levels(groups)[i]]))))
        }
      }
      at.risk <- rowSums(in.sample)
      e.failures <- tot.failures * in.sample / at.risk
      test.stat <- max(colSums(obs.failures) - colSums(e.failures))
    }

    #exact p-value
    OverflowState <- FALSE
    if (n.perms <= max.exact.perms && g <= 3){
      #try complete combinations
      try_result <- suppressWarnings(try({
        combins1 <- combn(n, n.g1)
        n.combins1 <- dim(combins1)[2]
        n.combins2 <- 1
        if (g ==3){
          tmp.array <- array(NA, dim = c(n - n.g1, dim(combins1)[2]))
          combins2 <- array(NA, dim = c(n.g2, choose(n - n.g1, n.g2),
                                        dim(combins1)[2]))
          n.combins2 <- dim(combins2)[2]
          for (i in 1:dim(combins1)[2]){
            tmp.array[, i] <- seq(1, n)[-combins1[, i]]
            combins2[, , i] <- combn(tmp.array[, i], n.g2)
          }
        }
      }, silent = TRUE)
      )
      if (any(class(try_result) == "try-error")){
        OverflowState <- TRUE
      }
    }
    if (n.perms > max.exact.perms | OverflowState| g > 3){
      #use Monte Carlo
      if (!is.null(seed)){set.seed(seed)}
      n.combins1 <- nsims.mc
    }
    #evaluate all combinations
    tmp.pval <- 0
    for (i in 1:n.combins1){
      for (j in 1:n.combins2){
        #define data
        if (n.perms > max.exact.perms | OverflowState | g > 3){
          tmp.combins <- sample(n, n)
        }else{
          tmp.combins <- combins1[, i]
          if (g == 3){
            tmp.combins <- c(tmp.combins, combins2[, j, i])
          }
          tmp.combins <- c(tmp.combins, seq(1, n)[-tmp.combins])
        }
        tmp.groups <- groups[tmp.combins]
        #calculate statistics
        if (score.censored){
          tmp.g1 <- tmp.groups[order(x)]
          tmp.max.score <- max(by(score, tmp.g1, sum))
          #check against actual test statistic
          if (tmp.max.score <= test.stat){
            tmp.pval <- tmp.pval + 1 / (n.combins1 * n.combins2)
          }
        }else{
          tmp.obs.failures <- table(x[censored == 0], tmp.groups[censored ==0])
          tmp.tot.failures <- rowSums(tmp.obs.failures)
          for (k in 1:g){
            if (k == 1){
              tmp.in.sample <-
                as.numeric(lapply(as.numeric(row.names(tmp.obs.failures)),
                              function(y)
                              sum(y <= x[tmp.groups == levels(tmp.groups)[k]])))
            }else{
              tmp.in.sample <-
                cbind(tmp.in.sample,
                      as.numeric(lapply(as.numeric(row.names(tmp.obs.failures)),
                            function(y)
                            sum(y <= x[tmp.groups == levels(tmp.groups)[k]]))))
            }
          }
          tmp.at.risk <- rowSums(tmp.in.sample)
          tmp.e.failures <- tmp.tot.failures * tmp.in.sample / tmp.at.risk
          tmp.max.sm <- max(colSums(tmp.obs.failures) - colSums(tmp.e.failures))
          #check against actual test statistic
          if (tmp.max.sm <= test.stat){
            tmp.pval <- tmp.pval + 1 / (n.combins1 * n.combins2)
          }
        }
      }
    }
    #output
    if (n.perms <= max.exact.perms && !OverflowState && g <= 3){
      pval.exact.stat <- test.stat
      pval.exact <- as.numeric(tmp.pval)
    }else{
      pval.mc.stat <- test.stat
      pval.mc <- as.numeric(tmp.pval)
    }

    #check if message needed
    if (n.perms > max.exact.perms) {
      test.note <- paste0("NOTE: Number of permutations required greater than ",
                          "current maximum allowed for exact calculations\n",
                          "required for exact test (max.exact.perms = ",
                          sprintf("%1.0f", max.exact.perms), ") so Monte ",
                          "Carlo p-value given")
    }else if (OverflowState){
      test.note <- paste0("NOTE: Insufficient memory for exact calculations",
                          "required for exact test\n(max.exact.perms = ",
                          sprintf("%1.0f", max.exact.perms), ") so Monte ",
                          "Carlo p-value given")
    }

    #define hypotheses
    H0 <- paste0("H0: the survival times distributions are identical\n",
                 "H1: the survival times distributions of ", varname1,
                 " are different across ", varname2, "\n")

    #return
    result <- list(title = "logrank test", varname1 = varname1,
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