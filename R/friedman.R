#' @importFrom stats complete.cases pf pchisq
friedman <-
  function(y, groups, blocks, ..., use.Iman.Davenport = FALSE,
           max.exact.perms = 100000, nsims.mc = 100000, seed = NULL,
           do.asymp = FALSE, do.exact = TRUE) {
    stopifnot(is.vector(y), is.numeric(y), is.factor(groups), is.factor(blocks),
              length(y) == length(groups), length(groups) == length(blocks),
              length(y) == nlevels(groups) * nlevels(blocks),
              is.logical(use.Iman.Davenport) == TRUE,
              is.numeric(max.exact.perms), length(max.exact.perms) == 1,
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              length(seed) == 1 | is.null(seed),
              is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE)

    #labels
    varname1 <- deparse(substitute(y))
    varname2 <- paste0(deparse(substitute(groups)), " (as groups) with ",
                       deparse(substitute(blocks)), " (as blocks)")

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
    complete.cases.ID <- complete.cases(y, groups, blocks)
    y <- y[complete.cases.ID] #remove missing cases
    groups <- groups[complete.cases.ID] #remove missing cases
    blocks <- blocks[complete.cases.ID] #remove missing cases
    g <- nlevels(groups)
    b <- nlevels(blocks)
    n.perms <- factorial(g) ** (b - 1)
    rank.tab <- simplify2array(by(y, blocks, rank, simplify = TRUE))
    Sr <- sum(rank.tab ** 2)
    St <- sum(rowSums(rank.tab) ** 2) / b
    C <- b * g * (g + 1) ** 2 / 4
    Tstat <- b * (g - 1) * (St - C) / (Sr - C)
    T1stat <- (b - 1) * (St - C) / (Sr - St)

    #check for ties
    tiesexist = !all(rank.tab == round(rank.tab,0)) # TRUE if ties exist in blocks

    #exact p-value
    if(do.exact && n.perms <= max.exact.perms){
      if (!tiesexist | !use.Iman.Davenport){
        pval.exact.stat <- Tstat
      }else{
        pval.exact.stat <- T1stat
        pval.exact.note <- paste0("NOTE: Test statistic calculation of Iman ",
                                  "and Davenport used due to the presence of ",
                                  "ties")
      }
      #create combinations for each row
      for (i in 1:g){
        if (i == 1){
          rowcomb <- matrix(1)
        }else{
          rowcomb.nrows <- nrow(rowcomb)
          tmp.mtx <- matrix(nrow = i * rowcomb.nrows, ncol = i)
          for(j in 1:i){
            tmp.mtx[(j - 1) * rowcomb.nrows + 1:rowcomb.nrows,] <-
              cbind(j, rowcomb + (rowcomb >= j))
          }
          rowcomb <- tmp.mtx
        }
      }
      rowcomb.nrows <- dim(rowcomb)[1]
      #loop around combinations
      pval.exact <- 0
      b_i <- b
      comb_i <- c(rep(1, b - 1), 0)
      repeat{
        comb_i[b_i] <- comb_i[b_i] + 1
        if (comb_i[b_i] > rowcomb.nrows){
          repeat{
            if (b_i == 2){break}
            comb_i[b_i - 1] <- comb_i[b_i - 1] + 1
            for (i in b_i:b){
              comb_i[i] <- 1
            }
            if (comb_i[b_i - 1] <= rowcomb.nrows){
              b_i <- b
              break
            }else{
              b_i <- b_i - 1
            }
          }
          if (b_i == 2){break}
        }
        tab_i <- NULL
        for (ib in 1:b){
          tab_i <- rbind(tab_i, y[blocks == levels(blocks)[ib]][rowcomb[comb_i[ib],]])
        }
        rank.tab_i <- apply(tab_i, 1, rank)
        Sr_i <- sum(rank.tab_i ** 2)
        St_i <- sum(rowSums(rank.tab_i) ** 2) / b
        if (!tiesexist){
          Tstat_i <- b * (g - 1) * (St_i - C) / (Sr_i - C)
          if (Tstat_i >= pval.exact.stat){
            pval.exact <- pval.exact + 1 / n.perms
          }
        }else{
          T1stat_i <- (b - 1) * (St_i - C) / (Sr_i - St_i)
          if (T1stat_i >= pval.exact.stat){
            pval.exact <- pval.exact + 1 / n.perms
          }
        }
      }
    }

    #Monte Carlo p-value
    if (do.exact && n.perms > max.exact.perms){
      if (!is.null(seed)){set.seed(seed)}
      if (!tiesexist | !use.Iman.Davenport){
        pval.mc.stat <- Tstat
      }else{
        pval.mc.stat <- T1stat
        pval.mc.note <- paste0("NOTE: Test statistic calculation of Iman and ",
                                  "Davenport used due to the presence of ties")
      }
      pval.mc <- 0
      for (i in 1:nsims.mc){
        tab_i <- NULL
        for (ib in 1:b){
          tab_i <- rbind(tab_i, sample(y[blocks == levels(blocks)[ib]], g))
        }
        rank.tab_i <- apply(tab_i, 1, rank)
        Sr_i <- sum(rank.tab_i ** 2)
        St_i <- sum(rowSums(rank.tab_i) ** 2) / b
        if (!tiesexist | !use.Iman.Davenport){
          Tstat_i <- b * (g - 1) * (St_i - C) / (Sr_i - C)
          if (Tstat_i >= pval.mc.stat){
            pval.mc <- pval.mc + 1 / nsims.mc
          }
        }else{
          T1stat_i <- (b - 1) * (St_i - C) / (Sr_i - St_i)
          if (T1stat_i >= pval.mc.stat){
            pval.mc <- pval.mc + 1 / nsims.mc
          }
        }
      }
    }

    #asymptotic p-value
    if(do.asymp){
      if (!tiesexist | !use.Iman.Davenport){
        pval.asymp.stat <- Tstat
        pval.asymp <- pchisq(Tstat, g - 1, lower.tail = FALSE)
      }else{
        pval.asymp.stat <- T1stat
        pval.asymp <- pf(T1stat, g - 1, (b - 1) * (g - 1), lower.tail = FALSE)
        pval.asymp.note <- paste0("NOTE: Test statistic calculation of Iman ",
                                  "and Davenport used due to the presence of ",
                                  "ties")
      }
    }

    #check if message needed
    if (!do.asymp && !do.exact) {
      test.note <- paste("Neither exact nor asymptotic test requested")
    }else if (do.exact && n.perms > max.exact.perms) {
      test.note <- paste0("NOTE: Number of permutations required greater than ",
                          "current maximum allowed for exact calculations\n",
                          "required for exact test (max.exact.perms = ",
                          sprintf("%1.0f", max.exact.perms), ") so Monte ",
                          "Carlo p-value given")
    }

    #create hypotheses
    H0 <- paste0("H0: distributions of ", varname1, " are identical\n")
    H0 <- paste0(H0, "H1: distributions differ")
    H0 <- paste0(H0, "\n")

    #return
    result <- list(title = "Friedman test",
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
