#' @importFrom stats complete.cases
hodges.ajne <-
  function(x, alternative = c("two.sided"), minx = 0, maxx = 360) {
    stopifnot(is.vector(x), is.numeric(x), length(x) > 1, min(x) >= minx,
              max(x) <= maxx)
    alternative <- match.arg(alternative)

    #labels
    varname <- deparse(substitute(x))

    #default outputs
    cont.corr <- NULL
    CI.width <- NULL
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
    n <- length(x)
    x <- (x + minx) #make smallest possible number zero
    x <- x * (360 / maxx) #make largest possible number 360
    x <- sort(x)

    #calculate angles to check
    angles <- NULL
    check3 <- 0
    can_exit <- FALSE
    starta <- x[1]
    repeat{
      if (check3 == 0){
        if (length(x[x > starta]) > 0){
          nexta <- min(x[x > starta])
        }else{
          nexta <- x[1]
        }
      }else{
        if (length(x[x > check1]) > 0){
          nexta <- min(x[x > check1]) - 180 + 360 * (min(x[x > check1]) - 180 < 0)
        }else{
          nexta <- x[1] - 180 + 360 * (x[1] - 180 < 0)
        }
      }
      check1 <- starta + 180 - 360 * (starta + 180 > 360)
      check2 <- nexta + 180 - 360 * (nexta + 180 > 360)
      check3 <- sum(x > check1 & x < check2)
      if (check3 == 0){
        if (starta < nexta){
          angles <- c(angles, (starta + nexta) / 2)
        }else{
          tempa <- ((starta - 360) + nexta) / 2
          if (tempa < 0){
            angles <- c(angles, 360 + tempa)
          }else{
            angles <- c(angles, tempa)
          }
        }
        starta <- nexta
        can_exit <- TRUE
      }
      if (starta == x[1] && can_exit){break}
    }

    #calculate m
    pval.stat <- length(x) + 1
    for (i in 1:length(angles)){
      m.a1 <- sum(x - angles[i] + 360 * (x - angles[i] < 0) > 180)
      m.a2 <- sum(x - angles[i] + 360 * (x - angles[i] < 0) < 180)
      if (m.a1 < pval.stat){pval.stat <- m.a1}
      if (m.a2 < pval.stat){pval.stat <- m.a2}
    }

    #p-value
    if (pval.stat < n / 3){
      pval <- choose(n, pval.stat) * (n - 2 * pval.stat) / (2 ^ (n - 1))
    }else{
      pval <- NA
      pval.note <- paste0(varname, " is distributed in a sufficiently\n",
                          "uniform fashion to result in the smallest number\n",
                          "of cases found in a half circle being more than a\n",
                          "third of the total number of cases, meaning that\n",
                          "the Mardia (1972) formula to calculate the\n",
                          "probabilities cannot be used")
    }

    #create hypotheses
    H0 <- paste0("H0: distribution of ", varname, " is uniform\n",
                 "H1: distribution of ", varname, " is not uniform\n")

    #return
    result <- list(title = paste0("Hodges-Ajne test"),
                   varname = varname, H0 = H0,
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
