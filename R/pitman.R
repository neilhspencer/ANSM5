#' @importFrom stats complete.cases pnorm
pitman <- function(x, H0, alternative=c("two.sided", "less", "greater"),
                        max.exact.cases = 1000, do.asymp = FALSE,
                        do.exact = TRUE) {
  stopifnot(is.vector(x), is.numeric(x), is.numeric(H0), length(H0) == 1,
            is.numeric(max.exact.cases), length(max.exact.cases) == 1,
            is.logical(do.asymp) == TRUE, is.logical(do.exact) == TRUE)
  alternative <- match.arg(alternative)

  #labels
  varname1 <- deparse(substitute(x))

  #unused arguments
  varname2 <- NULL
  cont.corr <- NULL
  CI.width <- NULL
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
  x <- x[x != H0] #remove cases equal to H0
  n <- length(x)

  #give asymptotic output if exact not possible
  if (do.exact && n > max.exact.cases){
    do.asymp <- TRUE
  }

  #statistics
  if (do.exact && n <= max.exact.cases){
    for (dp in 0:6){ #determine max decimal places (max 6)
      if (all(x == round(x, dp))){
        break
      }
    }
    x <- round(x * 10 ^ dp, 0) #multiply up and round to integers
    H0 <- H0 * 10 ^ dp #adjust H0 to match changes
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

  #exact p-value
  if (do.exact && n <= max.exact.cases){
    pval.stat.greater <- sum((x - H0)[(x - H0) > 0])
    pval.greater <- sum(permsums[permsums[,1] >= pval.stat.greater, 2]) / sum(permsums[,2])
    pval.stat.less <- -sum((x - H0)[(x - H0) < 0])
    pval.less <- sum(permsums[permsums[,1] >= pval.stat.less, 2]) / sum(permsums[,2])
    if (alternative=="two.sided"){
      pval.exact.stat <- pval.stat.greater
      pval.exact <- min(pval.less, pval.greater) * 2
    }else if (alternative == "less"){
      pval.exact.stat <- pval.stat.less
      pval.exact <- pval.less
    }else if (alternative == "greater"){
      pval.exact.stat <- pval.stat.greater
      pval.exact <- pval.greater
    }
  }

  #asymptotic p-value
  if (do.asymp){
    s <- x - H0
    pval.stat.less <- sum((H0 - x)[(x - H0) < 0])
    pval.stat.greater <- sum((x - H0)[(x - H0) > 0])
    pval.asymp.less <-
      pnorm((pval.stat.less - 0.5 * sum(abs(s))) / (0.5 * sqrt(sum(s ** 2))),
            lower.tail = FALSE)
    pval.asymp.greater <-
      pnorm((pval.stat.greater - 0.5 * sum(abs(s))) / (0.5 * sqrt(sum(s ** 2))),
            lower.tail = FALSE)
    if (alternative=="two.sided"){
      pval.asymp.stat <- abs((pval.stat.greater - 0.5 * sum(abs(s))) / (0.5 * sqrt(sum(s ** 2))))
      pval.asymp <- min(pval.asymp.less, pval.asymp.greater) * 2
    }else if (alternative == "less"){
      pval.asymp.stat <- abs((pval.stat.less - 0.5 * sum(abs(s))) / (0.5 * sqrt(sum(s ** 2))))
      pval.asymp <- pval.asymp.less
    }else if (alternative == "greater"){
      pval.asymp.stat <- abs((pval.stat.greater - 0.5 * sum(abs(s))) / (0.5 * sqrt(sum(s ** 2))))
      pval.asymp <- pval.asymp.greater
    }
    if (n < 20){
      pval.asymp.note <-
        pval.asymp.note <- paste("(WARNING: n is less than 20 so asymptotic",
                                 "test is not recommended)")
    }
  }

  #check if message needed
  if (do.exact && n > max.exact.cases) {
    test.note <- paste0("NOTE: Number of useful cases greater than current ",
                        "maximum allowed for exact\n calculations required ",
                        "(max.exact.cases = ",
                        sprintf("%1.0f", max.exact.cases), ")")
  }

  #undo effect of dp adjustment for exact test
  if (do.exact && n <= max.exact.cases) {
    pval.exact.stat <- pval.exact.stat / 10 ^ dp
    H0 <- H0 / 10 ^ dp
  }

  #return
  result <- list(title = "Pitman test", varname1 = varname1,
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