#' @importFrom stats median quantile
mc.ci <-
  function(x, CI.width = 0.95, nsims.mc = 10000, seed = NULL) {
    stopifnot(is.vector(x), is.numeric(x), is.numeric(CI.width),
              length(CI.width) == 1, CI.width > 0, CI.width < 1,
              is.numeric(nsims.mc), length(nsims.mc) == 1,
              is.numeric(seed) | is.null(seed),
              length(seed) == 1 | is.null(seed))

    #Monte Carlo confidence interval
    median.vec <- rep(NA, nsims.mc)
    n <- length(x)
    if (!is.null(seed)){set.seed(seed)}
    for (i in 1:nsims.mc){
      x.2 <- sample(x, n, replace = TRUE)
      median.vec[i] <- median(x.2)
    }
    mc.ci.lower <- quantile(median.vec, (1 - CI.width) / 2)
    mc.ci.upper <- quantile(median.vec, 1 - (1 - CI.width) / 2)

    #return
    return(c(as.numeric(mc.ci.lower), as.numeric(mc.ci.upper)))
  }
