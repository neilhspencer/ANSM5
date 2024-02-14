#' @importFrom stats median quantile
bs.ci <-
  function(x, y = NULL, CI.width = 0.95, nsims.bs = 10000, seed = NULL) {
    stopifnot(is.vector(x), is.numeric(x), is.vector(y) | is.null(y),
              is.numeric(y) | is.null(y), is.numeric(CI.width),
              length(CI.width) == 1, CI.width > 0, CI.width < 1,
              is.numeric(nsims.bs), length(nsims.bs) == 1,
              is.numeric(seed) | is.null(seed),
              length(seed) == 1 | is.null(seed))

    #Monte Carlo confidence interval
    if (is.null(y)){ #for one sample
      median.vec <- rep(NA, nsims.bs)
      n <- length(x)
      if (!is.null(seed)){set.seed(seed)}
      for (i in 1:nsims.bs){
        x.2 <- sample(x, n, replace = TRUE)
        median.vec[i] <- median(x.2)
      }
      bs.ci.lower <- quantile(median.vec, (1 - CI.width) / 2)
      bs.ci.upper <- quantile(median.vec, 1 - (1 - CI.width) / 2)
    }else{ #for two samples
      mediandiff.vec <- rep(NA, nsims.bs)
      nx <- length(x)
      ny <- length(y)
      if (!is.null(seed)){set.seed(seed)}
      for (i in 1:nsims.bs){
        x.2 <- sample(x, nx, replace = TRUE)
        y.2 <- sample(y, ny, replace = TRUE)
        mediandiff.vec[i] <- median(x.2) - median(y.2)
      }
      bs.ci.lower <- quantile(mediandiff.vec, (1 - CI.width) / 2)
      bs.ci.upper <- quantile(mediandiff.vec, 1 - (1 - CI.width) / 2)
    }

    #return
    return(c(as.numeric(bs.ci.lower), as.numeric(bs.ci.upper)))
  }
