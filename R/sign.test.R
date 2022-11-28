sign.test <- function(x, H0, alternative=c("two.sided", "less", "greater"), CI.width = 0.95) {
  stopifnot(is.vector(x), is.numeric(x), is.numeric(H0), length(H0) == 1, CI.width > 0, CI.width < 1)
  alternative <- match.arg(alternative)

  #statistics
  pluses <- sum(x > H0, na.rm = TRUE)
  n <- sum(x != H0, na.rm = TRUE)

  #p-value
  if (alternative=="two.sided"){
    pval<-pbinom(pluses, n, 0.5)+(1-pbinom(n-pluses-1, n, 0.5))
  }else if (alternative == "less"){
    pval <- pbinom(pluses, n, 0.5)
  }else{
    pval <- pbinom(n-pluses-1, n, 0.5)
  }

  #CI
  lower <- 0
  lower.cumu <- 0
  repeat{
    if (lower.cumu + dbinom(lower, n, 0.5) > (1-CI.width)/2) {break}
    lower.cumu <- lower.cumu + dbinom(lower, n, 0.5)
    lower <- lower + 1
  }
  if (lower == 0){
    CI.lower = Inf
  }else{
    CI.lower <- sort(x)[lower]
  }
  upper <- n
  upper.cumu <- 0
  repeat{
    if (upper.cumu + dbinom(upper, n, 0.5) > (1-CI.width)/2) {break}
    upper.cumu <- upper.cumu + dbinom(upper, n, 0.5)
    upper <- upper - 1
  }
  if (upper == n){
    CI.upper = Inf
  }else{
    CI.upper <- sort(x)[upper+1]
  }

  #labels
  varname <- deparse(substitute(x))
  actualCIwidth <- 1 - lower.cumu - upper.cumu

  #output
  print(structure(list(title = "Sign test", varname = varname, H0 = H0, alternative = alternative, pval = pval,
                       targetCIwidth = CI.width, actualCIwidth = actualCIwidth, CI.lower = CI.lower,
                       CI.upper = CI.upper), class = "ANSMtest"))
}
