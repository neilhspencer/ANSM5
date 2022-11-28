print.ANSMtest <- function(toprint) {
  cat("\n")
  cat(paste0(toprint$title, " for ", toprint$varname))
  cat("\n\n")
  cat(paste0("Null hypothesis: theta = ", toprint$H0))
  cat("\n")
  if (toprint$alternative == "two.sided") {
    cat(paste0("Alternative hypothesis (2-sided): theta <> ", toprint$H0))
  }else if (toprint$alternative == "less") {
    cat(paste0("Alternative hypothesis (1-sided): theta < ", toprint$H0))
  }else{
    cat(paste0("Alternative hypothesis (1-sided): theta > ", toprint$H0))
  }
  cat("\n")
  if (round(toprint$pval, 5) == 0){
    cat(paste0("Exact p-value: ", toprint$pval))
  }else{
    cat(paste0("Exact p-value: ", round(toprint$pval,5)))
  }
  cat("\n\n")
  cat(paste0(100 * round(toprint$targetCIwidth, 5), "% Confidence Interval (",
             100 * round(toprint$actualCIwidth, 5), "% achieved)"))
  cat("\n")
  cat(paste0("(", toprint$CI.lower, ", ", toprint$CI.upper, ")"))
  cat("\n\n")
}
