print.ANSMtest <- function(toprint) {
  cat("\n")

  cat(paste0(toprint$title, " for ", toprint$varname))

  cat("\n\n")

  cat(paste0("Null hypothesis: theta = ", toprint$H0))

  cat("\n")

  if (toprint$alternative == "two.sided") {
    cat(paste0("Alternative hypothesis (2-sided): theta <> ", toprint$H0), "\n")
  }else if (toprint$alternative == "less") {
    cat(paste0("Alternative hypothesis (1-sided): theta < ", toprint$H0), "\n")
  }else{
    cat(paste0("Alternative hypothesis (1-sided): theta > ", toprint$H0), "\n")
  }

  if (!is.null(toprint$pval)){
    if (round(toprint$pval, 5) == 0){
      cat("p-value: < 0.00001\n")
    }else{
      cat(paste0("p-value: ", round(toprint$pval,5), "\n"))
    }
  }
  if (!is.null(toprint$pval.approx)){
    if (toprint$cont.corr == TRUE){
      pval.approx.label = "Approx. p-value (continuity correction used): "
    }else{
      pval.approx.label = "Approx. p-value (continuity correction not used): "
    }
    if (round(toprint$pval.approx, 5) == 0){
      cat(paste0(pval.approx.label, "< 0.00001\n"))
    }else{
      cat(paste0(pval.approx.label, round(toprint$pval.approx,5), "\n"))
    }
    if (!is.null(toprint$pval.exact)){
      if (round(toprint$pval.exact, 5) == 0){
        cat("Exact p-value: < 0.00001\n")
      }else{
        cat(paste0("Exact p-value: ", round(toprint$pval.exact,5), "\n"))
      }
    }
  }

  cat("\n")

  cat(paste0(100 * round(toprint$targetCIwidth, 5), "% Confidence Interval (",
             100 * round(toprint$actualCIwidth, 5), "% achieved)"), "\n")

  cat(paste0("(", toprint$CI.lower, ", ", toprint$CI.upper, ")"), "\n")

  cat("\n")
}
