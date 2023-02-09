print.ANSMtest <- function(toprint) {
  #print space
  cat("\n")

  #print test/variable name
  cat(paste0(toprint$title, " for ", toprint$varname), "\n")

  #print space
  cat("\n")

  #print hypotheses
  cat(paste0("Null hypothesis: theta = ", toprint$H0), "\n")
  if (toprint$alternative == "two.sided") {
    cat(paste0("Alternative hypothesis (2-sided): theta != ", toprint$H0), "\n")
  }else if (toprint$alternative == "less") {
    cat(paste0("Alternative hypothesis (1-sided): theta < ", toprint$H0), "\n")
  }else{
    cat(paste0("Alternative hypothesis (1-sided): theta > ", toprint$H0), "\n")
  }

  #print space
  cat("\n")

  #print pval
  if (!is.null(toprint$pval)){
    if (!is.null(toprint$pval.stat)){
      cat(paste0("Statistic for test: ", round(toprint$pval.stat,5)), "\n")
    }
    if (is.na(toprint$pval)){
      cat("p-value cannot be calculated\n")
    }else if (round(toprint$pval, 5) == 0){
      cat("p-value: < 0.00001\n")
    }else{
      cat(paste0("p-value: ", round(toprint$pval,5), "\n"))
    }
  }
  if (!is.null(toprint$pval.note)){cat(toprint$pval.note, "\n")}

  #print pval.asymp
  if (!is.null(toprint$pval.asymp)){
    if (!is.null(toprint$pval.asymp.stat)){
      cat(paste0("Statistic for asymptotic test: ", round(toprint$pval.asymp.stat,5)), "\n")
    }
    if (!is.null(toprint$cont.corr)){
      if (toprint$cont.corr == TRUE){
        pval.asymp.label = "Asymptotic p-value (continuity correction used): "
      }else{
        pval.asymp.label = "Asymptotic p-value (continuity correction not used): "
      }
    }else{
      pval.asymp.label = "Asymptotic p-value: "
    }
    if (is.na(toprint$pval.asymp)){
      cat("Asymptotic p-value cannot be calculated\n")
    }else if (round(toprint$pval.asymp, 5) == 0 | round(toprint$pval.asymp,5) == 1e-05){
      cat(paste0(pval.asymp.label, "< 0.00001\n"))
    }else{
      cat(paste0(pval.asymp.label, round(toprint$pval.asymp,5), "\n"))
    }
  }
  if (!is.null(toprint$pval.asymp.note)){cat(toprint$pval.asymp.note, "\n")}

  #print pval.exact
  if (!is.null(toprint$pval.exact)){
    if (!is.null(toprint$pval.exact.stat)){
      cat(paste0("Statistic for exact test: ", round(toprint$pval.exact.stat,5)), "\n")
    }
    if (is.na(toprint$pval.exact)){
      cat("Exact p-value cannot be calculated\n")
    }else if (round(toprint$pval.exact, 5) == 0 | round(toprint$pval.exact,5) == 1e-05){
      cat("Exact p-value: < 0.00001\n")
    }else{
      cat(paste0("Exact p-value: ", round(toprint$pval.exact,5), "\n"))
    }
  }
  if (!is.null(toprint$pval.exact.note)){cat(toprint$pval.exact.note, "\n")}

  #print space
  if (!is.null(toprint$pval) | !is.null(toprint$pval.note) |
      !is.null(toprint$pval.asymp) | !is.null(toprint$pval.asymp.note) |
      !is.null(toprint$pval.exact) | !is.null(toprint$pval.exact.note))
    {cat("\n")}

  #print CI
  if (!is.null(toprint$CI.lower) && !is.null(toprint$CI.upper)){
    if (is.na(toprint$CI.lower) | is.na(toprint$CI.upper)){
      cat(paste0(100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval cannot be calculated"), "\n")
    }else if (!is.null(toprint$actualCIwidth)){
      cat(paste0(100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval (",
                 100 * round(toprint$actualCIwidth, 5), "% achieved)"), "\n")
      cat(paste0("(", toprint$CI.lower, ", ", toprint$CI.upper, ")"), "\n")
    }else{
      cat(paste0(100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval"), "\n")
      cat(paste0("(", toprint$CI.lower, ", ", toprint$CI.upper, ")"), "\n")
    }
  }
  if (!is.null(toprint$CI.note)){cat(toprint$CI.note, "\n")}

  #print space
  if (!is.null(toprint$actualCIwidth) | !is.null(toprint$CI.note)) {cat("\n")}

  #print test note
  if (!is.null(toprint$test.note)){
    cat(toprint$test.note, "\n")
    cat("\n")
  }
}
