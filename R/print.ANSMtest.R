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
      cat(paste0("Statistic for test: ", toprint$pval.stat), "\n")
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

  #print pval.approx
  if (!is.null(toprint$pval.approx)){
    if (!is.null(toprint$pval.approx.stat)){
      cat(paste0("Statistic for approximate test: ", toprint$pval.approx.stat), "\n")
    }
    if (toprint$cont.corr == TRUE){
      pval.approx.label = "Approx. p-value (continuity correction used): "
    }else{
      pval.approx.label = "Approx. p-value (continuity correction not used): "
    }
    if (is.na(toprint$pval.approx)){
      cat("Approx. p-value cannot be calculated\n")
    }else if (round(toprint$pval.approx, 5) == 0){
      cat(paste0(pval.approx.label, "< 0.00001\n"))
    }else{
      cat(paste0(pval.approx.label, round(toprint$pval.approx,5), "\n"))
    }
  }
  if (!is.null(toprint$pval.approx.note)){cat(toprint$pval.approx.note, "\n")}

  #print pval.exact
  if (!is.null(toprint$pval.exact)){
    if (!is.null(toprint$pval.exact.stat)){
      cat(paste0("Statistic for exact test: ", toprint$pval.exact.stat), "\n")
    }
    if (is.na(toprint$pval.exact)){
      cat("Exact p-value cannot be calculated\n")
    }else if (round(toprint$pval.exact, 5) == 0){
      cat("Exact p-value: < 0.00001\n")
    }else{
      cat(paste0("Exact p-value: ", round(toprint$pval.exact,5), "\n"))
    }
  }
  if (!is.null(toprint$pval.exact.note)){cat(toprint$pval.exact.note, "\n")}

  #print space
  if (!is.null(toprint$pval) | !is.null(toprint$pval.note) |
      !is.null(toprint$pval.approx) | !is.null(toprint$pval.approx.note) |
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
