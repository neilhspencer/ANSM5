print.ANSMtest <- function(toprint) {
  #print space
  cat("\n")

  #print test/variable name
  cat(paste0(toprint$title, " for ", toprint$varname), "\n")

  #print space
  cat("\n")

  #print hypotheses
  if (!is.null(toprint$H0)){
    if (is.character(toprint$H0)){
      cat(toprint$H0)
    }else{
      cat(paste0("Null hypothesis: theta = ", toprint$H0), "\n")
      if (toprint$alternative == "two.sided") {
        cat(paste0("Alternative hypothesis (2-sided): theta != ", toprint$H0), "\n")
      }else if (toprint$alternative == "less") {
        cat(paste0("Alternative hypothesis (1-sided): theta < ", toprint$H0), "\n")
      }else{
        cat(paste0("Alternative hypothesis (1-sided): theta > ", toprint$H0), "\n")
      }
    }
    #print space
    cat("\n")
  }

  #print pval.exact
  if (!is.null(toprint$pval.exact)){
    if (!is.null(toprint$pval.exact.stat)){
      cat(paste0("Statistic for exact test: ", round(toprint$pval.exact.stat,5)), "\n")
    }
    if (is.na(toprint$pval.exact)){
      cat("Exact p-value cannot be calculated\n")
    }else if (toprint$pval.exact < 0.00001){
      cat("Exact p-value: < 0.00001\n")
    }else{
      cat(paste0("Exact p-value: ", sprintf("%.5f",toprint$pval.exact), "\n"))
    }
  }
  if (!is.null(toprint$pval.exact.note)){cat(toprint$pval.exact.note, "\n")}

  #print exact CI
  if (!is.null(toprint$CI.exact.lower) && !is.null(toprint$CI.exact.upper)){
    if (is.na(toprint$CI.exact.lower) | is.na(toprint$CI.exact.upper)){
      cat(paste0("Exact ", 100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval cannot be calculated"), "\n")
    }else if (!is.null(toprint$actualCIwidth.exact)){
      cat(paste0("Exact ", 100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval (",
                 100 * round(toprint$actualCIwidth.exact, 5), "% achieved)"), "\n")
      cat(paste0("(", sprintf("%.5f", toprint$CI.exact.lower), ", ",
                 sprintf("%.5f", toprint$CI.exact.upper), ")"), "\n")
    }else{
      cat(paste0("Exact ", 100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval"), "\n")
      cat(paste0("(", sprintf("%.5f", toprint$CI.exact.lower), ", ",
                 sprintf("%.5f", toprint$CI.exact.upper), ")"), "\n")
    }
  }
  if (!is.null(toprint$CI.exact.note)){cat(toprint$CI.exact.note, "\n")}

  #print space
  if (!is.null(toprint$pval.exact) | !is.null(toprint$pval.exact.note) |
      !is.null(toprint$CI.exact.lower) | !is.null(toprint$CI.exact.note))
  {cat("\n")}

  #print pval
  if (!is.null(toprint$pval)){
    if (!is.null(toprint$pval.stat)){
      cat(paste0("Statistic for test: ", round(toprint$pval.stat,5)), "\n")
    }
    if (is.na(toprint$pval)){
      cat("p-value cannot be calculated\n")
    }else if (toprint$pval < 0.00001){
      cat("p-value: < 0.00001\n")
    }else{
      cat(paste0("p-value: ", sprintf("%.5f",toprint$pval), "\n"))
    }
  }
  if (!is.null(toprint$pval.note)){cat(toprint$pval.note, "\n")}

  #print space
  if (!is.null(toprint$pval) | !is.null(toprint$pval.note))
  {cat("\n")}

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
    }else if (toprint$pval.asymp < 0.00001){
      cat(paste0(pval.asymp.label, "< 0.00001\n"))
    }else{
      cat(paste0(pval.asymp.label, sprintf("%.5f",toprint$pval.asymp), "\n"))
    }
  }
  if (!is.null(toprint$pval.asymp.note)){cat(toprint$pval.asymp.note, "\n")}

  #print asymp CI
  if (!is.null(toprint$CI.asymp.lower) && !is.null(toprint$CI.asymp.upper)){
    if (is.na(toprint$CI.asymp.lower) | is.na(toprint$CI.asymp.upper)){
      cat(paste0("Asymptotic ", 100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval cannot be calculated"), "\n")
    }else{
      cat(paste0("Asymptotic ", 100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval"), "\n")
      cat(paste0("(", sprintf("%.5f", toprint$CI.asymp.lower), ", ",
                 sprintf("%.5f", toprint$CI.asymp.upper), ")"), "\n")
    }
  }
  if (!is.null(toprint$CI.asymp.note)){cat(toprint$CI.asymp.note, "\n")}

  #print space
  if (!is.null(toprint$pval.asymp) | !is.null(toprint$pval.asymp.note) |
      !is.null(toprint$CI.asymp.lower) | !is.null(toprint$CI.asymp.note))
  {cat("\n")}

  #print pval.mc
  if (!is.null(toprint$pval.mc)){
    if (!is.null(toprint$pval.mc.stat)){
      cat(paste0("Statistic for Monte Carlo test: ", round(toprint$pval.mc.stat,5)), "\n")
    }
    pval.mc.label = paste0("Monte Carlo p-value (", sprintf("%1$d",toprint$nsims.mc),
                           " simulations): ")
    if (toprint$pval.mc < 0.00001){
      cat(paste0(pval.mc.label, "< 0.00001\n"))
    }else{
      if (toprint$nsims.mc >= 100000){
        cat(paste0(pval.mc.label, sprintf("%.5f",toprint$pval.mc), "\n"))
      }else if (toprint$nsims.mc >= 10000){
        cat(paste0(pval.mc.label, sprintf("%.4f",toprint$pval.mc), "\n"))
      }else{
        cat(paste0(pval.mc.label, sprintf("%.3f",toprint$pval.mc), "\n"))
      }
    }
  }
  if (!is.null(toprint$pval.mc.note)){cat(toprint$pval.mc.note, "\n")}

  #print Monte Carlo CI
  if (!is.null(toprint$CI.mc.lower) && !is.null(toprint$CI.mc.upper)){
    if (is.na(toprint$CI.mc.lower) | is.na(toprint$CI.mc.upper)){
      cat(paste0("Monte Carlo ", 100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval cannot be calculated"), "\n")
    }else{
      cat(paste0("Monte Carlo ", 100 * round(toprint$targetCIwidth, 5),
                 "% Confidence Interval (", toprint$nsims.mc, " simulations)"), "\n")
      cat(paste0("(", toprint$CI.mc.lower, ", ", toprint$CI.mc.upper, ")"), "\n")
    }
  }
  if (!is.null(toprint$CI.mc.note)){cat(toprint$CI.mc.note, "\n")}

  #print space
  if (!is.null(toprint$pval.mc) | !is.null(toprint$pval.mc.note) |
      !is.null(toprint$CI.mc.lower) | !is.null(toprint$CI.mc.note))
  {cat("\n")}

  #print test note
  if (!is.null(toprint$test.note)){
    cat(toprint$test.note, "\n")
    cat("\n")
  }
}
