print.ANSMtest <- function(to.print) {
  #print space
  cat("\n")

  #print test/variable name
  if (is.null(to.print$varname2)){
    cat(paste0(to.print$title, " for ", to.print$varname1), "\n")
  }else{
    cat(paste0(to.print$title, " for ", to.print$varname1, " and ",
        to.print$varname2, "\n"))
  }

  #print space
  cat("\n")

  #print hypotheses
  if (!is.null(to.print$H0)){
    if (is.character(to.print$H0)){
      cat(to.print$H0)
    }else{
      cat(paste0("Null hypothesis: theta = ", to.print$H0), "\n")
      if (to.print$alternative == "two.sided") {
        cat(paste0("Alternative hypothesis (2-sided): theta != ", to.print$H0), "\n")
      }else if (to.print$alternative == "less") {
        cat(paste0("Alternative hypothesis (1-sided): theta < ", to.print$H0), "\n")
      }else{
        cat(paste0("Alternative hypothesis (1-sided): theta > ", to.print$H0), "\n")
      }
    }
    #print space
    cat("\n")
  }

  #print pval.exact
  if (!is.null(to.print$pval.exact)){
    if (!is.null(to.print$pval.exact.stat)){
      if (is.character(to.print$pval.exact.stat)){
        cat(paste0("Statistic for exact test: ", to.print$pval.exact.stat), "\n")
      }else{
        cat(paste0("Statistic for exact test: ", round(to.print$pval.exact.stat,5)), "\n")
      }
    }
    if (is.na(to.print$pval.exact)){
      cat("Exact p-value cannot be calculated\n")
    }else if (to.print$pval.exact < 0.00001){
      cat("Exact p-value: < 0.00001\n")
    }else{
      cat(paste0("Exact p-value: ", sprintf("%.5f",to.print$pval.exact), "\n"))
    }
  }
  if (!is.null(to.print$pval.exact.note)){cat(to.print$pval.exact.note, "\n")}

  #print exact CI
  if (!is.null(to.print$CI.exact.lower) && !is.null(to.print$CI.exact.upper)){
    if (is.na(to.print$CI.exact.lower) | is.na(to.print$CI.exact.upper)){
      cat(paste0("Exact ", 100 * round(to.print$targetCIwidth, 5),
                 "% Confidence Interval cannot be calculated"), "\n")
    }else if (!is.null(to.print$actualCIwidth.exact)){
      cat(paste0("Exact ", 100 * round(to.print$targetCIwidth, 5),
                 "% Confidence Interval (",
                 100 * round(to.print$actualCIwidth.exact, 5), "% achieved)"), "\n")
      cat(paste0("(", sprintf("%.5f", to.print$CI.exact.lower), ", ",
                 sprintf("%.5f", to.print$CI.exact.upper), ")"), "\n")
    }else{
      cat(paste0("Exact ", 100 * round(to.print$targetCIwidth, 5),
                 "% Confidence Interval"), "\n")
      cat(paste0("(", sprintf("%.5f", to.print$CI.exact.lower), ", ",
                 sprintf("%.5f", to.print$CI.exact.upper), ")"), "\n")
    }
  }
  if (!is.null(to.print$CI.exact.note)){cat(to.print$CI.exact.note, "\n")}

  #print space
  if (!is.null(to.print$pval.exact) | !is.null(to.print$pval.exact.note) |
      !is.null(to.print$CI.exact.lower) | !is.null(to.print$CI.exact.note))
  {cat("\n")}

  #print pval
  if (!is.null(to.print$pval)){
    if (!is.null(to.print$pval.stat)){
      if (is.character(to.print$pval.stat)){
        cat(paste0("Statistic for test: ", to.print$pval.stat), "\n")
      }else{
        cat(paste0("Statistic for test: ", round(to.print$pval.stat,5)), "\n")
      }
    }
    if (is.na(to.print$pval)){
      cat("p-value cannot be calculated\n")
    }else if (to.print$pval < 0.00001){
      cat("p-value: < 0.00001\n")
    }else{
      cat(paste0("p-value: ", sprintf("%.5f",to.print$pval), "\n"))
    }
  }
  if (!is.null(to.print$pval.note)){cat(to.print$pval.note, "\n")}

  #print space
  if (!is.null(to.print$pval) | !is.null(to.print$pval.note))
  {cat("\n")}

  #print pval.asymp
  if (!is.null(to.print$pval.asymp)){
    if (!is.null(to.print$pval.asymp.stat)){
      if (is.character(to.print$pval.asymp.stat)){
        cat(paste0("Statistic for asymptotic test: ", to.print$pval.asymp.stat), "\n")
      }else{
        cat(paste0("Statistic for asymptotic test: ", round(to.print$pval.asymp.stat,5)), "\n")
      }
    }
    if (!is.null(to.print$cont.corr)){
      if (to.print$cont.corr == TRUE){
        pval.asymp.label = "Asymptotic p-value (continuity correction used): "
      }else{
        pval.asymp.label = "Asymptotic p-value (continuity correction not used): "
      }
    }else{
      pval.asymp.label = "Asymptotic p-value: "
    }
    if (is.na(to.print$pval.asymp)){
      cat("Asymptotic p-value cannot be calculated\n")
    }else if (to.print$pval.asymp < 0.00001){
      cat(paste0(pval.asymp.label, "< 0.00001\n"))
    }else{
      cat(paste0(pval.asymp.label, sprintf("%.5f",to.print$pval.asymp), "\n"))
    }
  }
  if (!is.null(to.print$pval.asymp.note)){cat(to.print$pval.asymp.note, "\n")}

  #print asymp CI
  if (!is.null(to.print$CI.asymp.lower) && !is.null(to.print$CI.asymp.upper)){
    if (is.na(to.print$CI.asymp.lower) | is.na(to.print$CI.asymp.upper)){
      cat(paste0("Asymptotic ", 100 * round(to.print$targetCIwidth, 5),
                 "% Confidence Interval cannot be calculated"), "\n")
    }else{
      cat(paste0("Asymptotic ", 100 * round(to.print$targetCIwidth, 5),
                 "% Confidence Interval"), "\n")
      cat(paste0("(", sprintf("%.5f", to.print$CI.asymp.lower), ", ",
                 sprintf("%.5f", to.print$CI.asymp.upper), ")"), "\n")
    }
  }
  if (!is.null(to.print$CI.asymp.note)){cat(to.print$CI.asymp.note, "\n")}

  #print space
  if (!is.null(to.print$pval.asymp) | !is.null(to.print$pval.asymp.note) |
      !is.null(to.print$CI.asymp.lower) | !is.null(to.print$CI.asymp.note))
  {cat("\n")}

  #print pval.mc
  if (!is.null(to.print$pval.mc)){
    if (!is.null(to.print$pval.mc.stat)){
      if (is.character(to.print$pval.mc.stat)){
        cat(paste0("Statistic for Monte Carlo test: ", to.print$pval.mc.stat), "\n")
      }else{
        cat(paste0("Statistic for Monte Carlo test: ", round(to.print$pval.mc.stat,5)), "\n")
      }
    }
    pval.mc.label = paste0("Monte Carlo p-value (", sprintf("%1$d",to.print$nsims.mc),
                           " simulations): ")
    if (to.print$pval.mc < 0.00001){
      cat(paste0(pval.mc.label, "< 0.00001\n"))
    }else{
      if (to.print$nsims.mc >= 100000){
        cat(paste0(pval.mc.label, sprintf("%.5f",to.print$pval.mc), "\n"))
      }else if (to.print$nsims.mc >= 10000){
        cat(paste0(pval.mc.label, sprintf("%.4f",to.print$pval.mc), "\n"))
      }else{
        cat(paste0(pval.mc.label, sprintf("%.3f",to.print$pval.mc), "\n"))
      }
    }
  }
  if (!is.null(to.print$pval.mc.note)){cat(to.print$pval.mc.note, "\n")}

  #print Monte Carlo CI
  if (!is.null(to.print$CI.mc.lower) && !is.null(to.print$CI.mc.upper)){
    if (is.na(to.print$CI.mc.lower) | is.na(to.print$CI.mc.upper)){
      cat(paste0("Monte Carlo ", 100 * round(to.print$targetCIwidth, 5),
                 "% Confidence Interval cannot be calculated"), "\n")
    }else{
      cat(paste0("Monte Carlo ", 100 * round(to.print$targetCIwidth, 5),
                 "% Confidence Interval (", to.print$nsims.mc, " simulations)"), "\n")
      cat(paste0("(", to.print$CI.mc.lower, ", ", to.print$CI.mc.upper, ")"), "\n")
    }
  }
  if (!is.null(to.print$CI.mc.note)){cat(to.print$CI.mc.note, "\n")}

  #print space
  if (!is.null(to.print$pval.mc) | !is.null(to.print$pval.mc.note) |
      !is.null(to.print$CI.mc.lower) | !is.null(to.print$CI.mc.note))
  {cat("\n")}

  #print test note
  if (!is.null(to.print$test.note)){
    cat(to.print$test.note, "\n")
    cat("\n")
  }
}
