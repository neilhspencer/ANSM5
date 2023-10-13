test_that("Example 11.3", {
  parentlimit <- c(0, 1, 2, 3, 4, 5, 6)
  reportedtime <- c(2.5, 3.1, 3.4, 4, 4.6, 5.1, 11.1)
  H0 <- 1
  expect_equal(spearman.beta(reportedtime ~ parentlimit, H0 = 1)$stat,
               0.56666667)
  expect_equal(spearman.beta(reportedtime ~ parentlimit, H0 = 1)$pval.exact,
               0.59484127)
  expect_equal(spearman.beta(reportedtime ~ parentlimit, H0 = 1, do.CI = TRUE,
                            seed = 1)$CI.mc.lower, 0.455)
  expect_equal(spearman.beta(reportedtime ~ parentlimit, H0 = 1, do.CI = TRUE,
                            seed = 1)$CI.mc.upper, 2.8083333)
})
