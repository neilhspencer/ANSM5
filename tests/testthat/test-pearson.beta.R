test_that("Example 11.2", {
  Ex11.2 <- data.frame(parentlimit = c(0, 1, 2, 3, 4, 5, 6),
                       reportedtime = c(2.5, 3.1, 3.4, 4, 4.6, 5.1, 11.1))
  H0 <- 1
  expect_equal(pearson.beta(reportedtime ~ parentlimit, Ex11.2, H0 = 1)$stat,
               1.10714286)
  expect_equal(pearson.beta(reportedtime ~ parentlimit, Ex11.2,
                            H0 = 1)$pval.exact, 0.79246032)
  expect_equal(pearson.beta(reportedtime[1:6] ~ parentlimit[1:6], Ex11.2,
                            H0 = 1)$pval.exact, 0.0027777778)
  expect_equal(pearson.beta(reportedtime ~ parentlimit, Ex11.2, H0 = 1,
                            do.CI = TRUE, seed = 1)$CI.mc.lower, 0.49666667)
  expect_equal(pearson.beta(reportedtime ~ parentlimit, Ex11.2, H0 = 1,
                            do.CI = TRUE, seed = 1)$CI.mc.upper, 1.94787234)
})
