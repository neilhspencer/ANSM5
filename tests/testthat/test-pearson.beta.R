test_that("Example 11.2", {
  expect_equal(pearson.beta(ch11$reportedtime, ch11$parentlimit,
                            H0 = 1)$stat, 1.10714286)
  expect_equal(pearson.beta(ch11$reportedtime, ch11$parentlimit,
                            H0 = 1)$pval.exact.stat, 0.139175554)
  expect_equal(pearson.beta(ch11$reportedtime, ch11$parentlimit,
                            H0 = 1)$pval.exact, 0.79246032)
  expect_equal(pearson.beta(ch11$reportedtime[1:6], ch11$parentlimit[1:6],
                            H0 = 1)$pval.exact, 0.0027777778)
  expect_equal(pearson.beta(ch11$reportedtime, ch11$parentlimit, H0 = 1,
                            do.CI = TRUE, seed = 1)$CI.mc.lower, 0.49666667)
  expect_equal(pearson.beta(ch11$reportedtime, ch11$parentlimit, H0 = 1,
                            do.CI = TRUE, seed = 1)$CI.mc.upper, 1.94787234)
})
