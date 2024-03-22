test_that("Example 11.2", {
  tmp <- pearson.beta(ch11$reportedtime, ch11$parentlimit, H0 = 1)
  expect_equal(tmp$stat, 1.10714286)
  expect_equal(tmp$pval.exact.stat, 0.139175554)
  expect_equal(tmp$pval.exact, 0.79246032)
  expect_equal(pearson.beta(ch11$reportedtime[1:6], ch11$parentlimit[1:6],
                            H0 = 1)$pval.exact, 0.0027777778)
  tmp <- pearson.beta(ch11$reportedtime, ch11$parentlimit, H0 = 1, do.CI = TRUE,
                      seed = 1)
  expect_equal(tmp$CI.mc.lower, 0.49666667)
  expect_equal(tmp$CI.mc.upper, 1.94787234)
})
