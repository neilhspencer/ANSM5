test_that("Example 11.4", {
  expect_equal(theil.kendall(ch11$reportedtime, ch11$parentlimit, H0 = 1)$stat,
               0.56666667)
  expect_equal(theil.kendall(ch11$reportedtime, ch11$parentlimit,
                             H0 = 1)$pval.exact, 0.081746032)
  expect_equal(theil.kendall(ch11$reportedtime, ch11$parentlimit, H0 = 1,
                             do.mc = TRUE, seed = 1)$pval.mc, 0.08207)
  expect_equal(theil.kendall(ch11$reportedtime, ch11$parentlimit,
                             do.CI = TRUE)$CI.exact.lower, 0.5)
  expect_equal(theil.kendall(ch11$reportedtime, ch11$parentlimit,
                             do.CI = TRUE)$CI.exact.upper, 1.925)
  expect_equal(theil.kendall(ch11$reportedtime, ch11$parentlimit, H0 = 1,
                             do.asymp = TRUE)$pval.asymp, 0.176474291)
})

test_that("Example 11.5", {
  expect_equal(theil.kendall(ch11$length, ch11$age, do.abbreviated = TRUE)$stat,
               2.6111111)
  expect_equal(theil.kendall(ch11$length, ch11$age, do.abbreviated = TRUE,
                             do.CI = TRUE, seed = 1)$CI.exact.lower, 1.875)
  expect_equal(theil.kendall(ch11$length, ch11$age, do.abbreviated = TRUE,
                             do.CI = TRUE, seed = 1)$CI.exact.upper, 3.7666667)
  expect_equal(theil.kendall(ch11$length, ch11$age)$stat, 2.6458333)
  expect_equal(theil.kendall(ch11$length, ch11$age, do.CI = TRUE,
                             seed = 1)$CI.mc.lower, 2.0)
  expect_equal(theil.kendall(ch11$length, ch11$age, do.CI = TRUE,
                             seed = 1)$CI.mc.upper, 3.2727273)
})

test_that("Example 11.6", {
  expect_equal(theil.kendall(ch11$reportedtime, ch11$parentlimit,
                             do.alpha = TRUE)$stat.note,
               paste0("Estimate of alpha using median of d_i: 2.33333\n",
                      "Hodges–Lehmann estimator of alpha: 2.40000\n\n",
                      "Neither exact, asymptotic nor Monte Carlo test requested"))
})

test_that("Example 11.7", {
  expect_equal(theil.kendall(ch11$reportedtime.2, ch11$parentlimit.2)$stat,
               0.9347826)
  expect_equal(theil.kendall(ch11$reportedtime.2, ch11$parentlimit.2,
                             do.CI = TRUE, seed = 1)$CI.mc.lower, 0.5)
  expect_equal(theil.kendall(ch11$reportedtime.2, ch11$parentlimit.2,
                             do.CI = TRUE, seed = 1)$CI.mc.upper, 1.17826087)
})
