test_that("Example 11.4", {
  parentlimit <- c(0, 1, 2, 3, 4, 5, 6)
  reportedtime <- c(2.5, 3.1, 3.4, 4, 4.6, 5.1, 11.1)
  H0 <- 1
  expect_equal(theil.kendall(reportedtime ~ parentlimit, H0 = 1)$stat,
               0.56666667)
  expect_equal(theil.kendall(reportedtime ~ parentlimit, H0 = 1)$pval.exact,
               0.081746032)
  expect_equal(theil.kendall(reportedtime ~ parentlimit, H0 = 1, do.mc = TRUE,
                             seed = 1)$pval.mc, 0.08207)
  expect_equal(theil.kendall(reportedtime ~ parentlimit, do.CI = TRUE,
                             seed = 1)$CI.mc.lower, 0.5)
  expect_equal(theil.kendall(reportedtime ~ parentlimit, do.CI = TRUE,
                             seed = 1)$CI.mc.upper, 2.3666667)
  expect_equal(theil.kendall(reportedtime ~ parentlimit, H0 = 1,
                             do.asymp = TRUE)$pval.asymp, 0.176474291)
})

test_that("Example 11.5", {
  age <- c(4, 5, 6, 7, 8, 9, 10, 11, 12,   13, 14, 15, 16, 17, 18, 19, 20)
  len <- c(40, 45, 51, 55, 60, 67, 68, 65, 71, 74, 76, 76, 78, 83, 82, 85, 89)
  expect_equal(theil.kendall(len ~ age)$stat, 2.6458333)
  expect_equal(theil.kendall(len ~ age, do.CI = TRUE)$CI.mc.lower, 2.0)
  expect_equal(theil.kendall(len ~ age, do.CI = TRUE)$CI.mc.upper, 3.2727273)
})
