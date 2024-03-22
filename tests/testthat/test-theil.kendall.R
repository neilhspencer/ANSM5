test_that("Example 11.4", {
  tmp <- theil.kendall(ch11$reportedtime, ch11$parentlimit, H0 = 1,
                       do.asymp = TRUE, do.mc = TRUE, do.CI = TRUE, seed = 1)
  expect_equal(tmp$stat, 0.56666667)
  expect_equal(tmp$pval.exact, 0.081746032)
  expect_equal(tmp$pval.mc, 0.08207)
  expect_equal(tmp$CI.mc.lower, 0.5)
  expect_equal(tmp$CI.mc.upper, 2.3666667)
  expect_equal(tmp$pval.asymp, 0.176474291)
})

test_that("Example 11.5", {
  tmp <- theil.kendall(ch11$length, ch11$age, do.abbreviated = TRUE,
                       do.CI = TRUE, seed = 1)
  expect_equal(tmp$stat, 2.6111111)
  expect_equal(tmp$CI.mc.lower, 1.875)
  expect_equal(tmp$CI.mc.upper, 3.7666667)
  tmp <- theil.kendall(ch11$length, ch11$age, do.CI = TRUE, seed = 1)
  expect_equal(tmp$stat, 2.6458333)
  expect_equal(tmp$CI.mc.lower, 2.0)
  expect_equal(tmp$CI.mc.upper, 3.2727273)
})

test_that("Example 11.6", {
  expect_equal(theil.kendall(ch11$reportedtime, ch11$parentlimit,
                             do.alpha = TRUE)$stat.note,
               paste0("Estimate of alpha using median of d_i: 2.33333\n",
                      "Hodges–Lehmann estimator of alpha: 2.40000\n\n",
                      "Neither exact, asymptotic nor Monte Carlo test requested"))
})

test_that("Example 11.7", {
  tmp <- theil.kendall(ch11$reportedtime.2, ch11$parentlimit.2, do.CI = TRUE,
                       seed = 1)
  expect_equal(tmp$stat, 0.9347826)
  expect_equal(tmp$CI.mc.lower, 0.5)
  expect_equal(tmp$CI.mc.upper, 1.17826087)
})

test_that("Exercise 11.3", {
  expect_equal(theil.kendall(ch11$rotten, ch11$days.stored)$stat, 2.375)
})

test_that("Exercise 11.6", {
  tmp <- theil.kendall(ch11$ESMS, ch11$ERA, do.abbreviated = TRUE, do.CI = TRUE,
                       seed = 1)
  expect_equal(tmp$stat, 0.8888889)
  expect_equal(tmp$CI.mc.lower, 0.0098039216)
  expect_equal(tmp$CI.mc.upper, 1.46153846)
})

test_that("Exercise 11.8", {
  tmp <- theil.kendall(ch11$ammonia, ch11$depth, H0 = 1, do.CI = TRUE, seed = 1)
  expect_equal(tmp$stat, 0.006266667)
  expect_equal(tmp$CI.mc.lower, 0.00183908046)
  expect_equal(tmp$CI.mc.lower, 0.0104)
})

test_that("Exercise 11.9", {
  expect_equal(theil.kendall(ch11$weight.gain.A, ch11$food.weight.A,
                             do.abbreviated = TRUE)$stat, 0.17105263)
  expect_equal(theil.kendall(ch11$weight.gain.B, ch11$food.weight.B,
                             do.abbreviated = TRUE)$stat, 0.35365854)
})

test_that("Exercise 11.10", {
  expect_equal(theil.kendall(ch11$SW.England, ch11$N.Scotland)$stat, 1.024)
  expect_equal(theil.kendall(ch11$N.Scotland, ch11$SW.England)$stat, 0.97196262)
  expect_equal(median(
    ch11$N.Scotland - theil.kendall(ch11$N.Scotland, ch11$SW.England)$stat *
      ch11$SW.England), 20.490654)
  expect_equal(median(
    ch11$N.Scotland - theil.kendall(ch11$N.Scotland, ch11$SW.England)$stat *
      ch11$SW.England) + theil.kendall(ch11$N.Scotland, ch11$SW.England)$stat *
      122, 139.070093)
})
