test_that("Example 4.4", {
  expect_equal(shapiro.test.ANSM(ch4data$ages)$pval.asymp, 0.00117134809)
})

test_that("Example 5.1", {
  diffs <- c(-1, 2, 7, 14, 16, 16, 19, 20, 20, 22, 30, 33)
  expect_equal(shapiro.test.ANSM(diffs)$pval.asymp, 0.7289256)
})

test_that("Example 5.3", {
  bp1 <- c(-5, -5, 0, 2, 10, 15, 15, 15, 18, 20, 20, 20, 20, 22, 30, 30, 34, 40, 40, 40, 41, 47, 80, 85)
  expect_equal(shapiro.test.ANSM(bp1)$pval.asymp.stat, 0.90126828)
  expect_equal(shapiro.test.ANSM(bp1)$pval.asymp, 0.022900098)
  bp2 <- c(-5, -103, 0, 2, 10, 15, 15, 15, 18, 20, 20, 20, 20, 22, 30, 30, 34, 40, 40, 40, 41, 47, 80, 85)
  expect_equal(shapiro.test.ANSM(bp2)$pval.asymp.stat, 0.79465777)
  expect_equal(shapiro.test.ANSM(bp2)$pval.asymp, 0.000241231707)
})
