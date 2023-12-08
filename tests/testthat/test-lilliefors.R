test_that("Example 4.4", {
  expect_equal(lilliefors(ch4data$ages,seed = 1)$pval.mc, 0.0057)
})

test_that("Example 4.5", {
  expect_equal(lilliefors(ch4data$McAlpha, seed = 1)$pval.mc, 0)
})

test_that("Exercise 4.1", {
  expect_equal(lilliefors(ch4data$McGamma, seed = 1)$pval.mc, 0)
})

test_that("Exercise 4.3", {
  expect_equal(lilliefors(ch4data$visiting.supporters, seed = 1)$pval.mc,
               0.0087)
})

test_that("Example 5.1", {
  diffs <- c(-1, 2, 7, 14, 16, 16, 19, 20, 20, 22, 30, 33)
  expect_equal(lilliefors(diffs, seed = 1)$pval.mc.stat, 0.15254377)
  expect_equal(lilliefors(diffs, seed = 1)$pval.mc, 0.6096)
})

test_that("Example 5.3", {
  bp1 <- c(-5, -5, 0, 2, 10, 15, 15, 15, 18, 20, 20, 20, 20, 22, 30, 30, 34, 40, 40, 40, 41, 47, 80, 85)
  expect_equal(lilliefors(bp1, seed = 1)$pval.mc.stat, 0.16076409)
  expect_equal(lilliefors(bp1, seed = 1)$pval.mc, 0.1018)
  bp2<- c(-5, -103, 0, 2, 10, 15, 15, 15, 18, 20, 20, 20, 20, 22, 30, 30, 34, 40, 40, 40, 41, 47, 80, 85)
  expect_equal(lilliefors(bp2, seed = 1)$pval.mc.stat, 0.2071137)
  expect_equal(lilliefors(bp2, seed = 1)$pval.mc, 0.0093)
})
