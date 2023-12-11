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
  expect_equal(lilliefors(ch5data$LVF - ch5data$RVF, seed = 1)$pval.mc.stat, 0.15254377)
  expect_equal(lilliefors(ch5data$LVF - ch5data$RVF, seed = 1)$pval.mc, 0.6096)
})

test_that("Example 5.3", {
  expect_equal(lilliefors(ch5data$bp, seed = 1)$pval.mc.stat, 0.16076409)
  expect_equal(lilliefors(ch5data$bp, seed = 1)$pval.mc, 0.1018)
  expect_equal(lilliefors(ch5data$bp.incorrect, seed = 1)$pval.mc.stat,
               0.2071137)
  expect_equal(lilliefors(ch5data$bp.incorrect, seed = 1)$pval.mc, 0.0093)
})
