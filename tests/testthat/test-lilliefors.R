test_that("Example 4.4", {
  ages <- c(11, 13, 14, 22, 29, 30, 41, 41, 52, 55, 56, 59, 65, 65, 66, 74, 74,
            75, 77, 81, 82, 82, 82, 82, 83, 85, 85, 87, 87, 88)
  expect_equal(lilliefors(ages,seed = 1)$pval.mc, 0.0057)
})

test_that("Example 4.5", {
  McAlpha <- c(0, 0, 1, 2, 3, 9, 14, 22, 23, 29, 33, 41, 41, 42, 44, 52, 56, 57,
               58, 58, 60, 62, 63, 64, 65, 69, 72, 72, 73, 74, 74, 75, 75, 75,
               77, 77, 78, 78, 79, 79, 80, 81, 81, 81, 81, 82, 82, 83, 84, 84,
               85, 86, 87, 87, 88, 90, 92, 93, 95)
  expect_equal(lilliefors(McAlpha, seed = 1)$pval.mc, 0)
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
