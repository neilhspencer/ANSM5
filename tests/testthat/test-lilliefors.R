test_that("Example 4.4", {
  expect_equal(lilliefors(ch4$ages,seed = 1)$pval.mc, 0.0057)
})

test_that("Example 4.5", {
  expect_equal(lilliefors(ch4$McAlpha, seed = 1)$pval.mc, 0)
})

test_that("Exercise 4.1", {
  expect_equal(lilliefors(ch4$McGamma, seed = 1)$pval.mc, 0)
})

test_that("Exercise 4.3", {
  expect_equal(lilliefors(ch4$visiting.supporters, seed = 1)$pval.mc,
               0.0087)
})

test_that("Example 5.1", {
  tmp <- lilliefors(ch5$LVF - ch5$RVF, seed = 1)
  expect_equal(tmp$pval.mc.stat, 0.15254377)
  expect_equal(tmp$pval.mc, 0.6096)
})

test_that("Example 5.3", {
  tmp <- lilliefors(ch5$bp, seed = 1)
  expect_equal(tmp$pval.mc.stat, 0.16076409)
  expect_equal(tmp$pval.mc, 0.1018)
  tmp <- lilliefors(ch5$bp.incorrect, seed = 1)
  expect_equal(tmp$pval.mc.stat, 0.2071137)
  expect_equal(tmp$pval.mc, 0.0093)
})

test_that("Exercise 6.15", {
  expect_equal(lilliefors(ch6$doseI.2, seed = 1)$pval.mc, 0.0074)
})
