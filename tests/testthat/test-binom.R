test_that("Example 4.6", {
  expect_equal(binom(3, 20)$CI.exact.lower, 0.03207)
  expect_equal(binom(3, 20)$CI.exact.upper, 0.37893)
  expect_equal(binom(3, 20)$actualCIwidth.exact, 0.95000353)
})

test_that("Example 4.7", {
  expect_equal(binom(3, 20, CI.width = 0.90)$CI.exact.lower, 0.04216)
  expect_equal(binom(3, 20, CI.width = 0.90)$CI.exact.upper, 0.34367)
  expect_equal(binom(3, 20, CI.width = 0.90)$actualCIwidth.exact, 0.90003328)

  expect_equal(binom(3, 20, CI.width = 0.99)$CI.exact.lower, 0.01764)
  expect_equal(binom(3, 20, CI.width = 0.99)$CI.exact.upper, 0.44947)
  expect_equal(binom(3, 20, CI.width = 0.99)$actualCIwidth.exact, 0.99000265)

  expect_equal(binom(3, 20, do.asymp = TRUE)$CI.asymp.lower, -0.0064905747)
  expect_equal(binom(3, 20, do.asymp = TRUE)$CI.asymp.upper, 0.306490575)

  expect_equal(binom(2, 30, do.asymp = TRUE)$CI.exact.lower, 0.00817)
  expect_equal(binom(2, 30, do.asymp = TRUE)$CI.exact.upper, 0.22074)
  expect_equal(binom(2, 30, do.asymp = TRUE)$CI.asymp.lower, -0.02259402)
  expect_equal(binom(2, 30, do.asymp = TRUE)$CI.asymp.upper, 0.155927353)

  expect_equal(binom(13, 30, do.asymp = TRUE)$CI.exact.lower, 0.25460)
  expect_equal(binom(13, 30, do.asymp = TRUE)$CI.exact.upper, 0.62573)
  expect_equal(binom(13, 30, do.asymp = TRUE)$CI.asymp.lower, 0.256011446)
  expect_equal(binom(13, 30, do.asymp = TRUE)$CI.asymp.upper, 0.61065522)
})

test_that("Example 4.8", {
  expect_equal(pbinom(4, 20, 0.1), 0.9568255)
  expect_equal(pbinom(4, 20, 0.1, lower.tail = FALSE), 0.043174495)
  expect_equal(pbinom(4, 20, 0.2, lower.tail = FALSE), 0.37035174)
  expect_equal(pnorm(0.1118, lower.tail = FALSE), 0.455491)
})

test_that("Example 4.9", {
  expect_equal(binom(15, 100, 0.1, alternative="greater")$pval.exact, 0.072572965)
  expect_equal(binom(16, 100, 0.1, alternative="greater")$pval.exact, 0.039890527)
  expect_equal(binom(17, 100, 0.1, alternative="greater")$pval.exact, 0.02059881)
  expect_equal(pbinom(15, 100, 0.2, lower.tail = FALSE), 0.87149449)

  expect_equal(binom(9, 50, 0.1, alternative="greater")$pval.exact, 0.057867206)
  expect_equal(binom(10, 50, 0.1, alternative="greater")$pval.exact, 0.024537936)
  expect_equal(pbinom(9, 50, 0.2, lower.tail = FALSE), 0.55625959)

  expect_equal(binom(21, 150, 0.1, alternative="greater")$pval.exact, 0.072088006)
  expect_equal(binom(22, 150, 0.1, alternative="greater")$pval.exact, 0.043963596)
  expect_equal(pbinom(21, 150, 0.2, lower.tail = FALSE), 0.96278398)

  expect_equal(binom(27, 200, 0.1, alternative="greater")$pval.exact, 0.067224684)
  expect_equal(binom(28, 200, 0.1, alternative="greater")$pval.exact, 0.0434285)
  expect_equal(pbinom(27, 200, 0.2, lower.tail = FALSE), 0.98904923)

  expect_equal(binom(39, 300, 0.1, alternative="greater")$pval.exact, 0.054929804)
  expect_equal(binom(40, 300, 0.1, alternative="greater")$pval.exact, 0.037803981)
  expect_equal(pbinom(39, 300, 0.2, lower.tail = FALSE), 0.9990151)

  expect_equal(binom(16, 105, 0.1, alternative="greater")$pval.exact, 0.05813619)
  expect_equal(pbinom(15, 105, 0.2, lower.tail = FALSE), 0.9139548)

  expect_equal(binom(17, 105, 0.1, alternative="greater")$pval.exact, 0.0316356)
  expect_equal(pbinom(16, 105, 0.2, lower.tail = FALSE), 0.86529605)

  expect_equal(binom(17, 108, 0.1, alternative="greater")$pval.exact, 0.040067389)
  expect_equal(pbinom(16, 108, 0.2, lower.tail = FALSE), 0.89282262)

  expect_equal(binom(17, 109, 0.1, alternative="greater")$pval.exact, 0.043208076)
  expect_equal(pbinom(16, 109, 0.2, lower.tail = FALSE), 0.90092295)
})

test_that("Example 4.10", {
  expect_equal(binom(1, 54, 0.07, alternative="less")$pval.exact, 0.100604448)
  expect_equal(binom(1, 54)$CI.exact.lower, 0.00046)
  expect_equal(binom(1, 54)$CI.exact.upper, 0.09892)
})

test_that("Example 4.11", {
  expect_equal(binom(15, 20, 0.5)$pval.exact, 0.041389465)
  expect_equal(binom(15, 20)$CI.exact.lower, 0.50895)
  expect_equal(binom(15, 20)$CI.exact.upper, 0.91343)
  expect_equal(binom(14, 20)$CI.exact.lower, 0.45721)
  expect_equal(binom(14, 20)$CI.exact.upper, 0.88107)
})

test_that("Example 4.12", {
  expect_equal(binom(13, 32, 0.25)$pval.exact, 0.062912758)
  expect_equal(binom(13, 32)$CI.exact.lower, 0.23698)
  expect_equal(binom(13, 32)$CI.exact.upper, 0.59356)
})
