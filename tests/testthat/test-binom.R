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

test_that("Exercise 4.5", {
  expect_equal(binom(3, 100, 0.01, alternative="greater")$pval.exact, 0.079373202)
  expect_equal(binom(5, 200, 0.01, alternative="greater")$pval.exact, 0.051746264)
  expect_equal(binom(6, 300, 0.01, alternative="greater")$pval.exact, 0.082903563)
  expect_equal(binom(8, 400, 0.01, alternative="greater")$pval.exact, 0.050237416)
  expect_equal(binom(9, 500, 0.01, alternative="greater")$pval.exact, 0.06711016)
  expect_equal(binom(3, 100, 0.03, alternative="greater")$pval.exact, 0.58022492)
  expect_equal(binom(5, 200, 0.03, alternative="greater")$pval.exact, 0.71902044)
  expect_equal(binom(6, 300, 0.03, alternative="greater")$pval.exact, 0.88797997)
  expect_equal(binom(8, 400, 0.03, alternative="greater")$pval.exact, 0.91378872)
  expect_equal(binom(9, 500, 0.03, alternative="greater")$pval.exact, 0.9645875)
  expect_equal(binom(8, 450, 0.01, alternative="greater")$pval.exact, 0.08555122)
  expect_equal(binom(8, 450, 0.03, alternative="greater")$pval.exact, 0.96067824)
  expect_equal(binom(8, 436, 0.01, alternative="greater")$pval.exact, 0.07449773)
  expect_equal(binom(8, 436, 0.03, alternative="greater")$pval.exact, 0.9506539)
})

test_that("Exercise 4.6", {
  expect_equal(binom(26, 72)$CI.exact.lower, 0.25116)
  expect_equal(binom(26, 72)$CI.exact.upper, 0.48288)
})

test_that("Exercise 4.7", {
  expect_equal(binom(6, 18, 0.5)$pval.exact, 0.23788452)
  expect_equal(binom(75, 225, 0.5)$pval.exact, 6.433102e-07)
  expect_equal(binom(6, 18)$CI.exact.lower, 0.13342)
  expect_equal(binom(6, 18)$CI.exact.upper, 0.59008)
  expect_equal(binom(75, 225)$CI.exact.lower, 0.27208)
  expect_equal(binom(75, 225)$CI.exact.upper, 0.39905)
})

test_that("Example 5.4", {
  expect_equal(binom(14, 23, 0.5)$pval.exact, 0.4048729)
})

test_that("Example 5.5", {
  expect_equal(pnorm(-0.5, lower.tail = FALSE), 0.69146246)
  expect_equal(binom(7, 10, 0.5, alternative = "greater")$pval.exact, 0.171875)
  expect_equal(binom(8, 10, 0.5, alternative = "greater")$pval.exact, 0.0546875)
  expect_equal(binom(9, 10, 0.5, alternative = "greater")$pval.exact, 0.0107421875)
  expect_equal(binom(7, 10, 0.6915, alternative = "greater")$pval.exact, 0.62680453)
  expect_equal(binom(8, 10, 0.6915, alternative = "greater")$pval.exact, 0.3604304)
  expect_equal(binom(9, 10, 0.6915, alternative = "greater")$pval.exact, 0.13652719)
  #
  expect_equal(binom(9, 10, 0.6915, alternative = "greater")$pval.exact, 0.13652719)
  expect_equal(binom(15, 20, 0.6915, alternative = "greater")$pval.exact, 0.38428146)
  expect_equal(binom(20, 30, 0.6915, alternative = "greater")$pval.exact, 0.69513442)
  expect_equal(binom(32, 50, 0.6915, alternative = "greater")$pval.exact, 0.82743112)
  expect_equal(binom(59, 100, 0.6915, alternative = "greater")$pval.exact, 0.9880261)
  #
  expect_equal(binom(36, 58, 0.5, alternative = "greater")$pval.exact, 0.04347445)
  expect_equal(binom(36, 58, 0.6915, alternative = "greater")$pval.exact, 0.90306589)
})

test_that("Example 5.6", {
#  expect_equal(VGAM::plaplace(0, 1, sqrt(2), lower.tail = FALSE), 0.75346565)
  expect_equal(binom(7, 10, 0.5, alternative = "greater")$pval.exact, 0.171875)
  expect_equal(binom(8, 10, 0.5, alternative = "greater")$pval.exact, 0.0546875)
  expect_equal(binom(9, 10, 0.5, alternative = "greater")$pval.exact, 0.0107421875)
  expect_equal(binom(7, 10, 0.7534, alternative = "greater")$pval.exact, 0.78376273)
  expect_equal(binom(8, 10, 0.7534, alternative = "greater")$pval.exact, 0.53582662)
  expect_equal(binom(9, 10, 0.7534, alternative = "greater")$pval.exact, 0.25177087)
  #
  expect_equal(binom(20, 30, 0.5, alternative = "greater")$pval.exact, 0.049368573)
  expect_equal(binom(20, 30, 0.7534, alternative = "greater")$pval.exact, 0.9023053)
})

test_that("Example 5.7", {
  expect_equal(pexp(0.3862, rate = 1/2, lower.tail = FALSE), 0.82439953)
  expect_equal(binom(7, 10, 0.5, alternative = "greater")$pval.exact, 0.171875)
  expect_equal(binom(8, 10, 0.5, alternative = "greater")$pval.exact, 0.0546875)
  expect_equal(binom(9, 10, 0.5, alternative = "greater")$pval.exact, 0.0107421875)
  expect_equal(binom(8, 10, 0.8244, alternative = "greater")$pval.exact, 0.74991752)
  expect_equal(binom(9, 10, 0.8244, alternative = "greater")$pval.exact, 0.45386729)
})

test_that("Section 13.4", {
  expect_equal(binom.test(32, 100, 0.4)$p.value, 0.125293086)
  expect_equal(binom.test(48, 100, 0.4)$p.value, 0.10363706)
})
