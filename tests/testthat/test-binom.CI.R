test_that("CI calculations work", {
  expect_equal(binom.CI(3, 20)$CI.exact.lower, 0.03207)
  expect_equal(binom.CI(3, 20)$CI.exact.upper, 0.37893)
  expect_equal(binom.CI(3, 20)$actualCIwidth.exact, 0.95000353)

  expect_equal(binom.CI(3, 20, CI.width = 0.90)$CI.exact.lower, 0.04216)
  expect_equal(binom.CI(3, 20, CI.width = 0.90)$CI.exact.upper, 0.34367)
  expect_equal(binom.CI(3, 20, CI.width = 0.90)$actualCIwidth.exact, 0.90003328)

  expect_equal(binom.CI(3, 20, CI.width = 0.99)$CI.exact.lower, 0.01764)
  expect_equal(binom.CI(3, 20, CI.width = 0.99)$CI.exact.upper, 0.44947)
  expect_equal(binom.CI(3, 20, CI.width = 0.99)$actualCIwidth.exact, 0.99000265)

  expect_equal(binom.CI(3, 20, do.asymp = TRUE)$CI.asymp.lower, -0.0064905747)
  expect_equal(binom.CI(3, 20, do.asymp = TRUE)$CI.asymp.upper, 0.306490575)

  expect_equal(binom.CI(3, 20, do.asymp = TRUE, CI.width = 0.90)$CI.asymp.lower,
               0.0186689697)
  expect_equal(binom.CI(3, 20, do.asymp = TRUE, CI.width = 0.90)$CI.asymp.upper,
               0.28133103)

  expect_equal(binom.CI(3, 20, do.asymp = TRUE, CI.width = 0.99)$CI.asymp.lower,
               -0.055663477)
  expect_equal(binom.CI(3, 20, do.asymp = TRUE, CI.width = 0.99)$CI.asymp.upper,
               0.35566348)
})
