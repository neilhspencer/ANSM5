test_that("Example 6.7", {
  expect_equal(median.test(ch6data$males, ch6data$females)$pval.exact,
               0.38454106)
})

test_that("Example 6.8", {
  expect_equal(median.test(ch6data$males, ch6data$females,
                           CI.width = 0.90)$CI.exact.lower, -15)
  expect_equal(median.test(ch6data$males, ch6data$females,
                           CI.width = 0.90)$CI.exact.upper, 3)
  expect_equal(median.test(ch6data$males, ch6data$females,
                           CI.width = 0.90)$actualCIwidth, 0.9231884)
})

test_that("Example 7.4", {
  expect_equal(median.test(ch7data$time, ch7data$surgeon)$pval.exact,
               0.046045976)
})

test_that("Example 7.5", {
  expect_equal(median.test(ch7data$time, ch7data$surgeon, do.exact = FALSE,
                           do.asymp = TRUE)$pval.asymp.stat, 11.152381)
  expect_equal(median.test(ch7data$time, ch7data$surgeon, do.exact = FALSE,
                           do.asymp = TRUE)$pval.asymp, 0.048441037)
})
