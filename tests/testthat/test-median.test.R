test_that("Example 6.7", {
  expect_equal(median.test(ch6$males, ch6$females)$pval.exact,
               0.38454106)
})

test_that("Example 6.8", {
  expect_equal(median.test(ch6$males, ch6$females,
                           CI.width = 0.90)$CI.exact.lower, -14.4)
  expect_equal(median.test(ch6$males, ch6$females,
                           CI.width = 0.90)$CI.exact.upper, 4)
  expect_equal(median.test(ch6$males, ch6$females,
                           CI.width = 0.90)$actualCIwidth, 0.9231884)
})

test_that("Example 7.4", {
  expect_equal(median.test(ch7$time, ch7$surgeon)$pval.exact, 0.046045976)
})

test_that("Example 7.5", {
  expect_equal(median.test(ch7$time, ch7$surgeon, do.exact = FALSE,
                           do.asymp = TRUE)$pval.asymp.stat, 11.152381)
  expect_equal(median.test(ch7$time, ch7$surgeon, do.exact = FALSE,
                           do.asymp = TRUE)$pval.asymp, 0.048441037)
})
