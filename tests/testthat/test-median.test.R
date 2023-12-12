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
  time <- c(23, 25, 34, 45, 7, 11, 14, 15, 17, 24, 40, 5, 8, 16, 20, 26, 12, 21,
            31, 38, 30, 43, 4, 9, 10, 18, 19, 35)
  surgeon <- as.factor(c(rep("I", 4), rep("II", 7), rep("III", 5), rep("IV", 4),
               rep("V", 2), rep("VI", 6)))
  expect_equal(median.test(time, surgeon)$pval.exact, 0.046045976)
})

test_that("Example 7.5", {
  time <- c(23, 25, 34, 45, 7, 11, 14, 15, 17, 24, 40, 5, 8, 16, 20, 26, 12, 21,
            31, 38, 30, 43, 4, 9, 10, 18, 19, 35)
  surgeon <- as.factor(c(rep("I", 4), rep("II", 7), rep("III", 5), rep("IV", 4),
                         rep("V", 2), rep("VI", 6)))
  expect_equal(median.test(time, surgeon, do.exact = FALSE,
                           do.asymp = TRUE)$pval.asymp.stat, 11.152381)
  expect_equal(median.test(time, surgeon, do.exact = FALSE,
                           do.asymp = TRUE)$pval.asymp, 0.048441037)
})
