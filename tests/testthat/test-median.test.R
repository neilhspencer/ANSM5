test_that("Example 6.7", {
  Males <- c(15, 16, 20, 24, 27, 31, 34)
  Females <- c(8, 9, 11, 11, 14, 15, 16, 16, 16, 17, 18, 18, 19, 21, 23, 23, 25,
               25, 25, 27, 32)
  expect_equal(median.test(Males, Females)$pval.exact, 0.38454106)
})

test_that("Example 6.8", {
  Males <- c(15, 16, 20, 24, 27, 31, 34)
  Females <- c(8, 9, 11, 11, 14, 15, 16, 16, 16, 17, 18, 18, 19, 21, 23, 23, 25,
               25, 25, 27, 32)
  expect_equal(median.test(Males, Females, CI.width = 0.90)$CI.exact.lower, -15)
  expect_equal(median.test(Males, Females, CI.width = 0.90)$CI.exact.upper, 3)
  expect_equal(median.test(Males, Females, CI.width = 0.90)$actualCIwidth, 0.9231884)
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
