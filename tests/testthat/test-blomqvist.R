test_that("Example 10.9", {
  Q1 <- c(1, 3, 4, 5, 6, 8, 10, 11, 13, 14, 16, 17)
  Q2 <- c(13, 15, 18, 16, 23, 31, 39, 56, 45, 43, 37, 0)
  expect_equal(blomqvist(Q1, Q2, alternative = "greater")$stat,
               0.66666667)
  expect_equal(blomqvist(Q1, Q2, alternative = "greater")$pval.exact,
               0.04004329)
})
