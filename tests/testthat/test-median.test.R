test_that("Example 6.7", {
  Males <- c(15, 16, 20, 24, 27, 31, 34)
  Females <- c(8, 9, 11, 11, 14, 15, 16, 16, 16, 17, 18, 18, 19, 21, 23, 23, 25,
               25, 25, 27, 32)
  expect_equal(median.test(Males, Females)$pval.exact, 0.38454106)
})
