test_that("Example 4.17", {
  dates_as_degrees <- c(151, 152, 157, 167, 182, 154, 155, 209)
  expect_equal(range.test(dates_as_degrees)$pval.stat, 1.01229097)
  expect_equal(range.test(dates_as_degrees)$pval, 0.000022540754)
})
