test_that("Example 4.17", {
  expect_equal(range.test(ch4data$dates_as_degrees)$pval.stat, 1.01229097)
  expect_equal(range.test(ch4data$dates_as_degrees)$pval, 0.000022540754)
})
