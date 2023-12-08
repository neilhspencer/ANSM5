test_that("Example 4.17", {
  expect_equal(range.test(ch4data$dates_as_degrees)$pval.stat, 1.01229097)
  expect_equal(range.test(ch4data$dates_as_degrees)$pval, 0.000022540754)
})

test_that("Exercise 4.13", {
  expect_equal(range.test(ch4data$accident.bearings - 90)$pval, 0.0074382381)
})
