test_that("Example 4.13", {
  precipitation <- c(0, 1.841, 0.202, 0.891, 1.423, 0, 1.345, 49.8, 0, 0, 1.561,
                     4.376)
  expect_equal(cox.stuart(precipitation)$pval.exact, 0.68750)
})
