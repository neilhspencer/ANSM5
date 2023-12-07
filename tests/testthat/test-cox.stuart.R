test_that("Example 4.13", {
  expect_equal(cox.stuart(ch4data$precipitation)$pval.exact, 0.68750)
})
