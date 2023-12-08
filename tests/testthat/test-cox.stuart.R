test_that("Example 4.13", {
  expect_equal(cox.stuart(ch4data$precipitation)$pval.exact, 0.68750)
})

test_that("Exercise 4.8", {
  expect_equal(cox.stuart(ch4data$days.waiting)$pval.exact, 0.0703125)
})

test_that("Exercise 4.9", {
  expect_equal(cox.stuart(ch4data$rainfall.by.latitude)$pval.exact, 0.125)
})

test_that("Exercise 4.10", {
  expect_equal(cox.stuart(ch4data$points)$pval.exact, 0.2890625)
})

test_that("Exercise 4.11", {
  expect_equal(cox.stuart(ch4data$rainfall.DRC)$pval.exact, 0.2890625)
})

test_that("Exercise 4.12", {
  expect_equal(cox.stuart(ch4data$piped.water.DRC)$pval.exact, 1)
})
