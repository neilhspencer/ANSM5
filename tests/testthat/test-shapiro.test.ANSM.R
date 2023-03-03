test_that("p-val works", {
  ages <- c(11, 13, 14, 22, 29, 30, 41, 41, 52, 55, 56, 59, 65, 65, 66, 74, 74,
            75, 77, 81, 82, 82, 82, 82, 83, 85, 85, 87, 87, 88)
  expect_equal(shapiro.test.ANSM(ages)$pval, 0.1361)
})
