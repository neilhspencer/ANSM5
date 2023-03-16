test_that("Example 4.16", {
  times_as_degrees <- c(15, 45, 65, 75, 85, 95, 100, 105, 145, 165, 180, 260)
  expect_equal(hodges.ajne(times_as_degrees)$pval.stat, 1)
  expect_equal(hodges.ajne(times_as_degrees)$pval, 0.05859375)
})

test_that("Test overly-uniform data", {
  uniform_data <- c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)
  expect_equal(hodges.ajne(uniform_data)$pval.stat, 6)
  expect_equal(hodges.ajne(uniform_data)$pval, NA)
})
