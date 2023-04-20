test_that("Example 3.11", {
  heartrates1 <- c(73, 82, 87, 68, 106, 60, 97)
  expect_equal(mc.ci(heartrates1), c(68, 97))
})
