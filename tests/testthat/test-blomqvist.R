test_that("Example 10.9", {
  expect_equal(blomqvist(ch10$q1, ch10$q2, alternative = "greater")$stat,
               0.66666667)
  expect_equal(blomqvist(ch10$q1, ch10$q2, alternative = "greater")$pval.exact,
               0.04004329)
})

test_that("Exercise 10.6", {
  expect_equal(blomqvist(ch10$ERA, ch10$ESMS)$pval.exact, 0.007936508)
})

test_that("Exercise 10.7", {
  expect_equal(blomqvist(ch10$ERA, ch10$SSS)$pval.exact, 1)
})
