test_that("Example 6.12", {
  expect_equal(ansari.bradley(ch6data$typeA, ch6data$typeB)$pval.exact.stat, 30)
  expect_equal(ansari.bradley(ch6data$typeA, ch6data$typeB)$pval.exact,
               0.143732635)
  expect_equal(ansari.bradley(ch6data$typeA, ch6data$typeB,
                              do.asymp = TRUE)$pval.asymp.stat, 30)
  expect_equal(ansari.bradley(ch6data$typeA, ch6data$typeB,
                              do.asymp = TRUE)$pval.asymp, 0.121413624)
})

test_that("Exercise 6.14", {
  expect_equal(ansari.bradley(ch6data$time.withoutLD,
                              ch6data$time.withLD)$pval.exact, 1)
})

test_that("Exercise 6.16", {
  expect_equal(ansari.bradley(ch6data$travel, ch6data$politics)$pval.exact,
               0.032810842)
})
