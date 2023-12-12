test_that("Example 6.12", {
  TypeA <- c(177, 200, 227, 230, 232, 268, 272, 297)
  TypeB <- c(47, 105, 126, 142, 158, 172, 197, 220, 225, 230, 262, 270)
  expect_equal(ansari.bradley(ch6data$typeA, ch6data$typeB)$pval.exact.stat, 30)
  expect_equal(ansari.bradley(ch6data$typeA, ch6data$typeB)$pval.exact,
               0.143732635)
  expect_equal(ansari.bradley(ch6data$typeA, ch6data$typeB,
                              do.asymp = TRUE)$pval.asymp.stat, 30)
  expect_equal(ansari.bradley(ch6data$typeA, ch6data$typeB,
                              do.asymp = TRUE)$pval.asymp, 0.121413624)
})
