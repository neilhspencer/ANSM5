test_that("Example 6.12", {
  expect_equal(mood(ch6$typeA, ch6$typeB)$pval.exact.stat, 164)
  expect_equal(mood(ch6$typeA, ch6$typeB)$pval.exact, 0.12933238)
  expect_equal(mood(ch6$typeA, ch6$typeB,
                    do.asymp = TRUE)$pval.asymp.stat, 164)
  expect_equal(mood(ch6$typeA, ch6$typeB, do.asymp = TRUE)$pval.asymp,
               0.12562273)
})
