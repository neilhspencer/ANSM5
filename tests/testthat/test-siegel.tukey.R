test_that("Example 6.11", {
  expect_equal(siegel.tukey(ch6data$typeA, ch6data$typeB)$pval.exact.stat,
               paste0("\n","106 (rank sum from ch6data$typeA), ",
                      "104 (rank sum from ch6data$typeB)", "\n",
                      "70 (Mann-Whitney U from ch6data$typeA), ",
                      "26 (Mann-Whitney U from ch6data$typeB)"))
  expect_equal(siegel.tukey(ch6data$typeA, ch6data$typeB)$pval.exact,
               0.097880448)
})
