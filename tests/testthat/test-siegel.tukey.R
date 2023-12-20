test_that("Example 6.11", {
  expect_equal(siegel.tukey(ch6data$typeA, ch6data$typeB)$pval.exact.stat,
               paste0("\n","106 (rank sum from ch6data$typeA), ",
                      "104 (rank sum from ch6data$typeB)", "\n",
                      "70 (Mann-Whitney U from ch6data$typeA), ",
                      "26 (Mann-Whitney U from ch6data$typeB)"))
  expect_equal(siegel.tukey(ch6data$typeA, ch6data$typeB)$pval.exact,
               0.097880448)
})

test_that("Exercise 6.11", {
  expect_equal(siegel.tukey(ch6data$typeA, ch6data$typeB,
                            mean.shift = TRUE)$pval.exact, 0.20828769)
})

test_that("Exercise 6.14", {
  expect_equal(siegel.tukey(ch6data$time.withoutLD,
                            ch6data$time.withLD)$pval.exact, 0.86651127)
})

test_that("Exercise 6.16", {
  expect_equal(siegel.tukey(ch6data$travel, ch6data$politics)$pval.exact,
               0.040821408)
})
