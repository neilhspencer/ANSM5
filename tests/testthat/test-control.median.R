test_that("Example 6.9", {
  expect_equal(control.median(ch6data$sampleI, ch6data$sampleII,
                              alternative = "greater")$pval.exact, 0.0077527593)
})
