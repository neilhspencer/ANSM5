test_that("Example 6.9", {
  expect_equal(control.median(ch6$sampleI, ch6$sampleII,
                              alternative = "greater")$pval.exact, 0.0077527593)
})
