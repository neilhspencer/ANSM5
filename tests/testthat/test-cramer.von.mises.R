test_that("Example 6.16", {
  expect_equal(cramer.von.mises(ch6data$salivaF, ch6data$salivaM)$pval.stat,
               0.33418367)
  expect_equal(cramer.von.mises(ch6data$salivaF, ch6data$salivaM)$pval,
               "p-value >= 0.10")
  expect_equal(cramer.von.mises(ch6data$salivaF, ch6data$salivaM,
                                alternative = "greater")$pval,
               "p-value >= 0.05")
})
