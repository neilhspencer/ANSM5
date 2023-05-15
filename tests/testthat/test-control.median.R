test_that("Example 6.9", {
  SampleI <- c(49, 58, 75, 110, 112, 132, 151, 276, 281, 362)
  SampleII <- c(50, 58, 96, 139, 152, 159, 189, 225, 239, 242, 257, 262, 292,
                294, 300, 301, 306, 329, 342, 346, 349, 354, 359, 360, 365, 378,
                381, 388)
  expect_equal(control.median(SampleI, SampleII,
                              alternative = "greater")$pval.exact, 0.0077527593)
})

