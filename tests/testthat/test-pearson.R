test_that("Section 10.1.2", {
  Q1 <- c(1, 3, 4, 5, 6, 8, 10, 11, 13, 14, 16, 17)
  Q2 <- c(13, 15, 18, 16, 23, 31, 39, 56, 45, 43, 37, 0)
  expect_equal(pearson(Q1, Q2, alternative = "greater", do.asymp = TRUE,
                       do.exact = FALSE)$pval.asymp.stat, 0.37293673)
  expect_equal(pearson(Q1, Q2, alternative = "greater", do.asymp = TRUE,
                       do.exact = FALSE)$pval.asymp, 0.116244597)
  expect_equal(pearson(Q1, Q2, alternative = "greater",
                       seed = 1)$pval.mc.stat, 0.37293673)
  expect_equal(pearson(Q1, Q2, alternative = "greater", seed = 1)$pval.mc,
               0.11676)
})
