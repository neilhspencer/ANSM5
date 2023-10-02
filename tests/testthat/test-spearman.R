test_that("Example 10.2", {
  Q1 <- c(1, 3, 4, 5, 6, 8, 10, 11, 13, 14, 16, 17)
  Q2 <- c(13, 15, 18, 16, 23, 31, 39, 56, 45, 43, 37, 0)
  expect_equal(spearman(Q1, Q2, alternative = "greater", do.asymp = TRUE,
                       do.exact = FALSE)$stat, 0.43356643)
  expect_equal(spearman(Q1, Q2, alternative = "greater", do.asymp = TRUE,
                       do.exact = FALSE)$pval.asymp, 0.079552885)
  expect_equal(spearman(Q1, Q2, alternative = "greater",
                       seed = 1)$stat, 0.43356643)
  expect_equal(spearman(Q1, Q2, alternative = "greater", seed = 1)$pval.mc,
               0.08155)
})
