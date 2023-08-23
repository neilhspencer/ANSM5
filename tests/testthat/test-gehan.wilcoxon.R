test_that("Example 9.1", {
  S <- c(49, 58, 75, 110, 112, 132, 151, 276, 281, 362)
  S.c <- c(rep(0,9), 1)
  A <- c(50, 58, 96, 139, 152, 159, 189, 225, 239, 242, 257, 262, 292, 294, 300,
         301, 306, 329, 342, 346, 349, 354, 359, 360, 365, 378, 381, 388)
  A.c <- c(rep(0,14), 1, 0, rep(1, 6), 0, rep(1,5))
  expect_equal(gehan.wilcoxon(S, A, S.c, A.c, alternative = "less",
                              seed = 1)$pval.mc.stat, 217.5)
  expect_equal(gehan.wilcoxon(S, A, S.c, A.c, alternative = "less",
                              seed = 1)$pval.mc, 0.00385)
  expect_equal(gehan.wilcoxon(S, A, S.c, A.c, alternative = "less",
                              do.exact = FALSE,
                              do.asymp = TRUE)$pval.asymp.stat, 217.5)
  expect_equal(gehan.wilcoxon(S, A, S.c, A.c, alternative = "less",
                              do.exact = FALSE,
                              do.asymp = TRUE)$pval.asymp, 0.0050981517)
})
