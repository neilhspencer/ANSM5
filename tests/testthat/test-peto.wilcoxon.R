test_that("Example 9.1", {
  S <- c(49, 58, 75, 110, 112, 132, 151, 276, 281, 362)
  S.c <- c(rep(0,9), 1)
  A <- c(50, 58, 96, 139, 152, 159, 189, 225, 239, 242, 257, 262, 292, 294, 300,
         301, 306, 329, 342, 346, 349, 354, 359, 360, 365, 378, 381, 388)
  A.c <- c(rep(0,14), 1, 0, rep(1, 6), 0, rep(1,5))
  expect_equal(peto.wilcoxon(S, A, S.c, A.c, alternative = "less",
                             seed = 1)$pval.mc.stat, 622.96939)
  expect_equal(peto.wilcoxon(S, A, S.c, A.c, alternative = "less",
                             seed = 1)$pval.mc, 0.0034)
})

test_that("Following Example 9.3", {
  SampleI <-c(9, 25, 35, 38, 41)
  SampleI.c <- rep(0, 5)
  SampleII <-c(10, 12, 15, 22, 37, 48, 57, 58)
  SampleIIa <-c(10, 26, 29, 36, 37, 48, 57, 58)
  SampleII.c <- c(0, 1, 1, 1, 0, 0, 0, 0)
  expect_equal(peto.wilcoxon(SampleI, SampleII, SampleI.c, SampleII.c,
                             alternative = "less")$pval.exact.stat, 66.25)
  expect_equal(peto.wilcoxon(SampleI, SampleII, SampleI.c, SampleII.c,
                             alternative = "less")$pval.exact, 0.048174048)
})

