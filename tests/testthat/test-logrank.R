test_that("Example 9.5", {
  times <- c(7, 9, 13, 16, 8, 10, 14, 16, 25, 27)
  censoring <- c(0, 0, 1, 0, 0, 0, 1, 0, 1, 0)
  samples <- factor(c(rep("SampleI", 4), rep("SampleII", 6)))
  expect_equal(logrank(times, censoring, samples,
                       score.censored = FALSE)$pval.exact.stat, 1.10595238)
  expect_equal(logrank(times, censoring, samples,
                       score.censored = FALSE)$pval.exact, 0.076190476)
})

test_that("Example 9.6", {
  times <- c(4, 16, 9, 19, 23, 51, 11, 49, 63)
  censoring <- c(0, 0, 0, 0, 0, 1, 0, 1, 0)
  samples <- factor(c(rep("SampleI", 2), rep("SampleII", 4),
                      rep("SampleIII", 3)))
  expect_equal(logrank(times, censoring, samples)$pval.exact.stat, 1.34325397)
  expect_equal(logrank(times, censoring, samples)$pval.exact, 0.206349206)
})
