test_that("Section 10.1.2", {
  Q1 <- c(1, 3, 4, 5, 6, 8, 10, 11, 13, 14, 16, 17)
  Q2 <- c(13, 15, 18, 16, 23, 31, 39, 56, 45, 43, 37, 0)
  expect_equal(pearson(Q1, Q2, alternative = "greater", do.asymp = TRUE,
                       do.exact = FALSE)$stat, 0.37293673)
  expect_equal(pearson(Q1, Q2, alternative = "greater", do.asymp = TRUE,
                       do.exact = FALSE)$pval.asymp, 0.116244597)
  expect_equal(pearson(Q1, Q2, alternative = "greater",
                       seed = 1)$stat, 0.37293673)
  expect_equal(pearson(Q1, Q2, alternative = "greater", seed = 1)$pval.mc,
               0.11676)
})

test_that("Example 11.2", {
  parentlimit <- c(0, 1, 2, 3, 4, 5, 6)
  reportedtime <- c(2.5, 3.1, 3.4, 4, 4.6, 5.1, 11.1)
  H0 <- 1
  expect_equal(pearson(parentlimit, reportedtime - H0 * parentlimit,
                       alternative = "two.sided")$stat, 0.139175554)
  expect_equal(pearson(parentlimit, reportedtime - H0 * parentlimit,
                       alternative = "two.sided")$pval.exact, 0.79246032)
})


