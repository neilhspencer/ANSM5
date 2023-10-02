test_that("Example 10.4", {
  Q1 <- c(1, 3, 4, 5, 6, 8, 10, 11, 13, 14, 16, 17)
  Q2 <- c(13, 15, 18, 16, 23, 31, 39, 56, 45, 43, 37, 0)
  expect_equal(cor(Q1, Q2, method = "kendall"), 0.42424242)
})

test_that("Example 10.5", {
  Q1 <- c(1, 3, 4, 5, 6, 8, 10, 11, 13, 14, 16, 17)
  Q2 <- c(13, 15, 18, 16, 23, 31, 39, 56, 45, 43, 37, 0)
  expect_equal(kendall.tau(Q1, Q2, alternative = "greater",
                           seed = 1)$stat, 0.42424242)
  expect_equal(kendall.tau(Q1, Q2, alternative = "greater", seed = 1)$pval.mc,
               0.03176)
})

test_that("Example 10.7", {
  ExpertA <- c(1, 3.5, 3.5, 3.5, 3.5, 8, 8, 8, 8, 8, 11.5, 11.5)
  ExpertB <- c(1, 3.5, 3.5, 3.5, 3.5, 8, 8, 8, 8, 8, 11.5, 11.5)
  expect_equal(cor(ExpertA, ExpertB, method = "kendall"), 1)
})

test_that("Example 10.8", {
  Year <- c(1827, 1884, 1895, 1908, 1914, 1918, 1924, 1928, 1936, 1941, 1964, 1965, 1977)
  Age <- c(13, 83, 34, 1, 11, 16, 68, 13, 77, 74, 87, 65, 83)
  expect_equal(cor(Year, Age, method = "kendall"), 0.38964325)
  expect_equal(kendall.tau(Year, Age, alternative = "greater",
                           seed = 1)$stat, 0.38964325)
  expect_equal(kendall.tau(Year, Age, alternative = "greater",
                           seed = 1)$pval.mc, 0.03771)
  expect_equal(kendall.tau(Year, Age, alternative = "greater", do.asymp = TRUE,
                           do.exact = FALSE)$stat, 0.38964325)
  expect_equal(kendall.tau(Year, Age, alternative = "greater", do.asymp = TRUE,
                           do.exact = FALSE)$pval.asymp, 0.03309629)
})
