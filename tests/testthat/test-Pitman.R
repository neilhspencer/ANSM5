test_that("calculations work", {
  heartrates1 <- c(73, 82, 87, 68, 106, 60, 97)
  expect_equal(pitman(heartrates1, 70, "greater")$pval.exact.stat, 95)
  expect_equal(pitman(heartrates1, 70, "greater")$pval.exact, 0.0546875)
  expect_equal(pitman(heartrates1, 70, "greater")$pval.asymp.stat, 1.63691848888252 )
  expect_equal(pitman(heartrates1, 70, "greater")$pval.asymp, 0.050823751)
})

test_that("Example 3.12", {
  sampleI <- c(5.5, 6.0, 6.5, 7.6, 7.6, 7.7, 8.0, 8.2, 9.1, 15.1)
  sampleII <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5,
                8.7, 9.4, 14.3, 26.0)
  expect_equal(pitman(sampleI, 9)$pval.exact, 0.3515625)
  expect_equal(pitman(sampleI, 9)$pval.asymp, 0.3051186)
  expect_equal(pitman(sampleII, 9)$pval.exact, 0.9159565)
  expect_equal(pitman(sampleII, 9)$pval.asymp, 0.81653892)
  sampleIa <- c(5.5, 6.0, 6.5, 7.6, 7.6, 7.7, 8.0, 8.2, 9.1, 12.7)
  sampleIIa <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5,
                 8.7, 9.4, 12.6, 12.8)
  expect_equal(pitman(sampleIa, 9)$pval.exact, 0.123046875)
  expect_equal(pitman(sampleIIa, 9)$pval.exact, 0.035497665)
})
