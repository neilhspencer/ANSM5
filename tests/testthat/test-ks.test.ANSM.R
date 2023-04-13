test_that("exact p-val works", {
  breaks <- c(0.6, 0.8, 1.1, 1.2, 1.4, 1.7, 1.8, 1.9, 2.2, 2.4, 2.5, 2.9, 3.1,
              3.4, 3.4, 3.9, 4.4, 4.9, 5.2, 5.9)
  expect_equal(suppressWarnings(ks.test.ANSM(breaks, "punif", min = 0, max = 6,
                           alternative = "two.sided")$pval.exact.stat),
               0.183333333)
  expect_equal(suppressWarnings(ks.test.ANSM(breaks, "punif", min = 0, max = 6,
                           alternative = "two.sided")$pval.exact), 0.45829257)
  expect_equal(suppressWarnings(ks.test.ANSM(breaks, "punif", min = 0, max = 6,
                           alternative = "two.sided",
                           do.asymp = TRUE)$pval.asymp.stat), 0.183333333)
  expect_equal(suppressWarnings(ks.test.ANSM(breaks, "punif", min = 0, max = 6,
                           alternative = "two.sided",
                           do.asymp = TRUE)$pval.asymp), 0.51214416)
})
