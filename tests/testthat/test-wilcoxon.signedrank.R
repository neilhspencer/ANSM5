test_that("exact p-val works", {
  heartrates1 <- c(73, 82, 87, 68, 106, 60, 97)
  heartrates2 <- c(-2,4,8,25,-5,16,3,1,12,17,20,9)
  withties <- c(4, 4, 8, 8, 11, 11, 11)
  ages <- c(6, 18, 24, 26, 37, 40, 42, 47, 69, 69, 87, 130)
  expect_equal(wilcoxon.signedrank(heartrates1, 70,
                                         "greater")$pval.exact.stat, 4)
  expect_equal(wilcoxon.signedrank(heartrates1, 70,
                                         "greater")$pval.exact, 0.0546875)
  expect_equal(wilcoxon.signedrank(heartrates2, 15,
                                         "less")$pval.exact.stat, 14)
  expect_equal(wilcoxon.signedrank(heartrates2, 15,
                                         "less")$pval.exact, 0.026123047)
  expect_equal(wilcoxon.signedrank(heartrates2, 15)$pval.exact.stat, 14)
  expect_equal(wilcoxon.signedrank(heartrates2, 15)$pval.exact, 0.052246094)
  expect_equal(wilcoxon.signedrank(withties, 6, "greater")$pval.exact.stat, 10)
  expect_equal(wilcoxon.signedrank(withties, 6, "greater")$pval.exact, 0.0859375)
  expect_equal(wilcoxon.signedrank(ages, 30, "greater")$pval.exact.stat, 33)
  expect_equal(wilcoxon.signedrank(ages, 30, "greater")$pval.exact, 0.040527344)
})

test_that("Example 3.12", {
  sampleI <- c(5.5, 6.0, 6.5, 7.6, 7.6, 7.7, 8.0, 8.2, 9.1, 15.1)
  sampleII <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5,
                8.7, 9.4, 14.3, 26.0)
  expect_equal(wilcoxon.signedrank(sampleI, 9)$pval.exact, 0.1015625)
  expect_equal(wilcoxon.signedrank(sampleI, 9)$pval.asymp, 0.092388857)
  expect_equal(wilcoxon.signedrank(sampleII, 9)$pval.exact, 0.0148468018)
  expect_equal(wilcoxon.signedrank(sampleII, 9)$pval.asymp, 0.0168522505)
})
