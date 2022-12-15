test_that("exact p-val works", {
  heartrates1 <- c(73, 82, 87, 68, 106, 60, 97)
  heartrates2 <- c(-2,4,8,25,-5,16,3,1,12,17,20,9)
  withties <- c(4, 4, 8, 8, 11, 11, 11)
  ages <- c(6, 18, 24, 26, 37, 40, 42, 47, 69, 69, 87, 130)
  expect_equal(wilcoxon.signed.rank.test(heartrates1, 70,
                                         "greater")$pval.exact.stat, 4)
  expect_equal(wilcoxon.signed.rank.test(heartrates1, 70,
                                         "greater")$pval.exact, 0.05469)
  expect_equal(wilcoxon.signed.rank.test(heartrates2, 15,
                                         "less")$pval.exact.stat, 14)
  expect_equal(wilcoxon.signed.rank.test(heartrates2, 15,
                                         "less")$pval.exact, 0.02612)
  expect_equal(wilcoxon.signed.rank.test(heartrates2, 15)$pval.exact.stat, 14)
  expect_equal(wilcoxon.signed.rank.test(heartrates2, 15)$pval.exact, 0.05225)
  expect_equal(wilcoxon.signed.rank.test(withties, 6, "greater")$pval.exact.stat, NULL)
  expect_equal(wilcoxon.signed.rank.test(withties, 6, "greater")$pval.exact, NULL)
  expect_equal(wilcoxon.signed.rank.test(ages, 30, "greater")$pval.exact.stat, NULL)
  expect_equal(wilcoxon.signed.rank.test(ages, 30, "greater")$pval.exact, NULL)
})
