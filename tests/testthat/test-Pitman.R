test_that("calculations work", {
  heartrates1 <- c(73, 82, 87, 68, 106, 60, 97)
  expect_equal(Pitman.test(heartrates1, 70, "greater")$pval.stat, 95)
  expect_equal(wilcoxon.signed.rank.test(heartrates1, 70, "greater")$pval,
               0.05469)
})
