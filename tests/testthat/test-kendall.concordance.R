test_that("Example 10.9", {
  ranks <- c(2, 2, 4, 4, 3, 3, 1, 1, 2, 6, 5, 5, 3, 6, 1, 5, 4, 6)
  competitors <- as.factor(c(rep("I", 3), rep("II", 3), rep("III", 3),
                             rep("IV", 3), rep("V", 3), rep("VI", 3)))
  judges <- as.factor(c(rep(c("A", "B", "C"), 6)))
  expect_equal(kendall.concordance(ranks, competitors, judges,
                                   seed = 1)$pval.mc.stat, 0.63174603)
  expect_equal(kendall.concordance(ranks, competitors, judges,
                                   seed = 1)$pval.mc, 0.0615)
})
