test_that("Example 6.10", {
  GroupA <- c(23, 18, 17, 25, 22, 19,  31, 26, 29, 33)
  GroupB <- c(21, 28, 32, 30, 41, 24,  35, 34, 27, 39, 36)
  expect_equal(normal.scores.test(GroupA, GroupB)$pval.exact.stat, -4.9861603)
  expect_equal(normal.scores.test(GroupA, GroupB)$pval.exact, 0.011686456)
  expect_equal(normal.scores.test(GroupA, GroupB,
                                  do.asymp = TRUE)$pval.asymp.stat, -4.9861603)
  expect_equal(normal.scores.test(GroupA, GroupB,
                                  do.asymp = TRUE)$pval.asymp, 0.010654119)
})

test_that("Example 8.9", {
  Sequence <- c("AB", "AB", "AB", "AB", "AB", "BA", "BA", "BA", "BA", "BA")
  PeriodI <- c(1.75, 0.3, 0.35, 0.2, 0.3, 7.2, 7.1, 0.75, 2.15, 3.35)
  PeriodII <- c(0.55, 1.05, 0.63, 1.55, 8.2, 0.35, 1.55, 0.25, 0.35, 1.5)
  GroupAB.sum <- (PeriodI + PeriodII)[Sequence == "AB"]
  GroupBA.sum <- (PeriodI + PeriodII)[Sequence == "BA"]
  expect_equal(normal.scores.test(GroupAB.sum, GroupBA.sum)$pval.exact.stat,
               -1.4942462)
  expect_equal(normal.scores.test(GroupAB.sum, GroupBA.sum)$pval.exact,
               0.27777778)
})
