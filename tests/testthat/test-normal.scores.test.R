test_that("Example 6.10", {
  expect_equal(
    normal.scores.test(ch6data$groupA, ch6data$groupB)$pval.exact.stat,
    -4.9861603)
  expect_equal
  (normal.scores.test(ch6data$groupA, ch6data$groupB)$pval.exact, 0.011686456)
  expect_equal(normal.scores.test(ch6data$groupA, ch6data$groupB,
                                  do.asymp = TRUE)$pval.asymp.stat, -4.9861603)
  expect_equal(normal.scores.test(ch6data$groupA, ch6data$groupB,
                                  do.asymp = TRUE)$pval.asymp, 0.010654119)
})

test_that("Exercise 6.15", {
  expect_equal(normal.scores.test(ch6data$doseI, ch6data$doseII)$pval.exact,
               0.0080213904)
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
