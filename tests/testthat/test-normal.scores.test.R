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
