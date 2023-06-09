test_that("Example 6.14", {
  GroupA <- c(8, 9, 10, 12, 13, 15, 18, 20, 21)
  GroupB <- c(1, 3, 5, 6, 11, 14, 19, 22, 23, 25)
  expect_equal(moses.extreme.reactions(GroupA, GroupB)$pval.exact.stat, 12)
  expect_equal(moses.extreme.reactions(GroupA, GroupB)$pval.exact, 0.0148844963)
  GroupA2 <- c(8, 9, 10, 12, 13, 15, 18, 20, 22)
  expect_equal(moses.extreme.reactions(GroupA2, GroupB)$pval.exact.stat, 13)
  expect_equal(moses.extreme.reactions(GroupA2, GroupB)$pval.exact, 0.03989045)

})
