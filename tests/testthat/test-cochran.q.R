test_that("Example 7.8", {
  outcome <- c(1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1)
  member <- as.factor(rep(c("A", "B", "C", "D", "E"), 3))
  climb <- as.factor(c(rep("Climb1", 5), rep("Climb2", 5), rep("Climb3", 5)))
  expect_equal(cochran.q(outcome, climb, member)$pval.exact.stat, 1.2)
  expect_equal(cochran.q(outcome, climb, member)$pval.exact, 0.85185185)
  expect_equal(cochran.q(outcome, climb, member, do.exact = FALSE,
                                  do.asymp = TRUE)$pval.asymp.stat, 1.2)
  expect_equal(cochran.q(outcome, climb, member, do.exact = FALSE,
                                  do.asymp = TRUE)$pval.asymp, 0.54881164)
})
