test_that("Example 7.8", {
  expect_equal(cochran.q(ch7data$outcome, ch7data$climb,
                         ch7data$member)$pval.exact.stat, 1.2)
  expect_equal(cochran.q(ch7data$outcome, ch7data$climb,
                         ch7data$member)$pval.exact, 0.85185185)
  expect_equal(cochran.q(ch7data$outcome, ch7data$climb, ch7data$member,
                         do.exact = FALSE, do.asymp = TRUE)$pval.asymp.stat,
               1.2)
  expect_equal(cochran.q(ch7data$outcome, ch7data$climb, ch7data$member,
                         do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.54881164)
})
