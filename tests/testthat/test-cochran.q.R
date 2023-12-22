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

test_that("Exercise 7.14", {
  expect_equal(cochran.q(ch7data$soc.media.use, ch7data$participant,
                         ch7data$day, seed = 1)$pval.mc.stat, 1.4347826)
  expect_equal(cochran.q(ch7data$soc.media.use, ch7data$participant,
                         ch7data$day, seed = 1)$pval.mc, 0.85553)
  expect_equal(cochran.q(ch7data$soc.media.use, ch7data$participant,
                         ch7data$day, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp, 0.69740213)
})
