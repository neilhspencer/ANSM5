test_that("Example 13.2", {
  expect_equal(odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                                  ch13$gender, do.exact = FALSE,
                                  do.asymp = TRUE)$pval.asymp.stat, 0.7918946)
  expect_equal(odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                                  ch13$gender, do.exact = FALSE,
                                  do.asymp = TRUE)$pval.asymp, 0.42842214)
  expect_equal(odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                                  ch13$gender, do.exact = FALSE,
                                  do.asymp = TRUE)$CI.asymp.lower, -0.201675165)
  expect_equal(odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                                  ch13$gender, do.exact = FALSE,
                                  do.asymp = TRUE)$CI.asymp.upper, 0.475127192)
  tmp <- odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                            ch13$gender)
  expect_equal(tmp$pval.exact.stat, 0.136726013)
  expect_equal(tmp$pval.exact, 0.44031545)
  expect_equal(tmp$CI.exact.lower, -0.34815705)
  expect_equal(tmp$CI.exact.upper, 0.34589494)
  expect_equal(odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                                  ch13$gender, do.exact = FALSE, do.mc = TRUE,
                                  seed = 1)$pval.mc.stat, -0.136726013)
  expect_equal(odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                                  ch13$gender, do.exact = FALSE, do.mc = TRUE,
                                  seed = 1)$pval.mc, 0.44313)
  expect_equal(odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                                  ch13$gender, do.exact = FALSE, do.mc = TRUE,
                                  seed = 1)$CI.mc.lower, -0.34449187)
  expect_equal(odds.ratio.2x2diff(ch13$physical.activity, ch13$tv.viewing,
                                  ch13$gender, do.exact = FALSE, do.mc = TRUE,
                                  seed = 1)$CI.mc.upper, 0.34938077)
})
