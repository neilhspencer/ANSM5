test_that("Example 13.2", {
  physical.activity <- factor(c(rep("Daily", 167 + 320 + 123 + 157),
                                rep("Less than daily", 388 + 937 + 604 + 1114)),
                              levels = c("Daily", "Less than daily"))
  tv.viewing <- factor(c(rep("2 hours or less", 167), rep("More than 2 hours", 320),
                         rep("2 hours or less", 123), rep("More than 2 hours", 157),
                         rep("2 hours or less", 388), rep("More than 2 hours", 937),
                         rep("2 hours or less", 604), rep("More than 2 hours", 1114)),
                       levels = c("2 hours or less", "More than 2 hours"))
  gender <- factor(c(rep("Boy", 167 + 320), rep("Girl", 123 + 157),
                     rep("Boy", 388 + 937), rep("Girl", 604 + 1114)),
                   levels = c("Boy", "Girl"))
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing, gender,
                                  do.exact = FALSE,
                                  do.asymp = TRUE)$pval.asymp.stat, -0.7918946)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing, gender,
                                  do.exact = FALSE,
                                  do.asymp = TRUE)$pval.asymp, 0.42842214)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing, gender,
                                  do.exact = FALSE,
                                  do.asymp = TRUE)$CI.asymp.lower, -0.475127192)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing, gender,
                                  do.exact = FALSE,
                                  do.asymp = TRUE)$CI.asymp.upper, 0.201675165)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing,
                                  gender)$pval.exact.stat, -0.136726013)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing,
                                  gender)$pval.exact, 0.44031545)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing,
                                  gender)$CI.exact.lower, -0.34589494)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing,
                                  gender)$CI.exact.upper, 0.34815705)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing, gender,
                                  do.exact = FALSE, do.mc = TRUE,
                                  seed = 1)$pval.mc.stat, -0.136726013)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing, gender,
                                  do.exact = FALSE, do.mc = TRUE,
                                  seed = 1)$pval.mc, 0.44313)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing, gender,
                                  do.exact = FALSE, do.mc = TRUE,
                                  seed = 1)$CI.mc.lower, -0.34449187)
  expect_equal(odds.ratio.2x2diff(physical.activity, tv.viewing, gender,
                                  do.exact = FALSE, do.mc = TRUE,
                                  seed = 1)$CI.mc.upper, 0.34938077)
})
