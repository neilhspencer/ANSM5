test_that("Example 10.10", {
  expect_equal(kendall.concordance(ch10$diving.rank, ch10$competitors,
                                   ch10$judges, seed = 1)$stat, 0.63174603)
  expect_equal(kendall.concordance(ch10$diving.rank, ch10$competitors,
                                   ch10$judges, seed = 1)$pval.mc, 0.0615)
})

test_that("Exercise 10.10", {
  expect_equal(kendall.concordance(
    c(ch10$British, ch10$American, ch10$Canadian, ch10$Australian),
    ch10$country, ch10$design)$stat, 0.0163934426)
  expect_equal(kendall.concordance(
    c(ch10$British, ch10$American, ch10$Canadian, ch10$Australian),
    ch10$country, ch10$design)$pval.mc, 0.96711)
})

test_that("Exercise 10.11", {
  expect_equal(kendall.concordance(ch10$marks, ch10$script, ch10$examiner)$stat, XXX)
  expect_equal(kendall.concordance(ch10$marks, ch10$script, ch10$examiner)$pval.mc, XXX)

  expect_equal(kendall.concordance(ch10$marks, ch10$examiner, ch10$script)$stat, 0.36812865)
  expect_equal(kendall.concordance(ch10$marks, ch10$examiner, ch10$script)$pval.mc, 0.00103)
  expect_equal(kendall.concordance(ch10$marks, ch10$examiner, ch10$script, do.exact = FALSE, do.asymp = TRUE)$pval.asymp, 0.00247788356)


  kendall.concordance(ch10$marks, ch10$examiner, ch10$script, do.exact = FALSE, do.asymp = TRUE)

})
