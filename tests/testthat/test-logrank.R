test_that("Example 9.5", {
  expect_equal(logrank(ch9$samplesAB.survtime, ch9$samplesAB.censor,
                       ch9$samplesAB, score.censored = FALSE)$pval.exact.stat,
               1.10595238)
  expect_equal(logrank(ch9$samplesAB.survtime, ch9$samplesAB.censor,
                       ch9$samplesAB, score.censored = FALSE)$pval.exact,
               0.076190476)
})

test_that("Example 9.6", {
  expect_equal(logrank(ch9$samplesXYZ.survtime, ch9$samplesXYZ.censor,
                       ch9$samplesXYZ)$pval.exact.stat, 1.34325397)
  expect_equal(logrank(ch9$samplesXYZ.survtime, ch9$samplesXYZ.censor,
                       ch9$samplesXYZ)$pval.exact, 0.206349206)
})

test_that("Exercise 9.6", {
  expect_equal(logrank(c(ch9$regimeA.survtime, ch9$regimeB.survtime),
                       c(ch9$regimeA.censor, ch9$regimeB.censor),
                       factor(c(rep("RegimeA", 12), rep("RegimeB", 13))),
                       score.censored = FALSE)$pval.exact, XXX)
})
