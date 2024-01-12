test_that("Example 9.6", {
  #exact
  expect_equal(logrank(ch9$samplesAB.survtime, ch9$samplesAB.censor,
                       ch9$samplesAB, score.censored = FALSE)$pval.exact.stat,
               1.10595238)
  expect_equal(logrank(ch9$samplesAB.survtime, ch9$samplesAB.censor,
                       ch9$samplesAB, score.censored = FALSE)$pval.exact,
               0.33809524)
  #MC
  expect_equal(logrank(ch9$samplesAB.survtime, ch9$samplesAB.censor,
                       ch9$samplesAB, score.censored = FALSE,
                       max.exact.perms = 1, seed = 1)$pval.mc, 0.3411)
  expect_equal(logrank(ch9$samplesAB.survtime, ch9$samplesAB.censor,
                       ch9$samplesAB, score.censored = FALSE,
                       max.exact.perms = 1, seed = 1,
                       nsims.mc = 100000)$pval.mc, 0.3378)
})

test_that("Example 9.7", {
  expect_equal(logrank(ch9$samplesXYZ.survtime, ch9$samplesXYZ.censor,
                       ch9$samplesXYZ)$pval.exact.stat, 1.3702381)
  expect_equal(logrank(ch9$samplesXYZ.survtime, ch9$samplesXYZ.censor,
                       ch9$samplesXYZ)$pval.exact, 0.29761905)
  #MC
  expect_equal(logrank(ch9$samplesXYZ.survtime, ch9$samplesXYZ.censor,
                       ch9$samplesXYZ, max.exact.perms = 1,
                       seed = 1)$pval.mc.stat, 1.3702381)
  expect_equal(logrank(ch9$samplesXYZ.survtime, ch9$samplesXYZ.censor,
                       ch9$samplesXYZ, max.exact.perms = 1,
                       seed = 1)$pval.mc, 0.305)
  expect_equal(logrank(ch9$samplesXYZ.survtime, ch9$samplesXYZ.censor,
                       ch9$samplesXYZ, max.exact.perms = 1,
                       seed = 1, nsims.mc = 100000)$pval.mc, 0.29545)
})

test_that("Exercise 9.6", {
  expect_equal(logrank(c(ch9$regimeA.survtime, ch9$regimeB.survtime),
                       c(ch9$regimeA.censor, ch9$regimeB.censor),
                       factor(c(rep("RegimeA", 12), rep("RegimeB", 13))),
                       score.censored = FALSE,
                       max.exact.perms = 10000000)$pval.exact.stat, 2.3375973)
  expect_equal(logrank(c(ch9$regimeA.survtime, ch9$regimeB.survtime),
                       c(ch9$regimeA.censor, ch9$regimeB.censor),
                       factor(c(rep("RegimeA", 12), rep("RegimeB", 13))),
                       score.censored = FALSE,
                       max.exact.perms = 10000000)$pval.exact, 0.24978463)
  #MC
  expect_equal(logrank(c(ch9$regimeA.survtime, ch9$regimeB.survtime),
                       c(ch9$regimeA.censor, ch9$regimeB.censor),
                       factor(c(rep("RegimeA", 12), rep("RegimeB", 13))),
                       score.censored = FALSE, seed = 1)$pval.mc.stat,
               2.3375973)
  expect_equal(logrank(c(ch9$regimeA.survtime, ch9$regimeB.survtime),
                       c(ch9$regimeA.censor, ch9$regimeB.censor),
                       factor(c(rep("RegimeA", 12), rep("RegimeB", 13))),
                       score.censored = FALSE, seed = 1)$pval.mc, 0.2512)
  expect_equal(logrank(c(ch9$regimeA.survtime, ch9$regimeB.survtime),
                       c(ch9$regimeA.censor, ch9$regimeB.censor),
                       factor(c(rep("RegimeA", 12), rep("RegimeB", 13))),
                       score.censored = FALSE, seed = 1,
                       nsims.mc = 100000)$pval.mc, 0.24967)
})
