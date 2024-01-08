test_that("Example 9.5", {
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
#  #coin
#  expect_equal(coin::logrank_test(
#    Surv(ch9$samplesAB.survtime, 1 - ch9$samplesAB.censor) ~ ch9$samplesAB,
#    type = "logrank", distribution = "exact")@statistic@linearstatistic[1],
#    -1.10595238)
#  expect_equal(pvalue(coin::logrank_test(
#    Surv(ch9$samplesAB.survtime, 1 - ch9$samplesAB.censor) ~ ch9$samplesAB,
#    type = "logrank", distribution = "exact"))[1], 0.33809524)
#  expect_equal(pvalue(coin::logrank_test(
#    Surv(ch9$samplesAB.survtime, 1 - ch9$samplesAB.censor) ~ ch9$samplesAB),
#    type = "logrank")[1], 0.3138191)
})

test_that("Example 9.6", {
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
#  #coin
#  expect_equal(coin::pvalue(logrank_test(
#    Surv(ch9$samplesXYZ.survtime, 1 - ch9$samplesXYZ.censor) ~ ch9$samplesXYZ))[1],
#    0.2947899)
#  expect_equal(coin::pvalue(logrank_test(
#    Surv(ch9$samplesXYZ.survtime, 1 - ch9$samplesXYZ.censor) ~ ch9$samplesXYZ,
#    type = "Peto-Peto"))[1], 0.28732927)
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
#  #coin
#  expect_equal(
#    coin::logrank_test(
#      Surv(c(ch9$regimeA.survtime, ch9$regimeB.survtime),
#           1 - c(ch9$regimeA.censor, ch9$regimeB.censor)) ~
#        factor(c(rep("RegimeA", 12), rep("RegimeB", 13))),
#               distribution = "exact")@statistic@linearstatistic[1], 2.3375973)
#  expect_equal(pvalue(coin::logrank_test(
#    Surv(c(ch9$regimeA.survtime, ch9$regimeB.survtime),
#         1 - c(ch9$regimeA.censor, ch9$regimeB.censor)) ~
#      factor(c(rep("RegimeA", 12), rep("RegimeB", 13))),
#    distribution = "exact"))[1], 0.249796166)
})
