test_that("Example 9.3", {
  expect_equal(peto.wilcoxon(ch9$symp.survtime, ch9$asymp.survtime,
                             ch9$symp.censor, ch9$asymp.censor,
                             alternative = "less", seed = 1)$pval.mc.stat,
               622.96939)
  expect_equal(peto.wilcoxon(ch9$symp.survtime, ch9$asymp.survtime,
                             ch9$symp.censor, ch9$asymp.censor,
                             alternative = "less", seed = 1)$pval.mc, 0.0034)
})

test_that("Example 9.4", {
  expect_equal(peto.wilcoxon(ch9$sampleI.survtime, ch9$sampleII.survtime,
                             ch9$sampleI.censor, ch9$sampleII.censor,
                             alternative = "less")$pval.exact.stat, 66.25)
  expect_equal(peto.wilcoxon(ch9$sampleI.survtime, ch9$sampleII.survtime,
                             ch9$sampleI.censor, ch9$sampleII.censor,
                             alternative = "less")$pval.exact, 0.048174048)
})

