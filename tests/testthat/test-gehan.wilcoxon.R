test_that("Example 9.1", {
  tmp <- gehan.wilcoxon(ch9$symp.survtime, ch9$asymp.survtime, ch9$symp.censor,
                        ch9$asymp.censor, alternative = "less", seed = 1)
  expect_equal(tmp$pval.mc.stat, 217.5)
  expect_equal(tmp$pval.mc, 0.00372)
  tmp <- gehan.wilcoxon(ch9$symp.survtime, ch9$asymp.survtime,
                        ch9$symp.censor, ch9$asymp.censor,
                        alternative = "less", do.exact = FALSE,
                        do.asymp = TRUE)
  expect_equal(tmp$pval.asymp.stat, 217.5)
  expect_equal(tmp$pval.asymp, 0.0050981517)
})

test_that("Exercise 9.5", {
  expect_equal(gehan.wilcoxon(ch9$regimeA.survtime, ch9$regimeB.survtime,
                              ch9$regimeA.censor, ch9$regimeB.censor,
                              seed = 1)$pval.mc, 0.61121)
})
