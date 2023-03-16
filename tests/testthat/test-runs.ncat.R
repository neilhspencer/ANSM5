test_that("Example 4.15", {
  births <- c("A", "A", "A", "A", "B", "A", "C", "C", "A", "A", "A", "A", "A",
              "D", "D", "A", "A")
  expect_equal(runs.ncat(births, alternative = "less", cont.corr = TRUE)$pval.asymp.stat, -1.15433076)
  expect_equal(runs.ncat(births, alternative = "less", cont.corr = TRUE)$pval.asymp, 0.124182295)
  expect_equal(runs.ncat(births, alternative = "less", do.mc = TRUE,
                         nsims.mc = 100000, seed = 1)$pval.mc, 0.09714)
})
