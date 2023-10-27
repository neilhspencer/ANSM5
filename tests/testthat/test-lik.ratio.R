test_that("Example 12.2", {
  site <- factor(c("Nose", "Ears", "Nose", "Throat", "Throat", "Nose", "Throat",
                   rep("Nose", 8), rep("Ears", 7), "Throat", "Ears"),
                 levels = c("Nose", "Throat", "Ears"))
  district <- factor(c(rep("A", 2), rep("B", 2), "C", rep("D", 2),rep("E", 15),
                       rep("F", 2)))
  expect_equal(lik.ratio(site, district, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp.stat, 17.3344364)
  expect_equal(lik.ratio(site, district, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp, 0.06728363)
  expect_equal(lik.ratio(site, district, seed = 1, nsims.mc = 100)$pval.mc, XXX)
})
