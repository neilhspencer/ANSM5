test_that("Example 6.16", {
  Females <- c(0.45, 0.60, 0.80, 0.85, 0.95, 1.00, 1.75)
  Males <- c(0.40, 0.50, 0.55, 0.65, 0.70, 0.75, 0.90, 1.05, 1.15, 1.25, 1.30,
             1.35, 1.45, 1.50, 1.85, 1.90, 2.30, 2.55, 2.70, 2.85, 3.85)
  expect_equal(cramer.von.mises(Females, Males)$pval.stat, 0.33418367)
  expect_equal(cramer.von.mises(Females, Males)$pval, "p-value >= 0.10")
  expect_equal(cramer.von.mises(Females, Males, alternative = "greater")$pval,
               "p-value >= 0.05")
})
