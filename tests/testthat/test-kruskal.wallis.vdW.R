test_that("Example 7.2", {
  age <- c(55, 56, 66, 52, 58, 66, 67, 40, 63, 66, 69, 77, 77, 78)
  positions <- as.factor(c("CF", "CF", "CF", "CH", "CH", "CH", "CH", "Other",
                           "Other", "Other", "Other", "Other", "Other",
                           "Other"))
  expect_equal(kruskal.wallis.vdW(age, positions)$pval.exact.stat, 2.33260844)
  expect_equal(kruskal.wallis.vdW(age, positions)$pval.exact, 0.33568931)
  expect_equal(kruskal.wallis.vdW(age, positions, do.exact = FALSE,
                              do.asymp = TRUE)$pval.asymp.stat, 2.33260844)
  expect_equal(kruskal.wallis.vdW(age, positions, do.exact = FALSE,
                              do.asymp = TRUE)$pval.asymp, 0.31151611)
})
