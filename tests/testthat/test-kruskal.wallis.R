test_that("Example 7.1", {
  affordability <- c(3.4, 3.7, 4.5, 4.0, 4.3, 5.2, 5.9, 6.2, 7.8, 10.2)
  regions <- as.factor(c("A", "A", "A", "B", "B", "B", "B", "C", "C", "C"))
  expect_equal(kruskal.wallis(affordability, regions)$pval.exact.stat,
               6.7454545)
  expect_equal(kruskal.wallis(affordability, regions)$pval.exact, 0.01)
  expect_equal(kruskal.wallis(affordability, regions, do.exact = FALSE,
                              do.asymp = TRUE)$pval.asymp.stat, 6.7454545)
  expect_equal(kruskal.wallis(affordability, regions, do.exact = FALSE,
                              do.asymp = TRUE)$pval.asymp, 0.034295975)
})

test_that("Example 7.2", {
  age <- c(55, 56, 66, 52, 58, 66, 67, 40, 63, 66, 69, 77, 77, 78)
  positions <- as.factor(c("CF", "CF", "CF", "CH", "CH", "CH", "CH", "Other",
                           "Other", "Other", "Other", "Other", "Other",
                           "Other"))
  expect_equal(kruskal.wallis(age, positions)$pval.exact.stat, 2.734127)
  expect_equal(kruskal.wallis(age, positions)$pval.exact, 0.270820846)
  expect_equal(kruskal.wallis(age, positions, do.exact = FALSE,
                              do.asymp = TRUE)$pval.asymp.stat, 2.734127)
  expect_equal(kruskal.wallis(age, positions, do.exact = FALSE,
                              do.asymp = TRUE)$pval.asymp, 0.25485424)
})
