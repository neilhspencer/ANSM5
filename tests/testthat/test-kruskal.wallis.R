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

test_that("Example 7.2", {
  time <- factor(c(
    rep("<100", 2), rep("100-199", 6), rep("200-299", 9), rep("300+", 0),
    rep("<100", 0), rep("100-199", 1), rep("200-299", 7), rep("300+", 0),
    rep("<100", 4), rep("100-199", 10), rep("200-299", 6), rep("300+", 0),
    rep("<100", 1), rep("100-199", 6), rep("200-299", 5), rep("300+", 0),
    rep("<100", 0), rep("100-199", 8), rep("200-299", 10), rep("300+", 2)),
    levels = c("<100", "100-199", "200-299", "300+"))
  cause <- factor(c(rep("A", 17), rep("B", 8), rep("C", 20), rep("D", 12),
                  rep("E", 20)), levels = c("A", "B", "C", "D", "E"))
  kw.out <- kruskal.wallis(as.numeric(time), cause, do.asymp = TRUE, nsims.mc = 100000, seed = 1)
  expect_equal(kw.out$pval.mc.stat, 10.444765)
  expect_equal(kw.out$pval.mc, 0.02859)
  expect_equal(kw.out$pval.asymp.stat, 10.444765)
  expect_equal(kw.out$pval.asymp, 0.033566401)
})
