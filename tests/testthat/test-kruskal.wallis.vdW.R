test_that("Example 7.2", {
  expect_equal(
    kruskal.wallis.vdW(ch7$age, ch7$positions)$pval.exact.stat,
    2.33260844)
  expect_equal(round(kruskal.wallis.vdW(ch7$age, ch7$positions)$pval.exact, 4),
               0.3357)
  expect_equal(
    kruskal.wallis.vdW(ch7$age, ch7$positions, do.exact = FALSE,
                       do.asymp = TRUE)$pval.asymp.stat, 2.33260844)
  expect_equal(
    kruskal.wallis.vdW(ch7$age, ch7$positions, do.exact = FALSE,
                       do.asymp = TRUE)$pval.asymp, 0.31151611)
})
