test_that("Example 7.2", {
  tmp <- kruskal.wallis.vdW(ch7$age, ch7$positions)
  expect_equal(tmp$pval.exact.stat, 2.33260844)
  expect_equal(round(tmp$pval.exact, 3), 0.336)
  expect_equal(
    kruskal.wallis.vdW(ch7$age, ch7$positions, do.exact = FALSE,
                       do.asymp = TRUE)$pval.asymp.stat, 2.33260844)
  expect_equal(
    kruskal.wallis.vdW(ch7$age, ch7$positions, do.exact = FALSE,
                       do.asymp = TRUE)$pval.asymp, 0.31151611)
})
