test_that("Example 6.11", {
  TypeA <- c(177, 200, 227, 230, 232, 268, 272, 297)
  TypeB <- c(47, 105, 126, 142, 158, 172, 197, 220, 225, 230, 262, 270)
  expect_equal(siegel.tukey(TypeA, TypeB)$pval.exact.stat,
               paste0("\n","106 (rank sum from TypeA), ",
                      "104 (rank sum from TypeB)", "\n",
                      "70 (Mann-Whitney U from TypeA), ",
                      "26 (Mann-Whitney U from TypeB)"))
  expect_equal(siegel.tukey(TypeA, TypeB)$pval.exact, 0.097880448)
})
