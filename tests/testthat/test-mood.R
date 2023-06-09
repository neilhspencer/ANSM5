test_that("Example 6.12", {
  TypeA <- c(177, 200, 227, 230, 232, 268, 272, 297)
  TypeB <- c(47, 105, 126, 142, 158, 172, 197, 220, 225, 230, 262, 270)
  expect_equal(mood(TypeA, TypeB)$pval.exact.stat, 164)
  expect_equal(mood(TypeA, TypeB)$pval.exact, 0.12933238)
  expect_equal(mood(TypeA, TypeB, do.asymp = TRUE)$pval.asymp.stat, 164)
  expect_equal(mood(TypeA, TypeB, do.asymp = TRUE)$pval.asymp, 0.12562273)
})
