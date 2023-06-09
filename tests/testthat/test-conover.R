test_that("Example 6.13", {
  TypeA <- c(177, 200, 227, 230, 232, 268, 272, 297)
  TypeB <- c(47, 105, 126, 142, 158, 172, 197, 220, 225, 230, 262, 270)
  expect_equal(conover(TypeA, TypeB)$pval.exact.stat, 720)
  expect_equal(conover(TypeA, TypeB)$pval.exact, 0.126728586)
  expect_equal(conover(TypeA, TypeB, do.asymp = TRUE)$pval.asymp.stat, 720)
  expect_equal(conover(TypeA, TypeB, do.asymp = TRUE)$pval.asymp, 0.110647884)
})
