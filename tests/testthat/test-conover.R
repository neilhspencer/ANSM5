test_that("Example 6.13", {
  TypeA <- c(177, 200, 227, 230, 232, 268, 272, 297)
  TypeB <- c(47, 105, 126, 142, 158, 172, 197, 220, 225, 230, 262, 270)
  expect_equal(conover(TypeA, TypeB)$pval.exact.stat, 720)
  expect_equal(conover(TypeA, TypeB)$pval.exact, 0.126728586)
  expect_equal(conover(TypeA, TypeB, do.asymp = TRUE)$pval.asymp.stat, 720)
  expect_equal(conover(TypeA, TypeB, do.asymp = TRUE)$pval.asymp, 0.110647884)
})

test_that("Example 7.9", {
  age <- c(66, 66, 69, 77, 77, 78, 40, 52, 56, 58, 66, 67, 55, 63)
  features <- as.factor(c(rep("1/2", 6), rep("3", 6), rep("4", 2)))
  expect_equal(conover(age, features)$pval.exact.stat, 9)
  expect_equal(conover(age, features)$pval.exact, 0.0046620047)
  expect_equal(conover(age, features, do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp.stat, -2.52810291)
  expect_equal(conover(age, features, do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp, 0.0057340365)
})
