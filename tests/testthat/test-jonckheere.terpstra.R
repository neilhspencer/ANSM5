test_that("Example 7.3", {
  age <- c(66, 66, 69, 77, 77, 78, 40, 52, 56, 58, 66, 67, 55, 63)
  features <- as.factor(c(rep("1/2", 6), rep("3", 6), rep("4", 2)))
  expect_equal(jonckheere.terpstra(age, features)$pval.exact.stat, 9)
  expect_equal(jonckheere.terpstra(age, features)$pval.exact, 0.0046620047)
  expect_equal(jonckheere.terpstra(age, features, do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp.stat, -2.52810291)
  expect_equal(jonckheere.terpstra(age, features, do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp, 0.0057340365)
})
