test_that("Example 8.6", {
  weight <- c(16.2, 16.6, 17.1, 17.3, 17.5, 17.7, 17.2, 17.4, 17.5, 17.9, 18.1,
              18.3, 18.2, 18.5, 18.6, 19.1, 19.2, 19.5, 22.1, 22.9, 23.6, 23.9,
              24.1, 24.3)
  factorA <- as.factor(c(rep("A1", 12), rep("A2", 12)))
  factorB <- as.factor(c(rep("B1", 6), rep("B2", 6), rep("B1", 6),
                         rep("B2", 6)))
  expect_equal(hettmansperger.elmore(weight, factorA, factorB)$pval.asymp.stat,
               16.8033333)
  expect_equal(hettmansperger.elmore(weight, factorA, factorB)$pval.asymp,
               4.1460411e-05)
})

test_that("Example 8.7", {
  weight <- c(16.2, 16.6, 17.1, 17.3, 17.5, 17.7, 17.2, 17.4, 17.5, 17.9, 18.1,
              18.3, 18.2, 18.5, 18.6, 19.1, 19.2, 19.5, 22.1, 22.9, 23.6, 23.9,
              24.1, 16.0)
  factorA <- as.factor(c(rep("A1", 12), rep("A2", 12)))
  factorB <- as.factor(c(rep("B1", 6), rep("B2", 6), rep("B1", 6),
                         rep("B2", 6)))
  expect_equal(hettmansperger.elmore(weight, factorA, factorB,
                                     median.polish = TRUE)$pval.asymp.stat,
               9.72)
  expect_equal(hettmansperger.elmore(weight, factorA, factorB,
                                     median.polish = TRUE)$pval.asymp,
               0.00182273517)
})
