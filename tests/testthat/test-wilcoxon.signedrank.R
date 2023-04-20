test_that("Example 3.4", {
  heartrates1 <- c(73, 82, 87, 68, 106, 60, 97)
  expect_equal(wilcoxon.signedrank(heartrates1, 70, "greater")$pval.exact.stat,
               "4 (sum of negative ranks), 24 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(heartrates1, 70,
                                         "greater")$pval.exact, 0.0546875)
})

test_that("Example 3.5", {
  heartrates2 <- c(-2, 4, 8, 25 ,-5 ,16 ,3 ,1 ,12 ,17 ,20 ,9)
  expect_equal(wilcoxon.signedrank(heartrates2, 15, "less")$pval.exact.stat,
               "64 (sum of negative ranks), 14 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(heartrates2, 15,
                                   "less")$pval.exact, 0.026123047)
  expect_equal(wilcoxon.signedrank(heartrates2, 15)$pval.exact.stat,
               "64 (sum of negative ranks), 14 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(heartrates2, 15)$pval.exact, 0.052246094)
  expect_equal(wilcoxon.signedrank(heartrates2, 15, do.asymp = TRUE,
                                   cont.corr = FALSE)$pval.asymp, 0.049860204)
  expect_equal(wilcoxon.signedrank(heartrates2, 15, do.asymp = TRUE)$pval.asymp, 0.054613544)
})

test_that("Example 3.6", {
  heartrates2 <- c(-2, 4, 8, 25 ,-5 ,16 ,3 ,1 ,12 ,17 ,20 ,9)
  expect_equal(wilcoxon.signedrank(heartrates2)$CI.exact.lower, 2.5)
  expect_equal(wilcoxon.signedrank(heartrates2)$CI.exact.upper, 16)
  expect_equal(wilcoxon.signedrank(heartrates2)$actualCIwidth.exact, 0.95751953)
  expect_equal(wilcoxon.signedrank(heartrates2, CI.width = 0.99)$CI.exact.lower, 0.5)
  expect_equal(wilcoxon.signedrank(heartrates2, CI.width = 0.99)$CI.exact.upper, 18)
  expect_equal(wilcoxon.signedrank(heartrates2, CI.width = 0.99)$actualCIwidth.exact, 0.99072266)
})

test_that("Example 3.7", {
  heartrates2 <- c(-2, 4, 8, 25 ,-5 ,16 ,3 ,1 ,12 ,17 ,20 ,9)
  expect_equal(wilcoxon.signedrank(heartrates2, 15, do.asymp = TRUE)$pval.asymp, 0.054613544)
  expect_equal(wilcoxon.signedrank(heartrates2, 15, do.asymp = TRUE,
                                   cont.corr = FALSE)$pval.asymp, 0.049860204)
})

test_that("Example 3.8", {
  withties <- c(4, 4, 8, 8, 11, 11, 11)
  tiedifrounded1 <- c(3.9, 4.1, 8.2, 8.3, 10.9, 11.0, 11.1)
  tiedifrounded2 <- c(3.9, 4.0, 7.8, 7.9, 10.9, 11.0, 11.1)
  expect_equal(wilcoxon.signedrank(withties, 6, "greater")$pval.exact.stat,
               "5 (sum of negative ranks), 23 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(withties, 6, "greater")$pval.exact, 0.0859375)
  expect_equal(wilcoxon.signedrank(tiedifrounded1, 6, "greater")$pval.exact.stat,
               "3 (sum of negative ranks), 25 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(tiedifrounded1, 6, "greater")$pval.exact, 0.0390625)
  expect_equal(wilcoxon.signedrank(tiedifrounded2, 6, "greater")$pval.exact.stat,
               "7 (sum of negative ranks), 21 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(tiedifrounded2, 6, "greater")$pval.exact, 0.1484375)
})

test_that("Example 3.9", {
  ages <- c(6, 18, 24, 26, 37, 40, 42, 47, 69, 69, 87, 130)
  expect_equal(wilcoxon.signedrank(ages, 30, "greater")$pval.exact.stat,
               "16.5 (sum of negative ranks), 61.5 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(ages, 30, "greater")$pval.exact, 0.040527344)
})

test_that("Example 3.12", {
  sampleI <- c(5.5, 6.0, 6.5, 7.6, 7.6, 7.7, 8.0, 8.2, 9.1, 15.1)
  sampleII <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5,
                8.7, 9.4, 14.3, 26.0)
  expect_equal(wilcoxon.signedrank(sampleI, 9)$pval.exact, 0.1015625)
  expect_equal(wilcoxon.signedrank(sampleI, 9, do.asymp = TRUE)$pval.asymp, 0.092388857)
  expect_equal(wilcoxon.signedrank(sampleII, 9)$pval.exact, 0.0148468018)
  expect_equal(wilcoxon.signedrank(sampleII, 9, do.asymp = TRUE)$pval.asymp, 0.0168522505)
})

test_that("Example 3.13", {
  sampleII <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5,
                8.7, 9.4, 14.3, 26.0)
  expect_equal(wilcoxon.signedrank(sampleII, do.asymp = TRUE)$CI.asymp.lower, 7.15)
  expect_equal(wilcoxon.signedrank(sampleII, do.asymp = TRUE)$CI.asymp.upper, 8.45)
})

test_that("Example 3.14", {
  heartrates2a <- c(-2, 4, 8, 35 ,-5 ,16 ,3 ,1 ,12 ,17 ,20 ,9)
  heartrates2b <- c(-2, 4, 8, 65 ,-5 ,16 ,3 ,1 ,12 ,17 ,20 ,9)
  expect_equal(wilcoxon.signedrank(heartrates2a)$CI.exact.lower, 2.5)
  expect_equal(wilcoxon.signedrank(heartrates2a)$CI.exact.upper, 17)
  expect_equal(wilcoxon.signedrank(heartrates2a, CI.width = 0.99)$CI.exact.lower, 0.5)
  expect_equal(wilcoxon.signedrank(heartrates2a, CI.width = 0.99)$CI.exact.upper, 20)
  expect_equal(wilcoxon.signedrank(heartrates2b)$CI.exact.lower, 2.5)
  expect_equal(wilcoxon.signedrank(heartrates2b)$CI.exact.upper, 18.5)
  expect_equal(wilcoxon.signedrank(heartrates2b, CI.width = 0.99)$CI.exact.lower, 0.5)
  expect_equal(wilcoxon.signedrank(heartrates2b, CI.width = 0.99)$CI.exact.upper, 34.5)
})

test_that("Exercise 3.7", {
  fishlengths <- c(64, rep(65, 2), 66, 67, rep(68, 4), rep(69, 3), rep(70, 4),
                   rep(71, 5), rep(72, 3), rep(73, 3), 75, rep(77, 6), 78, 83)
  expect_equal(wilcoxon.signedrank(fishlengths, 73.5)$pval.exact, 0.0093478277)
  expect_equal(wilcoxon.signedrank(fishlengths, 73.5, do.asymp = TRUE)$pval.asymp, 0.0104156196)
})

test_that("Exercise 3.8", {
  ages <- c(6, 18, 24, 26, 37, 40, 42, 47, 69, 69, 87, 130)
  expect_equal(wilcoxon.signedrank(ages)$CI.exact.lower, 26.5)
  expect_equal(wilcoxon.signedrank(ages)$CI.exact.upper, 74)
  expect_equal(wilcoxon.signedrank(ages, CI.width = 0.99)$CI.exact.lower, 22)
  expect_equal(wilcoxon.signedrank(ages, CI.width = 0.99)$CI.exact.upper, 85)
})

test_that("Exercise 3.11", {
  fishlengths <- c(64, rep(65, 2), 66, 67, rep(68, 4), rep(69, 3), rep(70, 4),
                   rep(71, 5), rep(72, 3), rep(73, 3), 75, rep(77, 6), 78, 83)
  expect_equal(wilcoxon.signedrank(fishlengths, do.asymp = TRUE)$CI.exact.lower, 70)
  expect_equal(wilcoxon.signedrank(fishlengths, do.asymp = TRUE)$CI.exact.upper, 73)
})

test_that("Exercise 3.15", {
  assembly <- c(14.2, 11.3, 12.7, 19.2, 13.5, 14.4, 11.8, 15.1, 12.3, 11.7,
                13.2, 13.4, 14.0, 14.1, 13.7, 11.9, 11.8, 10.7, 11.3, 12.2)
  expect_equal(wilcoxon.signedrank(assembly, 14.2, do.asymp = TRUE)$pval.exact, 0.0050659180)
  expect_equal(wilcoxon.signedrank(assembly, 14.2, do.asymp = TRUE)$CI.exact.lower, 12.25)
  expect_equal(wilcoxon.signedrank(assembly, 14.2, do.asymp = TRUE)$CI.exact.upper, 13.7)
})

test_that("Exercise 3.16", {
  weightchange <- c(-1.2, 1.4, 0.2, -0.7, -6.4, -2.7, -8.6, -1.7, -2.2,
                    0.1, -0.4, -4.2, -1.6, 1.2, -1.3, -2.4, 3.1, -0.2,
                    -4.5, -6.3, -1.7, 0.0, 0.2, -3.7, 1.1, -2.3, -0.1,
                    -7.3, 0.2, -1.4, -0.9, -2.0, 0.0, 1.1, -0.3, -1.1)
  expect_equal(wilcoxon.signedrank(weightchange, 0)$pval.exact, 0.00041832903)
  expect_equal(wilcoxon.signedrank(weightchange)$CI.exact.lower, -2.15)
  expect_equal(wilcoxon.signedrank(weightchange)$CI.exact.upper, -0.55)
})

test_that("Exercise 3.17", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(wilcoxon.signedrank(sampleI, 110)$pval.exact, 0.044921875)
  expect_equal(wilcoxon.signedrank(sampleI, 110, do.asymp = TRUE)$pval.asymp, 0.046710479)
  expect_equal(wilcoxon.signedrank(sampleII, 110)$pval.exact, 0.0175762177)
  expect_equal(wilcoxon.signedrank(sampleII, 110, do.asymp = TRUE)$pval.asymp, 0.019622174)
})

test_that("Example 5.1", {
  diffs <- c(-1, 2, 7, 14, 16, 16, 19, 20, 20, 22, 30, 33)
  expect_equal(wilcoxon.signedrank(diffs, 0)$pval.exact, 0.0009765625)
  expect_equal(wilcoxon.signedrank(diffs)$CI.exact.lower, 9.5)
  expect_equal(wilcoxon.signedrank(diffs)$CI.exact.upper, 23.5)
})

test_that("Example 5.2", {
  arithmetic <- c(8, 6, 14, 5, 10, 2, 9, 19, 11, 4, 5)
  expect_equal(wilcoxon.signedrank(arithmetic, 10)$pval.exact, 0.248046875)
  expect_equal(wilcoxon.signedrank(arithmetic, 10, do.asymp = TRUE)$pval.asymp,
               0.22887085)
  expect_equal(wilcoxon.signedrank(arithmetic, 10, do.asymp = TRUE)$CI.asymp.lower, 5)
  expect_equal(wilcoxon.signedrank(arithmetic, 10, do.asymp = TRUE)$CI.asymp.upper, 12)
})

test_that("Example 5.3", {
  bp1 <- c(-5, -5, 0, 2, 10, 15, 15, 15, 18, 20, 20, 20, 20, 22, 30, 30, 34, 40, 40, 40, 41, 47, 80, 85)
  expect_equal(wilcoxon.signedrank(bp1)$CI.exact.lower, 17.5)
  expect_equal(wilcoxon.signedrank(bp1)$CI.exact.upper, 33.5)
  bp2<- c(-5, -103, 0, 2, 10, 15, 15, 15, 18, 20, 20, 20, 20, 22, 30, 30, 34, 40, 40, 40, 41, 47, 80, 85)
  expect_equal(wilcoxon.signedrank(bp2)$CI.exact.lower, 16)
  expect_equal(wilcoxon.signedrank(bp2)$CI.exact.upper, 32.5)
})
