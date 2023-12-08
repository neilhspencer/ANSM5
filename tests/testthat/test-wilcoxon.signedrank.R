test_that("Example 3.4", {
  expect_equal(wilcoxon.signedrank(ch3data$heartrates1, 70, "greater")$pval.exact.stat,
               "4 (sum of negative ranks), 24 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(ch3data$heartrates1, 70,
                                   "greater")$pval.exact, 0.0546875)
})

test_that("Example 3.5", {
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, 15, "less")$pval.exact.stat,
               "64 (sum of negative ranks), 14 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, 15,
                                   "less")$pval.exact, 0.026123047)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, 15)$pval.exact.stat,
               "64 (sum of negative ranks), 14 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, 15)$pval.exact, 0.052246094)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, 15, do.asymp = TRUE,
                                   cont.corr = FALSE)$pval.asymp, 0.049860204)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, 15, do.asymp = TRUE)$pval.asymp, 0.054613544)
})

test_that("Example 3.6", {
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2)$CI.exact.lower, 2.5)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2)$CI.exact.upper, 16)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2)$actualCIwidth.exact, 0.95751953)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, CI.width = 0.99)$CI.exact.lower, 0.5)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, CI.width = 0.99)$CI.exact.upper, 18)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, CI.width = 0.99)$actualCIwidth.exact, 0.99072266)
})

test_that("Example 3.7", {
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, 15, do.asymp = TRUE)$pval.asymp, 0.054613544)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2, 15, do.asymp = TRUE,
                                   cont.corr = FALSE)$pval.asymp, 0.049860204)
})

test_that("Example 3.8", {
  expect_equal(wilcoxon.signedrank(ch3data$withties, 6, "greater")$pval.exact.stat,
               "5 (sum of negative ranks), 23 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(ch3data$withties, 6, "greater")$pval.exact, 0.0859375)
  expect_equal(wilcoxon.signedrank(ch3data$tiedifrounded1, 6, "greater")$pval.exact.stat,
               "3 (sum of negative ranks), 25 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(ch3data$tiedifrounded1, 6, "greater")$pval.exact, 0.0390625)
  expect_equal(wilcoxon.signedrank(ch3data$tiedifrounded2, 6, "greater")$pval.exact.stat,
               "7 (sum of negative ranks), 21 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(ch3data$tiedifrounded2, 6, "greater")$pval.exact, 0.1484375)
})

test_that("Example 3.9", {
  expect_equal(wilcoxon.signedrank(ch3data$ages, 30, "greater")$pval.exact.stat,
               "16.5 (sum of negative ranks), 61.5 (sum of positive ranks)")
  expect_equal(wilcoxon.signedrank(ch3data$ages, 30, "greater")$pval.exact, 0.040527344)
})

test_that("Example 3.12", {
  expect_equal(wilcoxon.signedrank(ch3data$sampleA, 9)$pval.exact, 0.1015625)
  expect_equal(wilcoxon.signedrank(ch3data$sampleA, 9, do.asymp = TRUE)$pval.asymp, 0.092388857)
  expect_equal(wilcoxon.signedrank(ch3data$sampleB, 9)$pval.exact, 0.0148468018)
  expect_equal(wilcoxon.signedrank(ch3data$sampleB, 9, do.asymp = TRUE)$pval.asymp, 0.0168522505)
})

test_that("Example 3.13", {
  expect_equal(wilcoxon.signedrank(ch3data$sampleB, do.asymp = TRUE)$CI.asymp.lower, 7.15)
  expect_equal(wilcoxon.signedrank(ch3data$sampleB, do.asymp = TRUE)$CI.asymp.upper, 8.45)
})

test_that("Example 3.14", {
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2a)$CI.exact.lower, 2.5)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2a)$CI.exact.upper, 17)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2a, CI.width = 0.99)$CI.exact.lower, 0.5)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2a, CI.width = 0.99)$CI.exact.upper, 20)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2b)$CI.exact.lower, 2.5)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2b)$CI.exact.upper, 18.5)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2b, CI.width = 0.99)$CI.exact.lower, 0.5)
  expect_equal(wilcoxon.signedrank(ch3data$heartrates2b, CI.width = 0.99)$CI.exact.upper, 34.5)
})

test_that("Exercise 3.6", {
  expect_equal(wilcoxon.signedrank(ch3data$children, 400)$pval.exact, 0.12939453)
  expect_equal(wilcoxon.signedrank(ch3data$children, 400)$CI.exact.lower, 200.5)
  expect_equal(wilcoxon.signedrank(ch3data$children, 400)$CI.exact.upper, 433)
})

test_that("Exercise 3.7", {
  expect_equal(wilcoxon.signedrank(ch3data$fishlengths, 73.5)$pval.exact, 0.0093478277)
  expect_equal(wilcoxon.signedrank(ch3data$fishlengths, 73.5, do.asymp = TRUE)$pval.asymp, 0.0104156196)
})

test_that("Exercise 3.8", {
  expect_equal(wilcoxon.signedrank(ch3data$ages)$CI.exact.lower, 26.5)
  expect_equal(wilcoxon.signedrank(ch3data$ages)$CI.exact.upper, 74)
  expect_equal(wilcoxon.signedrank(ch3data$ages, CI.width = 0.99)$CI.exact.lower, 22)
  expect_equal(wilcoxon.signedrank(ch3data$ages, CI.width = 0.99)$CI.exact.upper, 85)
})

test_that("Exercise 3.10", {
  expect_equal(wilcoxon.signedrank(ch3data$sleeptime, 2, alternative = "greater")$pval.exact, 0.024902344)
})

test_that("Exercise 3.11", {
  expect_equal(wilcoxon.signedrank(ch3data$fishlengths, do.asymp = TRUE)$CI.exact.lower, 70)
  expect_equal(wilcoxon.signedrank(ch3data$fishlengths, do.asymp = TRUE)$CI.exact.upper, 73)
})

test_that("Exercise 3.12", {
  expect_equal(wilcoxon.signedrank(ch3data$weightloss, 5)$pval.exact, 0.03137207)
})

test_that("Exercise 3.13", {
  expect_equal(wilcoxon.signedrank(ch3data$plants, 50)$pval.exact, 0.65339544)
})

test_that("Exercise 3.14", {
  expect_equal(wilcoxon.signedrank(ch3data$birthprops, CI.width = 0.99)$CI.exact.lower, 0.28250)
  expect_equal(wilcoxon.signedrank(ch3data$birthprops, CI.width = 0.99)$CI.exact.upper, 0.29550)
})

test_that("Exercise 3.15", {
  expect_equal(wilcoxon.signedrank(ch3data$assembly, 14.2, do.asymp = TRUE)$pval.exact, 0.0050659180)
  expect_equal(wilcoxon.signedrank(ch3data$assembly, 14.2, do.asymp = TRUE)$CI.exact.lower, 12.25)
  expect_equal(wilcoxon.signedrank(ch3data$assembly, 14.2, do.asymp = TRUE)$CI.exact.upper, 13.7)
})

test_that("Exercise 3.16", {
  expect_equal(wilcoxon.signedrank(ch3data$weightchange, 0)$pval.exact, 0.00041832903)
  expect_equal(wilcoxon.signedrank(ch3data$weightchange)$CI.exact.lower, -2.15)
  expect_equal(wilcoxon.signedrank(ch3data$weightchange)$CI.exact.upper, -0.55)
})

test_that("Exercise 3.17", {
  expect_equal(wilcoxon.signedrank(ch3data$sampleI, 110)$pval.exact, 0.044921875)
  expect_equal(wilcoxon.signedrank(ch3data$sampleI, 110, do.asymp = TRUE)$pval.asymp, 0.046710479)
  expect_equal(wilcoxon.signedrank(ch3data$sampleII, 110)$pval.exact, 0.0175762177)
  expect_equal(wilcoxon.signedrank(ch3data$sampleII, 110, do.asymp = TRUE)$pval.asymp, 0.019622174)
})

test_that("Exercise 4.15", {
  expect_equal(wilcoxon.signedrank(ch4data$arrow.angles, 145)$pval.exact,
               0.048828125)
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
