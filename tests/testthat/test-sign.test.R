test_that("Example 3.1", {
  expect_equal(sign.test(ch3data$sampleI, 110)$pval.exact, 0.109375)
  expect_equal(sign.test(ch3data$sampleII, 110)$pval.exact, 0.041389465)
})

test_that("Example 3.2", {
  expect_equal(sign.test(ch3data$sampleI)$actualCIwidth.exact, 0.97851562)
  expect_equal(sign.test(ch3data$sampleI)$CI.exact.lower, 11)
  expect_equal(sign.test(ch3data$sampleI)$CI.exact.upper, 151)
  expect_equal(sign.test(ch3data$sampleII)$actualCIwidth.exact, 0.95861053)
  expect_equal(sign.test(ch3data$sampleII)$CI.exact.lower, 40)
  expect_equal(sign.test(ch3data$sampleII)$CI.exact.upper, 108)
})

test_that("Example 3.3", {
  expect_equal(sign.test(ch3data$sampleI, 110, do.asymp = TRUE)$pval.asymp, 0.113846298)
  expect_equal(sign.test(ch3data$sampleI, 110, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.057779571)
  expect_equal(sign.test(ch3data$sampleII, 110, do.asymp = TRUE)$pval.asymp, 0.044171345)
  expect_equal(sign.test(ch3data$sampleII, 110, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.025347319)
})

test_that("Example 3.12", {
  expect_equal(sign.test(ch3data$sampleA, 9)$pval.exact, 0.109375)
  expect_equal(sign.test(ch3data$sampleA, 9, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.057779571)
  expect_equal(sign.test(ch3data$sampleA, 9, do.asymp = TRUE)$pval.asymp, 0.113846298)
  expect_equal(sign.test(ch3data$sampleB, 9)$pval.exact, 0.002576828)
  expect_equal(sign.test(ch3data$sampleB, 9, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.0017451187)
  expect_equal(sign.test(ch3data$sampleB, 9, do.asymp = TRUE)$pval.asymp, 0.0036504344)
})

test_that("Example 3.13", {
  expect_equal(sign.test(ch3data$sampleB, do.asymp = TRUE)$CI.asymp.lower, 7)
  expect_equal(sign.test(ch3data$sampleB, do.asymp = TRUE)$CI.asymp.upper, 8.2)
  expect_equal(sign.test(ch3data$sampleB, 6.95, "greater")$pval.exact, 0.05765915)
  expect_equal(sign.test(ch3data$sampleB, 8.25, "less")$pval.exact, 0.05765915)
  expect_equal(sign.test(ch3data$sampleB, 7.05, "greater")$pval.exact, 0.131587982)
  expect_equal(sign.test(ch3data$sampleB, 8.15, "less")$pval.exact, 0.131587982)
})

test_that("Example 3.14", {
  expect_equal(sign.test(ch3data$heartrates2a)$CI.exact.lower, 1)
  expect_equal(sign.test(ch3data$heartrates2a)$CI.exact.upper, 17)
  expect_equal(sign.test(ch3data$heartrates2a, CI.width = 0.99)$CI.exact.lower, -2)
  expect_equal(sign.test(ch3data$heartrates2a, CI.width = 0.99)$CI.exact.upper, 20)
  expect_equal(sign.test(ch3data$heartrates2b)$CI.exact.lower, 1)
  expect_equal(sign.test(ch3data$heartrates2b)$CI.exact.upper, 17)
  expect_equal(sign.test(ch3data$heartrates2b, CI.width = 0.99)$CI.exact.lower, -2)
  expect_equal(sign.test(ch3data$heartrates2b, CI.width = 0.99)$CI.exact.upper, 20)
})

test_that("Exercise 3.1", {
  expect_equal(sign.test(ch3data$sampleIa, 110)$pval.exact, 0.34375)
})

test_that("Exercise 3.3", {
  expect_equal(sign.test(ch3data$parkingtime, 20)$pval.exact, 0.098737147)
  expect_equal(sign.test(ch3data$parkingtime, 20, do.asymp = TRUE)$pval.asymp, 0.100348246)
  expect_equal(sign.test(ch3data$parkingtime, 20, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.067889155)
})

test_that("Exercise 3.10", {
  expect_equal(sign.test(ch3data$sleeptime, 2, alternative = "greater")$pval.exact, 0.032714844)
})

test_that("Exercise 3.11", {
  expect_equal(sign.test(ch3data$fishlengths)$actualCIwidth, 0.97118328)
  expect_equal(sign.test(ch3data$fishlengths)$CI.exact.lower, 69)
  expect_equal(sign.test(ch3data$fishlengths)$CI.exact.upper, 73)
  expect_equal(sign.test(ch3data$fishlengths, CI.width = 0.90)$actualCIwidth, 0.93475466)
  expect_equal(sign.test(ch3data$fishlengths, CI.width = 0.90)$CI.exact.lower, 70)
  expect_equal(sign.test(ch3data$fishlengths, CI.width = 0.90)$CI.exact.upper, 72)
})

test_that("Exercise 3.13", {
  expect_equal(sign.test(ch3data$plants, 50)$pval.exact, 0.33678364)
})

test_that("Exercise 3.14", {
  expect_equal(sign.test(ch3data$birthprops, CI.width = 0.99)$CI.exact.lower, 0.27700)
  expect_equal(sign.test(ch3data$birthprops, CI.width = 0.99)$CI.exact.upper, 0.29800)
})

test_that("Exercise 3.15", {
  expect_equal(sign.test(ch3data$assembly)$CI.exact.lower, 11.8)
  expect_equal(sign.test(ch3data$assembly)$CI.exact.upper, 14)
})

test_that("Exercise 3.16", {
  expect_equal(sign.test(ch3data$weightchange)$CI.exact.lower, -2)
  expect_equal(sign.test(ch3data$weightchange)$CI.exact.upper, -0.1)
})

test_that("Exercise 3.17", {
  expect_equal(sign.test(ch3data$sampleI, 110)$pval.exact, 0.109375)
  expect_equal(sign.test(ch3data$sampleI, 110, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.057779571)
  expect_equal(sign.test(ch3data$sampleI, 110, do.asymp = TRUE)$pval.asymp, 0.113846298)
  expect_equal(sign.test(ch3data$sampleII, 110)$pval.exact, 0.041389465)
  expect_equal(sign.test(ch3data$sampleII, 110, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.025347319)
  expect_equal(sign.test(ch3data$sampleII, 110, do.asymp = TRUE)$pval.asymp, 0.044171345)
})

test_that("Example 5.1", {
  diffs <- c(-1, 2, 7, 14, 16, 16, 19, 20, 20, 22, 30, 33)
  expect_equal(sign.test(diffs, 0)$pval.exact, 0.0063476563)
  expect_equal(sign.test(diffs)$CI.exact.lower, 7)
  expect_equal(sign.test(diffs)$CI.exact.upper, 22)
})

test_that("Example 5.3", {
  bp1 <- c(-5, -5, 0, 2, 10, 15, 15, 15, 18, 20, 20, 20, 20, 22, 30, 30, 34, 40, 40, 40, 41, 47, 80, 85)
  expect_equal(sign.test(bp1)$CI.exact.lower, 15)
  expect_equal(sign.test(bp1)$CI.exact.upper, 40)
  bp2<- c(-5, -103, 0, 2, 10, 15, 15, 15, 18, 20, 20, 20, 20, 22, 30, 30, 34, 40, 40, 40, 41, 47, 80, 85)
  expect_equal(sign.test(bp2)$CI.exact.lower, 15)
  expect_equal(sign.test(bp2)$CI.exact.upper, 40)
})

test_that("Example 5.4", {
  expect_equal(sign.test(c(rep(0, 14), rep(1, 9)), 0.5)$pval.exact, 0.4048729)
  expect_equal(sign.test(c(rep(0, 14), rep(1, 9)), 0.5, do.asymp = TRUE)$pval.asymp,
               0.4042485)
  expect_equal(sign.test(c(rep(0, 14), rep(1, 9)), 0.5, do.asymp = TRUE,
                         cont.corr = FALSE)$pval.asymp, 0.29714653)
})
