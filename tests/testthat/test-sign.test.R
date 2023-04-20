test_that("Example 3.1", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110)$pval.exact, 0.109375)
  expect_equal(sign.test(sampleII, 110)$pval.exact, 0.041389465)
})

test_that("Example 3.2", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI)$actualCIwidth.exact, 0.97851562)
  expect_equal(sign.test(sampleI)$CI.exact.lower, 11)
  expect_equal(sign.test(sampleI)$CI.exact.upper, 151)
  expect_equal(sign.test(sampleII)$actualCIwidth.exact, 0.95861053)
  expect_equal(sign.test(sampleII)$CI.exact.lower, 40)
  expect_equal(sign.test(sampleII)$CI.exact.upper, 108)
})

test_that("Example 3.3", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110, do.asymp = TRUE)$pval.asymp, 0.113846298)
  expect_equal(sign.test(sampleI, 110, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.057779571)
  expect_equal(sign.test(sampleII, 110, do.asymp = TRUE)$pval.asymp, 0.044171345)
  expect_equal(sign.test(sampleII, 110, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.025347319)
})

test_that("Example 3.12", {
  sampleI <- c(5.5, 6.0, 6.5, 7.6, 7.6, 7.7, 8.0, 8.2, 9.1, 15.1)
  sampleII <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5,
                8.7, 9.4, 14.3, 26.0)
  expect_equal(sign.test(sampleI, 9)$pval.exact, 0.109375)
  expect_equal(sign.test(sampleI, 9, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.057779571)
  expect_equal(sign.test(sampleI, 9, do.asymp = TRUE)$pval.asymp, 0.113846298)
  expect_equal(sign.test(sampleII, 9)$pval.exact, 0.002576828)
  expect_equal(sign.test(sampleII, 9, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.0017451187)
  expect_equal(sign.test(sampleII, 9, do.asymp = TRUE)$pval.asymp, 0.0036504344)
})

test_that("Example 3.13", {
  sampleII <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5,
                8.7, 9.4, 14.3, 26.0)
  expect_equal(sign.test(sampleII, do.asymp = TRUE)$CI.asymp.lower, 7)
  expect_equal(sign.test(sampleII, do.asymp = TRUE)$CI.asymp.upper, 8.2)
  expect_equal(sign.test(sampleII, 6.95, "greater")$pval.exact, 0.05765915)
  expect_equal(sign.test(sampleII, 8.25, "less")$pval.exact, 0.05765915)
  expect_equal(sign.test(sampleII, 7.05, "greater")$pval.exact, 0.131587982)
  expect_equal(sign.test(sampleII, 8.15, "less")$pval.exact, 0.131587982)
})

test_that("Example 3.14", {
  heartrates2a <- c(-2, 4, 8, 35 ,-5 ,16 ,3 ,1 ,12 ,17 ,20 ,9)
  heartrates2b <- c(-2, 4, 8, 65 ,-5 ,16 ,3 ,1 ,12 ,17 ,20 ,9)
  expect_equal(sign.test(heartrates2a)$CI.exact.lower, 1)
  expect_equal(sign.test(heartrates2a)$CI.exact.upper, 17)
  expect_equal(sign.test(heartrates2a, CI.width = 0.99)$CI.exact.lower, -2)
  expect_equal(sign.test(heartrates2a, CI.width = 0.99)$CI.exact.upper, 20)
  expect_equal(sign.test(heartrates2b)$CI.exact.lower, 1)
  expect_equal(sign.test(heartrates2b)$CI.exact.upper, 17)
  expect_equal(sign.test(heartrates2b, CI.width = 0.99)$CI.exact.lower, -2)
  expect_equal(sign.test(heartrates2b, CI.width = 0.99)$CI.exact.upper, 20)
})

test_that("Exercise 3.11", {
  fishlengths <- c(64, rep(65, 2), 66, 67, rep(68, 4), rep(69, 3), rep(70, 4),
                   rep(71, 5), rep(72, 3), rep(73, 3), 75, rep(77, 6), 78, 83)
  expect_equal(sign.test(fishlengths)$actualCIwidth, 0.97118328)
  expect_equal(sign.test(fishlengths)$CI.exact.lower, 69)
  expect_equal(sign.test(fishlengths)$CI.exact.upper, 73)
  expect_equal(sign.test(fishlengths, CI.width = 0.90)$actualCIwidth, 0.93475466)
  expect_equal(sign.test(fishlengths, CI.width = 0.90)$CI.exact.lower, 70)
  expect_equal(sign.test(fishlengths, CI.width = 0.90)$CI.exact.upper, 72)
})

test_that("Exercise 3.15", {
  assembly <- c(14.2, 11.3, 12.7, 19.2, 13.5, 14.4, 11.8, 15.1, 12.3, 11.7,
                13.2, 13.4, 14.0, 14.1, 13.7, 11.9, 11.8, 10.7, 11.3, 12.2)
  expect_equal(sign.test(assembly)$CI.exact.lower, 11.8)
  expect_equal(sign.test(assembly)$CI.exact.upper, 14)
})

test_that("Exercise 3.16", {
  weightchange <- c(-1.2, 1.4, 0.2, -0.7, -6.4, -2.7, -8.6, -1.7, -2.2,
                    0.1, -0.4, -4.2, -1.6, 1.2, -1.3, -2.4, 3.1, -0.2,
                    -4.5, -6.3, -1.7, 0.0, 0.2, -3.7, 1.1, -2.3, -0.1,
                    -7.3, 0.2, -1.4, -0.9, -2.0, 0.0, 1.1, -0.3, -1.1)
  expect_equal(sign.test(weightchange)$CI.exact.lower, -2)
  expect_equal(sign.test(weightchange)$CI.exact.upper, -0.1)
})

test_that("Exercise 3.17", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110)$pval.exact, 0.109375)
  expect_equal(sign.test(sampleI, 110, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.057779571)
  expect_equal(sign.test(sampleI, 110, do.asymp = TRUE)$pval.asymp, 0.113846298)
  expect_equal(sign.test(sampleII, 110)$pval.exact, 0.041389465)
  expect_equal(sign.test(sampleII, 110, do.asymp = TRUE, cont.corr = FALSE)$pval.asymp, 0.025347319)
  expect_equal(sign.test(sampleII, 110, do.asymp = TRUE)$pval.asymp, 0.044171345)
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
