test_that("Example 3.1", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110)$pval.exact, 0.109375)
  expect_equal(sign.test(sampleII, 110)$pval.exact, 0.041389465)
})

test_that("CI works", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110)$CI.exact.lower, 11)
  expect_equal(sign.test(sampleI, 110)$CI.exact.upper, 151)
  expect_equal(sign.test(sampleII, 110)$CI.exact.lower, 40)
  expect_equal(sign.test(sampleII, 110)$CI.exact.upper, 108)
})

test_that("actualCIwidth works", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110)$actualCIwidth.exact, 0.97851562)
  expect_equal(sign.test(sampleII, 110)$actualCIwidth.exact, 0.95861053)
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

