test_that("exact p-val works", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110)$pval.exact, 0.109375)
  expect_equal(sign.test(sampleII, 110)$pval.exact, 0.041389465)
})

test_that("CI works", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110)$CI.lower, 11)
  expect_equal(sign.test(sampleI, 110)$CI.upper, 151)
  expect_equal(sign.test(sampleII, 110)$CI.lower, 40)
  expect_equal(sign.test(sampleII, 110)$CI.upper, 108)
})

test_that("actualCIwidth works", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI, 110)$actualCIwidth, 0.97851562)
  expect_equal(sign.test(sampleII, 110)$actualCIwidth, 0.95861053)
})

test_that("Example 3.12", {
  sampleI <- c(5.5, 6.0, 6.5, 7.6, 7.6, 7.7, 8.0, 8.2, 9.1, 15.1)
  sampleII <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5,
                8.7, 9.4, 14.3, 26.0)
  expect_equal(sign.test(sampleI, 9)$pval.exact, 0.109375)
  expect_equal(sign.test(sampleI, 9, cont.corr = FALSE)$pval.asymp, 0.057779571)
  expect_equal(sign.test(sampleI, 9)$pval.asymp, 0.113846298)
  expect_equal(sign.test(sampleII, 9)$pval.exact, 0.002576828)
  expect_equal(sign.test(sampleII, 9, cont.corr = FALSE)$pval.asymp, 0.0017451187)
  expect_equal(sign.test(sampleII, 9)$pval.asymp, 0.0036504344)
})
