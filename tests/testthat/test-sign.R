test_that("exact p-val works", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI,110)$pval.exact, 0.109375)
  expect_equal(sign.test(sampleII,110)$pval.exact, 0.041389465)
})

test_that("CI works", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI,110)$CI.lower, 11)
  expect_equal(sign.test(sampleI,110)$CI.upper, 151)
  expect_equal(sign.test(sampleII,110)$CI.lower, 40)
  expect_equal(sign.test(sampleII,110)$CI.upper, 108)
})

test_that("actualCIwidth works", {
  sampleI <- c(1, 11, 35, 41, 50, 62, 104, 104, 151, 161)
  sampleII <- c(2, 10, 27, 35, 38, 40, 47, 50, 58, 60, 68, 70, 79, 103, 108, 151, 157, 158, 162, 166)
  expect_equal(sign.test(sampleI,110)$actualCIwidth, 0.97851562)
  expect_equal(sign.test(sampleII,110)$actualCIwidth, 0.95861053)
})
