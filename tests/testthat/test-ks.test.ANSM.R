test_that("Example 4.2", {
  expect_equal(suppressWarnings(
    ks.test.ANSM(ch4data$breaks, "punif", min = 0, max = 6,
                 alternative = "two.sided")$pval.exact.stat), 0.183333333)
  expect_equal(suppressWarnings(
    ks.test.ANSM(ch4data$breaks, "punif", min = 0, max = 6,
                 alternative = "two.sided")$pval.exact), 0.45829257)
  expect_equal(suppressWarnings(
    ks.test.ANSM(ch4data$breaks, "punif", min = 0, max = 6,
                 alternative = "two.sided", do.asymp = TRUE)$pval.asymp.stat),
    0.183333333)
  expect_equal(suppressWarnings(
    ks.test.ANSM(ch4data$breaks, "punif", min = 0, max = 6,
                 alternative = "two.sided", do.asymp = TRUE)$pval.asymp),
    0.51214416)
})

test_that("Exercise 4.2", {
  expect_equal(suppressWarnings(
    ks.test.ANSM(ch4data$waiting.time, "pexp", rate = 1 / 20)$pval.exact),
    0.16521394)
})

test_that("Exercise 4.3", {
  expect_equal(ks.test.ANSM(ch4data$visiting.supporters, "pexp",
                            rate = 2600)$pval.exact, 0)
  expect_equal(ks.test.ANSM(ch4data$visiting.supporters, "punif", min = 1100,
                            max = 7700)$pval.exact, 0.000227738766)
})

test_that("Example 6.15", {
  expect_equal(ks.test.ANSM(ch6data$salivaF, ch6data$salivaM,
                            alternative = "greater")$pval.exact.stat,
               0.52380952)
  expect_equal(ks.test.ANSM(ch6data$salivaF, ch6data$salivaM,
                            alternative = "greater")$pval.exact, 0.046479004)
  expect_equal(ks.test.ANSM(ch6data$salivaF, ch6data$salivaM,
                            alternative = "two.sided")$pval.exact, 0.092958008)
})

test_that("Exercise 6.13", {
  expect_equal(ks.test.ANSM(ch6data$time.withoutLD,
                            ch6data$time.withLD)$pval.exact, 0.055944056)
  expect_equal(ks.test.ANSM(ch6data$time.withoutLD, ch6data$time.withLD,
                            alternative = "greater")$pval.exact, 0.027972028)
})

test_that("Exercise 6.15", {
  expect_equal(ks.test.ANSM(ch6data$doseI, ch6data$doseII)$pval.exact,
               0.033566434)
})

test_that("Exercise 6.16", {
  expect_equal(ks.test.ANSM(ch6data$travel, ch6data$politics)$pval.exact,
               0.0104786854)
})
