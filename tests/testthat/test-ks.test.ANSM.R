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
  Females <- c(0.45, 0.60, 0.80, 0.85, 0.95, 1.00, 1.75)
  Males <- c(0.40, 0.50, 0.55, 0.65, 0.70, 0.75, 0.90, 1.05, 1.15, 1.25, 1.30,
             1.35, 1.45, 1.50, 1.85, 1.90, 2.30, 2.55, 2.70, 2.85, 3.85)
  expect_equal(ks.test.ANSM(Females, Males,
                            alternative = "greater")$pval.exact.stat,
               0.52380952)
  expect_equal(ks.test.ANSM(Females, Males, alternative = "greater")$pval.exact,
               0.046479004)
  expect_equal(ks.test.ANSM(Females, Males,
                            alternative = "two.sided")$pval.exact, 0.092958008)
})
