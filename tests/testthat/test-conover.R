test_that("Example 6.13", {
  expect_equal(conover(ch6data$typeA, ch6data$typeB)$pval.exact.stat, 720)
  expect_equal(conover(ch6data$typeA, ch6data$typeB)$pval.exact, 0.126728586)
  expect_equal(conover(ch6data$typeA, ch6data$typeB,
                       do.asymp = TRUE)$pval.asymp.stat, 720)
  expect_equal(conover(ch6data$typeA, ch6data$typeB,
                       do.asymp = TRUE)$pval.asymp, 0.110647884)
})

test_that("Exercise 6.12", {
  expect_equal(conover(ch6data$typeA, ch6data$typeB,
                       abs.ranks = TRUE)$pval.exact, 0.13493689)
  expect_equal(conover(ch6data$typeA, ch6data$typeB, abs.ranks = TRUE,
                       do.asymp = TRUE)$pval.asymp, 0.10703901)
})

test_that("Exercise 6.14", {
  expect_equal(conover(ch6data$time.withoutLD,
                       ch6data$time.withLD)$pval.exact, 0.55602176)
})

test_that("Exercise 6.16", {
  expect_equal(conover(ch6data$travel, ch6data$politics)$pval.exact,
               0.0104786854)
})

test_that("Example 7.9", {
  expect_equal(conover(ch7data$dementia.age, ch7data$features)$pval.exact.stat,
               1.85924)
  expect_equal(conover(ch7data$dementia.age, ch7data$features)$pval.exact,
               0.66714238)
  expect_equal(conover(ch7data$dementia.age, ch7data$features, do.exact = FALSE,
                       do.asymp = TRUE)$pval.asymp.stat, 1.85924)
  expect_equal(conover(ch7data$dementia.age, ch7data$features, do.exact = FALSE,
                       do.asymp = TRUE)$pval.asymp, 0.39470367)
})

test_that("Exercise 7.15", {
  expect_equal(conover(ch7data$braking.distance.2, ch7data$initial.speed, do.exact = FALSE,
                       do.asymp = TRUE)$pval.asymp, 0.0455638507)
})
