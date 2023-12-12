test_that("Example 4.14", {
  expect_equal(runs.2cat(ch4data$tosses1)$pval.exact.stat, 3)
  expect_equal(runs.2cat(ch4data$tosses1)$pval.exact, 0.000216501764)
  expect_equal(runs.2cat(ch4data$tosses1, do.asymp = TRUE)$pval.asymp.stat,
               -3.4460122)
  expect_equal(runs.2cat(ch4data$tosses1, do.asymp = TRUE)$pval.asymp,
               0.0005689247)
  expect_equal(runs.2cat(ch4data$tosses2)$pval.exact.stat, 20)
  expect_equal(runs.2cat(ch4data$tosses2)$pval.exact, 0.0000216501764)
  expect_equal(runs.2cat(ch4data$tosses2, do.asymp = TRUE)$pval.asymp.stat,
               3.9054805)
  expect_equal(runs.2cat(ch4data$tosses2, do.asymp = TRUE)$pval.asymp,
               0.00009403835)
  expect_equal(runs.2cat(ch4data$tosses3)$pval.exact.stat, 13)
  expect_equal(runs.2cat(ch4data$tosses3)$pval.exact, 0.36184806)
  expect_equal(runs.2cat(ch4data$tosses3, do.asymp = TRUE)$pval.asymp.stat,
               0.74299208)
  expect_equal(runs.2cat(ch4data$tosses3, do.asymp = TRUE)$pval.asymp,
               0.45748648)
})

test_that("Exercise 4.10", {
  expect_equal(runs.2cat(
    sign(ch4data$points - median(ch4data$points)))$pval.exact, 0.063403263)
})

test_that("Exercise 4.11", {
  expect_equal(runs.2cat(
    sign(ch4data$rainfall.DRC - median(ch4data$rainfall.DRC)))$pval.exact,
    0.063403263)
})

test_that("Exercise 4.12", {
  expect_equal(runs.2cat(
    sign(ch4data$piped.water.DRC - median(ch4data$piped.water.DRC)))$pval.exact,
    0.42890443)
})

test_that("Example 5.10", {
  expect_equal(runs.2cat(sign(ch5data$yr0910 - ch5data$yr1314),
                         alternative = "less")$pval.exact, 0.65151515)
})

test_that("Example 6.17", {
  code <- c(rep(0, length(ch6data$groupA)), rep(1, length(ch6data$groupB)))
  ranked_code <- code[order(c(ch6data$groupA, ch6data$groupB))]
  expect_equal(runs.2cat(ranked_code, alternative = "less")$pval.exact.stat,
               12)
  expect_equal(runs.2cat(ranked_code, alternative = "less")$pval.exact,
               0.68004287)
})

test_that("Example 6.18", {
  expect_equal(runs.2cat(ch6data$sex, alternative = "less")$pval.exact.stat, 5)
  expect_equal(runs.2cat(ch6data$sex, alternative = "less")$pval.exact,
               0.196969697)
})

