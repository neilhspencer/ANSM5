test_that("Example 4.14", {
  tosses1 <- c("H", "H", "H", "H", "H", "T", "T", "T", "T", "T", "T", "T", "T",
               "T", "T", "H", "H", "H", "H", "H")
  tosses2 <- c("H", "T", "H", "T", "H", "T", "H", "T", "H", "T", "H", "T", "H",
               "T", "H", "T", "H", "T", "H", "T")
  tosses3 <- c("H", "H", "T", "H", "T", "T", "T", "H", "T", "H", "H", "T", "H",
               "T", "H", "H", "H", "T", "T", "H")
  expect_equal(runs.2cat(tosses1)$pval.exact.stat, 3)
  expect_equal(runs.2cat(tosses1)$pval.exact, 0.000216501764)
  expect_equal(runs.2cat(tosses1, do.asymp = TRUE)$pval.asymp.stat, -3.4460122)
  expect_equal(runs.2cat(tosses1, do.asymp = TRUE)$pval.asymp, 0.0005689247)
  expect_equal(runs.2cat(tosses2)$pval.exact.stat, 20)
  expect_equal(runs.2cat(tosses2)$pval.exact, 0.0000216501764)
  expect_equal(runs.2cat(tosses2, do.asymp = TRUE)$pval.asymp.stat, 3.9054805)
  expect_equal(runs.2cat(tosses2, do.asymp = TRUE)$pval.asymp, 0.00009403835)
  expect_equal(runs.2cat(tosses3)$pval.exact.stat, 13)
  expect_equal(runs.2cat(tosses3)$pval.exact, 0.36184806)
  expect_equal(runs.2cat(tosses3, do.asymp = TRUE)$pval.asymp.stat, 0.74299208)
  expect_equal(runs.2cat(tosses3, do.asymp = TRUE)$pval.asymp, 0.45748648)
})

test_that("Example 5.10", {
  yr0910 <- c(4282, 5233, 4306, 3408, 4177, 2986, 3872, 3739, 2095, 3735, 3028,
              2691)
  yr1314 <- c(4443, 2847, 3291, 3699, 3071, 3801, 3815, 3527, 3939, 4104, 4307,
              3939)
  expect_equal(runs.2cat(sign(yr0910 - yr1314),
                         alternative = "less")$pval.exact, 0.65151515)
})

test_that("Example 6.17", {
  GroupA <- c(23, 18, 17, 25, 22, 19, 31, 26, 29, 33)
  GroupB <- c(21, 28, 32, 30, 41, 24, 35, 34, 27, 39, 36)
  code <- c(rep(0, length(GroupA)), rep(1, length(GroupB)))
  ranked_code <- code[order(c(GroupA, GroupB))]
  expect_equal(runs.2cat(ranked_code, alternative = "less")$pval.exact.stat,
               12)
  expect_equal(runs.2cat(ranked_code, alternative = "less")$pval.exact,
               0.68004287)
})

test_that("Example 6.18", {
  sex <- c("M", "F", "F", "F", "M", "M", "F", "F", "F", "F", "M", "M")
  expect_equal(runs.2cat(sex, alternative = "less")$pval.exact.stat, 5)
  expect_equal(runs.2cat(sex, alternative = "less")$pval.exact, 0.196969697)
})

