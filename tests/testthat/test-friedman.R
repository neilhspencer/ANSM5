test_that("Example 7.6", {
  expect_equal(friedman(ch7data$pulse, ch7data$time.period,
                        ch7data$student)$pval.exact.stat, 10.5714286)
  expect_equal(friedman(ch7data$pulse, ch7data$time.period,
                        ch7data$student)$pval.exact, 0.00272205075)
  expect_equal(friedman(ch7data$pulse, ch7data$time.period, ch7data$student,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp.stat,
               10.5714286)
  expect_equal(friedman(ch7data$pulse, ch7data$time.period, ch7data$student,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.0050634142)
})

test_that("Example 7.7", {
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        seed = 1)$pval.mc.stat, 6.4035088)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        seed = 1)$pval.mc, 0.00482)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp.stat,
               6.4035088)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.0022524635)
})

test_that("Exercise 7.7", {
  expect_equal(friedman(ch7data$liver.weight, ch7data$dose, ch7data$house,
                        seed = 1)$pval.mc.stat, 13)
  expect_equal(friedman(ch7data$liver.weight, ch7data$dose, ch7data$house,
                        seed = 1)$pval.mc, 0.00025)
})

test_that("Exercise 7.8", {
  expect_equal(friedman(ch7data$mark, ch7data$scheme, ch7data$candidate,
                        seed = 1)$pval.mc.stat, 3.0994475)
  expect_equal(friedman(ch7data$liver.weight, ch7data$dose, ch7data$house,
                        seed = 1)$pval.mc, 0.04407)
})
