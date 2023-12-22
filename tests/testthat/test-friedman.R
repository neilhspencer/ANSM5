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
                        seed = 1)$pval.mc.stat, 13.619403)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        seed = 1)$pval.mc, 0.00482)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp.stat,
               13.619403)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.018216607)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        use.Iman.Davenport = TRUE, seed = 1)$pval.mc.stat,
               6.4035088)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        use.Iman.Davenport = TRUE, seed = 1)$pval.mc, 0.00482)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        use.Iman.Davenport = TRUE, do.exact = FALSE,
                        do.asymp = TRUE)$pval.asymp.stat, 6.4035088)
  expect_equal(friedman(ch7data$nodes, ch7data$treatment, ch7data$block,
                        use.Iman.Davenport = TRUE, do.exact = FALSE,
                        do.asymp = TRUE)$pval.asymp, 0.0022524635)
})

test_that("Exercise 7.7", {
  expect_equal(friedman(ch7data$liver.weight, ch7data$dose, ch7data$house,
                        seed = 1)$pval.mc.stat, 13)
  expect_equal(friedman(ch7data$liver.weight, ch7data$dose, ch7data$house,
                        seed = 1)$pval.mc, 0.00025)
})

test_that("Exercise 7.8", {
  expect_equal(friedman(ch7data$mark, ch7data$scheme, ch7data$candidate,
                        seed = 1)$pval.mc.stat, 7.913793)
  expect_equal(friedman(ch7data$mark, ch7data$scheme, ch7data$candidate,
                        seed = 1)$pval.mc, 0.04407)
  expect_equal(friedman(ch7data$mark, ch7data$scheme, ch7data$candidate,
                        use.Iman.Davenport = TRUE, seed = 1)$pval.mc.stat,
               3.0994475)
  expect_equal(friedman(ch7data$mark, ch7data$scheme, ch7data$candidate,
                        use.Iman.Davenport = TRUE, seed = 1)$pval.mc, 0.04407)
})

test_that("Exercise 7.9", {
  expect_equal(friedman(ch7data$prem.contractions, ch7data$drug,
                        ch7data$patient, seed = 1)$pval.mc.stat, 8.0425532)
  expect_equal(friedman(ch7data$prem.contractions, ch7data$drug,
                        ch7data$patient, seed = 1)$pval.mc, 0.01558)
  expect_equal(friedman(ch7data$prem.contractions, ch7data$drug,
                        ch7data$patient, do.exact = FALSE,
                        do.asymp = TRUE)$pval.asymp, 0.0179300609)
  expect_equal(friedman(ch7data$prem.contractions, ch7data$drug,
                        ch7data$patient, use.Iman.Davenport = TRUE,
                        seed = 1)$pval.mc.stat, 5.544)
  expect_equal(friedman(ch7data$prem.contractions, ch7data$drug,
                        ch7data$patient, use.Iman.Davenport = TRUE,
                        seed = 1)$pval.mc, 0.01558)
  expect_equal(friedman(ch7data$prem.contractions, ch7data$drug,
                        ch7data$patient, use.Iman.Davenport = TRUE,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.0112272605)
})

test_that("Exercise 7.11", {
  #(i)
  expect_equal(friedman(ch7data$births, ch7data$weekday, ch7data$week,
                        seed = 1)$pval.mc.stat, 7.690909)
  expect_equal(friedman(ch7data$births, ch7data$weekday, ch7data$week,
                        seed = 1)$pval.mc, 0.26782)
  expect_equal(friedman(ch7data$births, ch7data$weekday, ch7data$week,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.26163381)
  expect_equal(friedman(ch7data$births, ch7data$weekday, ch7data$week,
                        use.Iman.Davenport = TRUE, seed = 1)$pval.mc.stat,
               1.4147157)
  expect_equal(friedman(ch7data$births, ch7data$weekday, ch7data$week,
                        use.Iman.Davenport = TRUE, seed = 1)$pval.mc, 0.26782)
  expect_equal(friedman(ch7data$births, ch7data$weekday, ch7data$week,
                        use.Iman.Davenport = TRUE, do.exact = FALSE,
                        do.asymp = TRUE)$pval.asymp, 0.26281532)
  #(ii)
  expect_equal(friedman(ch7data$births, ch7data$week, ch7data$weekday,
                        seed = 1)$pval.mc.stat, 9.8382353)
  expect_equal(friedman(ch7data$births, ch7data$week, ch7data$weekday,
                        seed = 1)$pval.mc, 0.01432)
  expect_equal(friedman(ch7data$births, ch7data$week, ch7data$weekday,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.019992448)
  expect_equal(friedman(ch7data$births, ch7data$week, ch7data$weekday,
                        use.Iman.Davenport = TRUE, seed = 1)$pval.mc.stat,
               5.2885375)
  expect_equal(friedman(ch7data$births, ch7data$week, ch7data$weekday,
                        use.Iman.Davenport = TRUE, seed = 1)$pval.mc, 0.01432)
  expect_equal(friedman(ch7data$births, ch7data$week, ch7data$weekday,
                        use.Iman.Davenport = TRUE, do.exact = FALSE,
                        do.asymp = TRUE)$pval.asymp, 0.0086079546)
})

test_that("Exercise 7.12", {
  expect_equal(friedman(ch7data$names.recalled, ch7data$group,
                        ch7data$medical.student, seed = 1)$pval.mc.stat,
               10.888889)
  expect_equal(friedman(ch7data$names.recalled, ch7data$group,
                        ch7data$medical.student, seed = 1)$pval.mc, 0.00259)
  expect_equal(friedman(ch7data$names.recalled, ch7data$group,
                        ch7data$medical.student, do.exact = FALSE,
                        do.asymp = TRUE)$pval.asymp, 0.0043202395)
  expect_equal(friedman(ch7data$names.recalled, ch7data$group,
                        ch7data$medical.student, use.Iman.Davenport = TRUE,
                        seed = 1)$pval.mc.stat, 10.7560976)
  expect_equal(friedman(ch7data$names.recalled, ch7data$group,
                        ch7data$medical.student, use.Iman.Davenport = TRUE,
                        seed = 1)$pval.mc, 0.00259)
  expect_equal(friedman(ch7data$names.recalled, ch7data$group,
                        ch7data$medical.student, use.Iman.Davenport = TRUE,
                        do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.00084503)
})
