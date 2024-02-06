test_that("Example 13.7", {
  expect_equal(linear.by.linear(ch13$dose, ch13$dose.side.effect,
                                seed = 1)$pval.mc.stat, 484)
  expect_equal(linear.by.linear(ch13$dose, ch13$dose.side.effect,
                                seed = 1)$pval.mc, 0.01099)
  expect_equal(linear.by.linear(ch13$dose, ch13$dose.side.effect,
                                v = c(1, 2, 20, 200),
                                seed = 1)$pval.mc.stat, 2188)
  expect_equal(linear.by.linear(ch13$dose, ch13$dose.side.effect,
                                v = c(1, 2, 20, 200),
                                seed = 1)$pval.mc, 0.00715)
})

test_that("Example 13.8", {
  expect_equal(linear.by.linear(
    ch13$dose, ch13$dose.side.effect, do.mc = FALSE,
    do.asymp = TRUE)$pval.asymp.stat, 2.3888324)
  expect_equal(linear.by.linear(
    ch13$dose, ch13$dose.side.effect, do.mc = FALSE,
    do.asymp = TRUE)$pval.asymp, 0.0084510055)
})

test_that("Example 13.9", {
  expect_equal(linear.by.linear(ch13$alcohol, ch13$malformation,
                                nsims.mc = 10000, seed = 1)$pval.mc.stat, 49347)
  expect_equal(linear.by.linear(ch13$alcohol, ch13$malformation,
                                nsims.mc = 10000, seed = 1)$pval.mc, 0.1025)
  expect_equal(linear.by.linear(ch13$alcohol, ch13$malformation,
                                u = c(8557.5, 24365.5, 32013, 32473, 32555.5),
                                nsims.mc = 10000, seed = 1)$pval.mc.stat,
               532159498.5)
  expect_equal(linear.by.linear(ch13$alcohol, ch13$malformation,
                                u = c(8557.5, 24365.5, 32013, 32473, 32555.5),
                                nsims.mc = 10000, seed = 1)$pval.mc, 0.2746)
  expect_equal(linear.by.linear(ch13$alcohol, ch13$malformation,
                                u = c(0, 0.5, 1.5, 4, 7), nsims.mc = 10000,
                                seed = 1)$pval.mc.stat, 9253)
  expect_equal(linear.by.linear(ch13$alcohol, ch13$malformation,
                                u = c(0, 0.5, 1.5, 4, 7), nsims.mc = 10000,
                                seed = 1)$pval.mc, 0.0170)
})

test_that("Example 13.10", {
  expect_equal(linear.by.linear(ch13$frequency, ch13$person,
                                seed = 1)$pval.mc.stat, 300)
  expect_equal(linear.by.linear(ch13$frequency, ch13$person,
                                seed = 1)$pval.mc * 2, 0.00682)
  expect_equal(linear.by.linear(ch13$frequency, ch13$person, do.mc = FALSE,
                                do.asymp = TRUE)$pval.asymp * 2, 0.0051610501)
})

test_that("Exercise 13.4", {
  expect_equal(linear.by.linear(ch13$SBP, ch13$cholesterol,
                                seed = 1)$pval.mc * 2, 0.63174)
  expect_equal(linear.by.linear(ch13$SBP, ch13$cholesterol,
                                u = c(1, 2, 3, 6, 10), v = c(1, 2, 6),
                                seed = 1)$pval.mc * 2, 0.38774)
  expect_equal(linear.by.linear(ch13$SBP, ch13$cholesterol, do.mc = FALSE,
                                do.asymp = TRUE)$pval.asymp * 2, 0.58217184)
  expect_equal(linear.by.linear(ch13$SBP, ch13$cholesterol,
                   u = c(1, 2, 3, 6, 10), v = c(1, 2, 6), do.mc = FALSE,
                   do.asymp = TRUE)$pval.asymp * 2, 0.381711194)
})

test_that("Exercise 13.5", {
  expect_equal(linear.by.linear(ch13$schooling, ch13$abortion.attitude,
                                do.mc = FALSE, do.asymp = TRUE)$pval.asymp * 2,
               1.339675e-21)
})

test_that("Exercise 13.9", {
  expect_equal(linear.by.linear(ch13$PPI.ages, ch13$PPI.people,
                                do.mc = FALSE, do.asymp = TRUE)$pval.asymp * 2,
               8.1210037e-08)
})

test_that("Exercise 13.11", {
  expect_equal(linear.by.linear(ch13$laid.off, ch13$employee.ages.2,
                                seed = 1)$pval.mc, 0)
})
