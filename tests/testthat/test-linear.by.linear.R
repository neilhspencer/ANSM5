test_that("Example 13.7", {
  expect_equal(linear.by.linear(
    factor(ch13$dose, levels = rev(levels(ch13$dose))), ch13$dose.side.effect,
    seed = 1)$pval.mc.stat, 484)
  expect_equal(linear.by.linear(
    factor(ch13$dose, levels = rev(levels(ch13$dose))), ch13$dose.side.effect,
    seed = 1)$pval.mc, 0.01099)
  expect_equal(linear.by.linear(
    factor(ch13$dose, levels = rev(levels(ch13$dose))), ch13$dose.side.effect,
    v = c(1, 2, 20, 200), seed = 1)$pval.mc.stat, 2188)
  expect_equal(linear.by.linear(
    factor(ch13$dose, levels = rev(levels(ch13$dose))), ch13$dose.side.effect,
    v = c(1, 2, 20, 200), seed = 1)$pval.mc, 0.00715)
})

test_that("Example 13.8", {
  expect_equal(linear.by.linear(
    factor(ch13$dose, levels = rev(levels(ch13$dose))), ch13$dose.side.effect,
    do.mc = FALSE, do.asymp = TRUE)$pval.asymp.stat, 2.3888324)
  expect_equal(linear.by.linear(
    factor(ch13$dose, levels = rev(levels(ch13$dose))), ch13$dose.side.effect,
    do.mc = FALSE, do.asymp = TRUE)$pval.asymp, 0.0084510055)
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
})
