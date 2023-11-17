test_that("Example 13.7", {
  dose <- factor(c(rep("100mg", 51), rep("200mg", 61), rep("300mg", 42),
                   rep("400mg", 34)),
                 levels = c("100mg", "200mg", "300mg", "400mg"))
  side.effects <- factor(c(rep("None", 50), "Moderate",
                           rep("None", 60), "Slight",
                           rep("None", 40), "Slight", "Moderate",
                           rep("None", 30), "Slight", "Moderate", rep("Severe", 2)),
                         levels = c("None", "Slight", "Moderate", "Severe"))
  expect_equal(linear.by.linear(dose, side.effects, seed = 1)$pval.mc.stat, 484)
  expect_equal(linear.by.linear(dose, side.effects, seed = 1)$pval.mc, 0.01099)
  expect_equal(linear.by.linear(dose, side.effects, v = c(1, 2, 20, 200),
                                seed = 1)$pval.mc.stat, 2188)
  expect_equal(linear.by.linear(dose, side.effects, v = c(1, 2, 20, 200),
                                seed = 1)$pval.mc, 0.00715)
})

test_that("Example 13.8", {
  dose <- factor(c(rep("100mg", 51), rep("200mg", 61), rep("300mg", 42),
                   rep("400mg", 34)),
                 levels = c("100mg", "200mg", "300mg", "400mg"))
  side.effects <- factor(c(rep("None", 50), "Moderate",
                           rep("None", 60), "Slight",
                           rep("None", 40), "Slight", "Moderate",
                           rep("None", 30), "Slight", "Moderate", rep("Severe", 2)),
                         levels = c("None", "Slight", "Moderate", "Severe"))
  expect_equal(linear.by.linear(dose, side.effects, do.mc = FALSE,
                                do.asymp = TRUE)$pval.asymp.stat, 2.3888324)
  expect_equal(linear.by.linear(dose, side.effects, do.mc = FALSE,
                                do.asymp = TRUE)$pval.asymp, 0.0084510055)
})
