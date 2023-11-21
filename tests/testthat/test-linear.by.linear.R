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

test_that("Example 13.9", {
  alcohol <- factor(c(rep("0", 17066 + 48), rep("< 1", 14466 + 38),
                      rep("1 - 2", 788 + 5), rep("3 - 5", 126 + 1),
                      rep(">= 6", 37 + 1)), levels = c("0", "< 1", "1 - 2",
                                                      "3 - 5", ">= 6"))
  malformation <- factor(c(rep("Absent", 17066), rep("Present", 48),
                           rep("Absent", 14466), rep("Present", 38),
                           rep("Absent", 788), rep("Present", 5),
                           rep("Absent", 126), rep("Present", 1),
                           rep("Absent", 37), rep("Present", 1)),
                           levels = c("Absent", "Present"))
  expect_equal(linear.by.linear(alcohol, malformation, nsims.mc = 10000,
                                seed = 1)$pval.mc.stat, 49347)
  expect_equal(linear.by.linear(alcohol, malformation, nsims.mc = 10000,
                                seed = 1)$pval.mc, 0.1025)
  expect_equal(linear.by.linear(alcohol, malformation,
                                u = c(8557.5, 24365.5, 32013, 32473, 32555.5),
                                nsims.mc = 10000, seed = 1)$pval.mc.stat,
               532159498.5)
  expect_equal(linear.by.linear(alcohol, malformation,
                                u = c(8557.5, 24365.5, 32013, 32473, 32555.5),
                                nsims.mc = 10000, seed = 1)$pval.mc, 0.2746)
  expect_equal(linear.by.linear(alcohol, malformation, u = c(0, 0.5, 1.5, 4, 7),
                                nsims.mc = 10000, seed = 1)$pval.mc.stat, 9253)
  expect_equal(linear.by.linear(alcohol, malformation, u = c(0, 0.5, 1.5, 4, 7),
                                nsims.mc = 10000, seed = 1)$pval.mc, 0.0170)
})

test_that("Example 13.10", {
  frequency <- factor(c(rep("Never", 12 + 4), rep("Sometimes", 40 + 17),
                        rep("Always", 10 + 17)),
                      levels = c("Never", "Sometimes", "Always"))
  person <- factor(c(rep("Representative", 12), rep("Researcher", 4),
                     rep("Representative", 40), rep("Researcher", 17),
                     rep("Representative", 10), rep("Researcher", 17)),
                   levels = c("Representative", "Researcher"))
  expect_equal(linear.by.linear(frequency, person, seed = 1)$pval.mc.stat, 300)
  expect_equal(linear.by.linear(frequency, person, seed = 1)$pval.mc * 2,
               0.00682)
})
