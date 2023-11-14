test_that("Example 13.4", {
  drug <- factor(c(rep("A", 9), rep("B", 12), rep("A", 16), rep("B", 18),
                   rep("A", 28), rep("B", 44), rep("A", 42), rep("B", 28)),
                 levels = c("A", "B"))
  side.effects <- factor(c(rep("None", 8), rep("Some", 1),
                           rep("None", 11), rep("Some", 1),
                           rep("None", 14), rep("Some", 2),
                           rep("None", 18),
                           rep("None", 25), rep("Some", 3),
                           rep("None", 42), rep("Some", 2),
                           rep("None", 39), rep("Some", 3),
                           rep("None", 22), rep("Some", 6)),
                         levels = c("None", "Some"))
  age.group <- factor(c(rep("20-29", 21), rep("30-39", 34), rep("40-49", 72),
                        rep("50+", 70)), levels = c("20-29", "30-39", "40-49", "50+"))
  expect_equal(mantel.haenszel(drug, side.effects, age.group)$pval.asymp.stat,
               0.0051217907)
  expect_equal(mantel.haenszel(drug, side.effects, age.group)$pval.asymp,
               0.94294675)
})
