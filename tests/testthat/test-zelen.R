test_that("Section 13.2.5", {
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
  expect_equal(zelen(drug, side.effects, age.group)$pval.exact.stat, 0.0082126222)
  expect_equal(zelen(drug, side.effects, age.group)$pval.exact, 0.068931048)
})

test_that("Example 13.3", {
  machine <- factor(c(rep("TypeI", 42 + 2), rep("TypeII", 33 + 9),
                      rep("TypeI", 23 + 2), rep("TypeII", 18 + 7),
                      rep("TypeI", 41 + 4), rep("TypeII", 29 + 12)),
                    levels = c("TypeI", "TypeII"))
  output.status <-
    factor(c(rep("Satisfactory", 42), rep("Faulty", 2), rep("Satisfactory", 33), rep("Faulty", 9),
             rep("Satisfactory", 23), rep("Faulty", 2), rep("Satisfactory", 18), rep("Faulty", 7),
             rep("Satisfactory", 41), rep("Faulty", 4), rep("Satisfactory", 29), rep("Faulty", 12)),
           levels = c("Satisfactory", "Faulty"))
  material.source <- factor(c(rep("A", 42 + 2 + 33 + 9),
                              rep("B", 23 + 2 + 18 + 7),
                              rep("C", 41 + 4 + 29 + 12)),
                            levels = c("A", "B", "C"))
  expect_equal(zelen(machine, output.status, material.source)$pval.exact.stat,
               0.14161837)
  expect_equal(zelen(machine, output.status, material.source)$pval.exact, 1)
})
