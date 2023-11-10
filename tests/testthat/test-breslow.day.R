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
  expect_equal(breslow.day(machine, output.status,
                           material.source)$pval.asymp.stat, 0.089150303)
  expect_equal(breslow.day(machine, output.status,
                           material.source)$pval.asymp, 0.95640372)
  expect_equal(breslow.day(machine, output.status,
                           material.source)$CI.asymp.lower, 2.02585526)
  expect_equal(breslow.day(machine, output.status,
                           material.source)$CI.asymp.upper, 10.913474)
})
