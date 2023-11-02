test_that("Example 12.12", {
  seffect.new <- factor(c(rep("None", 90), rep("Slight", 44), rep("Severe", 24)),
                        levels <- c("None", "Slight", "Severe"))
  seffect.old <- factor(c(rep("None", 83), rep("Slight", 4), rep("Severe", 3),
                          rep("None", 17), rep("Slight", 22), rep("Severe", 5),
                          rep("None", 4), rep("Slight", 9), rep("Severe", 11)),
                        levels <- c("None", "Slight", "Severe"))
  expect_equal(bowker(seffect.new, seffect.old)$pval.asymp.stat, 9.3333333)
  expect_equal(bowker(seffect.new, seffect.old)$pval.asymp, 0.02517214)
})
