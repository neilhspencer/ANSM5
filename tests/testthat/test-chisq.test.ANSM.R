test_that("Example 12.1", {
  frequency <- factor(c(rep("Never", 12), rep("Sometimes", 40),
                        rep("Always", 10), rep("Never", 4),
                        rep("Sometimes", 17), rep("Always", 17)),
                      levels = c("Never", "Sometimes", "Always"))
  person <- factor(c(rep("Representative", 62), rep("Researcher", 38)),
                   levels = c("Representative", "Researcher"))
  expect_equal(chisq.test.ANSM(frequency, person, do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp.stat, 9.9061084)
  expect_equal(chisq.test.ANSM(frequency, person, do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp, 0.0070618077)
})

test_that("Example 12.3", {
  site <- factor(c("Nose", "Ears", "Nose", "Throat", "Throat", "Nose", "Throat",
                   rep("Nose", 8), rep("Ears", 7), "Throat", "Ears"),
                 levels = c("Nose", "Throat", "Ears"))
  district <- factor(c(rep("A", 2), rep("B", 2), "C", rep("D", 2),rep("E", 15),
                       rep("F", 2)))
  expect_equal(chisq.test.ANSM(site, district, do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp.stat, 14.959596)
  expect_equal(chisq.test.ANSM(site, district, do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp, 0.133541873)
  expect_equal(chisq.test.ANSM(site, district, seed = 1)$pval.mc, 0.109321891)
})

test_that("Example 12.4", {
  drug <- factor(c(rep("A", 6), rep("B", 44)), levels = c("A", "B"))
  side.effect <- factor(c("Y", rep("N", 5), rep("Y", 38), rep("N", 6)),
                        levels = c("Y", "N"))
  expect_equal(chisq.test.ANSM(drug, side.effect, do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp.stat, 11.160998)
  expect_equal(chisq.test.ANSM(drug, side.effect, do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp, 0.0008353497)
  expect_equal(chisq.test.ANSM(drug, side.effect, seed = 1)$pval.mc,
               0.0011099989)
  expect_equal(chisq.test.ANSM(drug, side.effect, cont.corr = FALSE,
                               do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp.stat, 14.9466695)
})
