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
  ct.out <- chisq.test.ANSM(site, district, seed = 1)
  expect_equal(ct.out$pval.mc, 0.109388906)
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

test_that("Example 12.5", {
  drug <- factor(c(rep("Drug A", 45), rep("Drug B", 54)),
                 levels = c("Drug A", "Drug B"))
  side.effect.level <- factor(c(rep("None", 23), rep("Slight", 8),
                                rep("Moderate", 9), rep("Severe", 3),
                                rep("Fatal", 2), rep("None", 42),
                                rep("Slight", 8), rep("Moderate", 4),
                                rep("Severe", 0)),
                              levels = c("None", "Slight", "Moderate", "Severe",
                                         "Fatal"))
  expect_equal(chisq.test.ANSM(drug, side.effect.level, do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp.stat, 11.7558974)
  expect_equal(chisq.test.ANSM(drug, side.effect.level, do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp, 0.0192618608)
})

test_that("Example 12.10", {
  last.digits <- factor(c(rep(0, 7), rep(1, 11), rep(2, 17), rep(3, 19), rep(4, 9),
                          rep(5, 13), rep(6, 9), rep(7, 11), rep(8, 13), rep(9, 8)),
                        levels = c(0:9))
  expect_equal(chisq.test.ANSM(table(last.digits), do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp.stat, 11.6324786)
  expect_equal(chisq.test.ANSM(table(last.digits), do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp, 0.23483005)
  ct.out <- chisq.test.ANSM(table(last.digits), seed = 1)
  expect_equal(ct.out$pval.mc, 0.23810762)
})

test_that("Example 12.11", {
  accidents <- factor(c(rep(0, 181), rep(1, 9), rep(2, 4), rep(3, 10),
                        rep(4, 7), rep(5, 4), rep(">=6", 5)),
                      levels = c(0:5, ">=6"))
  lambda <- sum(0:6 * table(accidents)) / length(accidents)
  accidents.v2 <- factor(c(rep(0, 181), rep(1, 9), rep(2, 4), rep(">=3", 26)),
                         levels = c(0:2, ">=3"))
  poisson.probs <- c(dpois(0:2, lambda), 1 - sum(dpois(0:2, lambda)))
  expect_equal(chisq.test.ANSM(table(accidents.v2), p = poisson.probs,
                               do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp.stat, 197.73701)
  expect_equal(chisq.test.ANSM(table(accidents.v2), p = poisson.probs,
                               do.exact = FALSE,
                               do.asymp = TRUE)$pval.asymp, 0)
})
