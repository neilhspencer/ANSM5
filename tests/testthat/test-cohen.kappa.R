test_that("Example 10.11", {
  dentistA <- as.factor(c(rep("needed", 45), rep("not needed", 55)))
  dentistB <- as.factor(c(rep("needed", 40), rep("not needed", 5),
                          rep("needed", 25), rep("not needed", 30)))
  expect_equal(cohen.kappa(dentistA, dentistB, do.exact = FALSE,
                           do.asymp = TRUE, alternative = "greater")$stat,
               0.41747573)
  expect_equal(cohen.kappa(dentistA, dentistB, do.exact = FALSE,
                           do.asymp = TRUE,
                           alternative = "greater")$pval.asymp.stat,
               4.5303334)
  expect_equal(cohen.kappa(dentistA, dentistB, do.exact = FALSE,
                           do.asymp = TRUE,
                           alternative = "greater")$pval.asymp, 0.0000029445342)
  expect_equal(cohen.kappa(dentistA, dentistB, seed = 1,
                           alternative = "greater")$stat, 0.41747573)
  expect_equal(cohen.kappa(dentistA, dentistB, seed = 1,
                           alternative = "greater")$pval.mc, 0)
  expect_equal(cohen.kappa(dentistA, dentistB, do.exact = FALSE, do.CI = TRUE,
                           seed = 1)$CI.mc.lower, 0.25373134)
  expect_equal(cohen.kappa(dentistA, dentistB, do.exact = FALSE, do.CI = TRUE,
                           seed = 1)$CI.mc.upper, 0.57627119)
})

test_that("Example 10.12", {
  questionnaire <- as.factor(c(rep("symptoms", 37), rep("no symptoms", 59),
                               rep("symptoms", 149), rep("no symptoms", 236),
                               rep("symptoms", 39), rep("no symptoms", 80)))
  demonstration <- as.factor(c(rep("symptoms", 14), rep("no symptoms", 23),
                               rep("symptoms", 5), rep("no symptoms", 54),
                               rep("symptoms", 45), rep("no symptoms", 104),
                               rep("symptoms", 23), rep("no symptoms", 213),
                               rep("symptoms", 5), rep("no symptoms", 34),
                               rep("symptoms", 9), rep("no symptoms", 71)))
  items <- as.factor(c(rep("0-1 items", 96), rep("2-3 items", 385),
                       rep("4-5 items", 119)))
  expect_equal(cohen.kappa(questionnaire, demonstration, items)$stat,
               c(0.3229219144, 0.2273352191, 0.0187919463))
  expect_equal(cohen.kappa(questionnaire, demonstration, items)$pval.asymp.stat,
               7.1914942)
  expect_equal(cohen.kappa(questionnaire, demonstration, items)$pval.asymp,
               0.027440175)
})
