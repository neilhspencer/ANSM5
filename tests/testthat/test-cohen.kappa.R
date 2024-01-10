test_that("Example 10.11", {
  expect_equal(cohen.kappa(ch10$dentistA, ch10$dentistB, do.exact = FALSE,
                           do.asymp = TRUE, alternative = "greater")$stat,
               0.41747573)
  expect_equal(cohen.kappa(ch10$dentistA, ch10$dentistB, do.exact = FALSE,
                           do.asymp = TRUE,
                           alternative = "greater")$pval.asymp.stat,
               4.5303334)
  expect_equal(cohen.kappa(ch10$dentistA, ch10$dentistB, do.exact = FALSE,
                           do.asymp = TRUE,
                           alternative = "greater")$pval.asymp, 0.0000029445342)
  expect_equal(cohen.kappa(ch10$dentistA, ch10$dentistB, seed = 1,
                           alternative = "greater")$stat, 0.41747573)
  expect_equal(cohen.kappa(ch10$dentistA, ch10$dentistB, seed = 1,
                           alternative = "greater")$pval.mc, 0)
  expect_equal(cohen.kappa(ch10$dentistA, ch10$dentistB, do.exact = FALSE,
                           do.CI = TRUE, seed = 1)$CI.mc.lower, 0.25373134)
  expect_equal(cohen.kappa(ch10$dentistA, ch10$dentistB, do.exact = FALSE,
                           do.CI = TRUE, seed = 1)$CI.mc.upper, 0.57627119)
})

test_that("Example 10.12", {
  expect_equal(cohen.kappa(ch10$questionnaire, ch10$demonstration,
                           ch10$items)$stat,
               c(0.3229219144, 0.2273352191, 0.0187919463))
  expect_equal(cohen.kappa(ch10$questionnaire, ch10$demonstration,
                           ch10$items)$pval.asymp.stat, 7.1914942)
  expect_equal(cohen.kappa(ch10$questionnaire, ch10$demonstration,
                           ch10$items)$pval.asymp, 0.027440175)
})

test_that("Exercise 10.12", {
  expect_equal(cohen.kappa(ch10$observerA, ch10$observerB)$stat, 0.39839572)
  expect_equal(cohen.kappa(ch10$observerA, ch10$observerB, do.exact = FALSE,
                           do.asymp = TRUE)$pval.asymp, 2.528931e-10)
})

