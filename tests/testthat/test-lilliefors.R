test_that("MC p-val works", {
  ages <- c(11, 13, 14, 22, 29, 30, 41, 41, 52, 55, 56, 59, 65, 65, 66, 74, 74,
            75, 77, 81, 82, 82, 82, 82, 83, 85, 85, 87, 87, 88)
  expect_equal(lilliefors(ages, alternative = "two.sided", seed = 1)$pval.mc,
               0.1361)
  McAlpha <- c(0, 0, 1, 2, 3, 9, 14, 22, 23, 29, 33, 41, 41, 42, 44, 52, 56, 57,
               58, 58, 60, 62, 63, 64, 65, 69, 72, 72, 73, 74, 74, 75, 75, 75,
               77, 77, 78, 78, 79, 79, 80, 81, 81, 81, 81, 82, 82, 83, 84, 84,
               85, 86, 87, 87, 88, 90, 92, 93, 95)
  expect_equal(lilliefors(McAlpha, alternative = "two.sided", seed = 1)$pval.mc,
               0)
})
