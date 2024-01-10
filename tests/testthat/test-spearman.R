test_that("Example 10.2", {
  expect_equal(spearman(ch10$q1, ch10$q2, alternative = "greater",
                        do.asymp = TRUE, do.exact = FALSE)$stat, 0.43356643)
  expect_equal(spearman(ch10$q1, ch10$q2, alternative = "greater",
                        do.asymp = TRUE, do.exact = FALSE)$pval.asymp,
               0.079552885)
  expect_equal(spearman(ch10$q1, ch10$q2, alternative = "greater",
                        seed = 1)$stat, 0.43356643)
  expect_equal(spearman(ch10$q1, ch10$q2, alternative = "greater",
                        seed = 1)$pval.mc, 0.08155)
})

test_that("Exercise 10.1", {
  expect_equal(spearman(ch10$ERA, ch10$ESMS, do.exact = FALSE)$stat, 0.9030303)
})

test_that("Example 10.4", {
  expect_equal(spearman(ch10$death.year, ch10$age.at.death,
                        alternative = "greater", seed = 1)$stat, 0.50688898)
  expect_equal(spearman(ch10$death.year, ch10$age.at.death,
                        alternative = "greater", seed = 1)$pval.mc, 0.0399)
})

test_that("Example 10.8", {
  expect_equal(spearman(ch10$British, ch10$American)$stat, 0.82142857)
  expect_equal(spearman(ch10$British, ch10$American)$pval.exact, 0.034126984)
})

test_that("Example 10.9", {
  expect_equal(spearman(ch10$Canadian, ch10$Australian)$stat, 0)
  expect_equal(spearman(ch10$Canadian, ch10$Australian)$pval.exact, 1)
})

