test_that("Example 8.11", {
  prey_preference <- c(1, 2, 1, 1, 3, 1,
                       7, 7, 7, 6.5, 6.5, 7,
                       5.5, 5.5, 5, 5, 5, 2.5,
                       3, 4, 4, 6.5, 1.5, 4,
                       2, 1, 6, 4, 6.5, 6,
                       4, 3, 2, 2, 1.5, 2.5,
                       5.5, 5.5, 3, 3, 4, 5)
  prey <- factor(c(rep("Anopheles", 6), rep("Cyclops", 6), rep("Ostracod", 6),
                   rep("Simocephalus_(wild)", 6),
                   rep("Simocephalus_(domestic)", 6), rep("Daphnia_magna", 6),
                   rep("Daphnia_longspina", 6)))
  larva <- factor(rep(c("Larva1", "Larva2", "Larva3", "Larva4", "Larva5",
                        "Larva6"), 7))
  expect_equal(friedman.lsd(prey_preference, prey, larva,
                            c("Cyclops", "Anopheles"))$pval.asymp.stat,
               6.4649763)
  expect_equal(friedman.lsd(prey_preference, prey, larva,
                            c("Cyclops", "Anopheles"))$pval.asymp,
               1.9153983e-07)
})
