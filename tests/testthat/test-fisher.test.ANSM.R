test_that("Example 6.7", {
  Males <- c(15, 16, 20, 24, 27, 31, 34)
  Females <- c(8, 9, 11, 11, 14, 15, 16, 16, 16, 17, 18, 18, 19, 21, 23, 23, 25,
               25, 25, 27, 32)
  expect_equal(fisher.test.ANSM(Males, Females)$pval.exact, 0.38454106)
})

test_that("Example 12.2", {
  site <- factor(c("Nose", "Ears", "Nose", "Throat", "Throat", "Nose", "Throat",
                   rep("Nose", 8), rep("Ears", 7), "Throat", "Ears"),
                 levels = c("Nose", "Throat", "Ears"))
  district <- factor(c(rep("A", 2), rep("B", 2), "C", rep("D", 2),rep("E", 15),
                       rep("F", 2)))
  expect_equal(fisher.test.ANSM(site, district)$pval.exact, 0.026133508)
})
