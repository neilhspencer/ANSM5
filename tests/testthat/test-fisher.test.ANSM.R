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
  expect_equal(fisher.test.ANSM(drug, side.effect.level)$pval.exact,
               0.0127213014)
})

test_that("Example 13.1", {
  physical.activity <- factor(c(rep("Daily", 167 + 320 + 123 + 157),
                                rep("Less than daily", 388 + 937 + 604 + 1114)),
                              levels = c("Daily", "Less than daily"))
  tv.viewing <- factor(c(rep("2 hours or less", 167), rep("More than 2 hours", 320),
                         rep("2 hours or less", 123), rep("More than 2 hours", 157),
                         rep("2 hours or less", 388), rep("More than 2 hours", 937),
                         rep("2 hours or less", 604), rep("More than 2 hours", 1114)),
                         levels = c("2 hours or less", "More than 2 hours"))
  gender <- factor(c(rep("Boy", 167 + 320), rep("Girl", 123 + 157),
                     rep("Boy", 388 + 937), rep("Girl", 604 + 1114)),
                   levels = c("Boy", "Girl"))
  expect_equal(fisher.test.ANSM(physical.activity[gender == "Boy"],
                                tv.viewing[gender == "Boy"])$pval.exact,
               0.044196753)
  expect_equal(fisher.test.ANSM(physical.activity[gender == "Girl"],
                                tv.viewing[gender == "Girl"])$pval.exact,
               0.0059470492)
})

