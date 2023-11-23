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

test_that("Section 13.3.1", {
  medicine <- factor(c(rep("Standard", 498 + 103 + 339 + 341),
                       rep("New", 1042 + 367 + 125 + 190)),
                     levels = c("Standard", "New"))
  response <- factor(c(rep("No effect", 498), rep("Cure", 103),
                       rep("No effect", 339), rep("Cure", 341),
                       rep("No effect", 1042), rep("Cure", 367),
                       rep("No effect", 125), rep("Cure", 190)),
                     levels = c("No effect", "Cure"))
  location <- factor(c(rep("Urban", 498 + 103), rep("Rural", 339 + 341),
                       rep("Urban", 1042 + 367), rep("Rural", 125 + 190)),
                     levels = c("Urban", "Rural"))
  expect_equal(fisher.test.ANSM(medicine[location == "Urban"],
                                response[location == "Urban"],
                                alternative = "greater")$pval.exact,
               0.0000068532897)
  expect_equal(fisher.test.ANSM(medicine[location == "Rural"],
                                response[location == "Rural"],
                                alternative = "greater")$pval.exact,
               0.00169314846)
  expect_equal(fisher.test.ANSM(medicine, response,
                                alternative = "less")$pval.exact, 0.094564075)
})

test_that("Section 13.4", {
  group <- factor(c(rep("Minority", 1 + 31 + 2 + 46),
                    rep("Majority", 10 + 58 + 9 + 43)),
                  levels = c("Minority", "Majority"))
  promoted <- factor(c(rep("Yes", 1), rep("No", 31),
                       rep("Yes", 2), rep("No", 46),
                       rep("Yes", 10), rep("No", 58),
                       rep("Yes", 9), rep("No", 43)), levels = c("Yes", "No"))
  company <- factor(c(rep("A", 32), rep("B", 48), rep("A", 68), rep("B", 52)),
                    levels = c("A", "B"))
  expect_equal(fisher.test.ANSM(group[company == "A"], promoted[company == "A"],
                                alternative = "less")$pval.exact, 0.076517329)
  expect_equal(fisher.test.ANSM(group[company == "B"], promoted[company == "B"],
                                alternative = "less")$pval.exact, 0.035089803)
  expect_equal(fisher.test.ANSM(group, company)$pval.exact, 0.030086355)
})
