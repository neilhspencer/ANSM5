test_that("Example 12.2", {
  site <- factor(c("Nose", "Ears", "Nose", "Throat", "Throat", "Nose", "Throat",
                   rep("Nose", 8), rep("Ears", 7), "Throat", "Ears"),
                 levels = c("Nose", "Throat", "Ears"))
  district <- factor(c(rep("A", 2), rep("B", 2), "C", rep("D", 2),rep("E", 15),
                       rep("F", 2)))
  expect_equal(lik.ratio(site, district, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp.stat, 17.3344364)
  expect_equal(lik.ratio(site, district, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp, 0.06728363)
  expect_equal(lik.ratio(site, district, seed = 1)$pval.mc, 0.04813)
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
  expect_equal(lik.ratio(physical.activity[gender == "Boy"],
                         tv.viewing[gender == "Boy"], seed = 1)$pval.mc, 0.0447)
  expect_equal(lik.ratio(physical.activity[gender == "Girl"],
                         tv.viewing[gender == "Girl"], seed = 1)$pval.mc,
               0.00607)
})

test_that("Example 13.12", {
  drug <- factor(c(rep("A", 9 + 4 + 16 + 31), rep("B", 3 + 16 + 2 + 19)),
                 levels = c("A", "B"))
  side.effect <- factor(c(rep("Hair loss", 9), rep("Visual impairment", 4),
                          rep("Hair loss & Visual impairment", 16),
                          rep("None", 31),
                          rep("Hair loss", 3), rep("Visual impairment", 16),
                          rep("Hair loss & Visual impairment", 2),
                          rep("None", 19)),
                        levels = c("Hair loss", "Visual impairment",
                                   "Hair loss & Visual impairment", "None"))
  side.effect.2 <- side.effect
  levels(side.effect.2) <- list("Hair loss or Visual impairment" = c("Hair loss", "Visual impairment"),
                                "Hair loss & Visual impairment" = "Hair loss & Visual impairment",
                                "None" = "None")
  side.effect.3 <- side.effect
  levels(side.effect.3) <-
    list("Side-effect" = c("Hair loss", "Visual impairment",
                           "Hair loss & Visual impairment"), "None" = "None")
  expect_equal(lik.ratio(drug, side.effect, seed = 1)$pval.mc, 0.00010)
  expect_equal(lik.ratio(drug, side.effect, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp, 0.000061414559)
  expect_equal(lik.ratio(drug[side.effect == "Hair loss" |
                                side.effect == "Visual impairment"],
                         side.effect[side.effect == "Hair loss" |
                                       side.effect == "Visual impairment"],
                         seed = 1)$pval.mc, 0.00337)
  expect_equal(lik.ratio(drug[side.effect == "Hair loss" |
                                side.effect == "Visual impairment"],
                         side.effect[side.effect == "Hair loss" |
                                       side.effect == "Visual impairment"],
                         do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.00182512405)
  expect_equal(lik.ratio(drug[side.effect.2 == "Hair loss or Visual impairment" |
                                side.effect.2 == "Hair loss & Visual impairment"],
                         side.effect.2[side.effect.2 == "Hair loss or Visual impairment" |
                                         side.effect.2 == "Hair loss & Visual impairment"],
                         seed = 1)$pval.mc, 0.00086)
  expect_equal(lik.ratio(drug[side.effect.2 == "Hair loss or Visual impairment" |
                                side.effect.2 == "Hair loss & Visual impairment"],
                         side.effect.2[side.effect.2 == "Hair loss or Visual impairment" |
                                side.effect.2 == "Hair loss & Visual impairment"],
                         do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.00046737958)
  expect_equal(lik.ratio(drug, side.effect.3, seed = 1)$pval.mc, 0.83606)
  expect_equal(lik.ratio(drug, side.effect.3, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp, 0.6830428)
})
