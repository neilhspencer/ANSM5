test_that("Example 12.2", {
  expect_equal(lik.ratio(ch12$infection.site, ch12$district, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp.stat, 17.3344364)
  expect_equal(lik.ratio(ch12$infection.site, ch12$district, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp, 0.06728363)
  expect_equal(lik.ratio(ch12$infection.site, ch12$district, seed = 1)$pval.mc,
               0.04813)
})

test_that("Exercise 12.5", {
  expect_equal(lik.ratio(ch12$win.opinion, ch12$supporter, seed = 1)$pval.mc,
               0.07012)
})

test_that("Example 13.1", {
  expect_equal(lik.ratio(ch13$physical.activity[ch13$gender == "Boy"],
                         ch13$tv.viewing[ch13$gender == "Boy"],
                         seed = 1)$pval.mc, 0.0447)
  expect_equal(lik.ratio(ch13$physical.activity[ch13$gender == "Girl"],
                         ch13$tv.viewing[ch13$gender == "Girl"],
                         seed = 1)$pval.mc, 0.00607)
})

test_that("Example 13.12", {
  expect_equal(lik.ratio(ch13$chemo.drug, ch13$chemo.side.effect,
                         seed = 1)$pval.mc, 0.00010)
  expect_equal(lik.ratio(ch13$chemo.drug, ch13$chemo.side.effect,
                         do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
               0.000061414559)

  expect_equal(lik.ratio(
    ch13$chemo.drug[ch13$chemo.side.effect == "Hair loss" |
                      ch13$chemo.side.effect == "Visual impairment"],
    ch13$chemo.side.effect[ch13$chemo.side.effect == "Hair loss" |
                             ch13$chemo.side.effect == "Visual impairment"],
    seed = 1)$pval.mc, 0.00337)
  expect_equal(lik.ratio(
    ch13$chemo.drug[ch13$chemo.side.effect == "Hair loss" |
                      ch13$chemo.side.effect == "Visual impairment"],
    ch13$chemo.side.effect[ch13$chemo.side.effect == "Hair loss" |
                             ch13$chemo.side.effect == "Visual impairment"],
    do.exact = FALSE, do.asymp = TRUE)$pval.asymp, 0.00182512405)

  chemo.side.effect.2 <- ch13$chemo.side.effect
  levels(chemo.side.effect.2) <-
    list("Hair loss or Visual impairment" = c("Hair loss", "Visual impairment"),
         "Hair loss & Visual impairment" = "Hair loss & Visual impairment",
         "None" = "None")
  expect_equal(lik.ratio(
    ch13$chemo.drug[chemo.side.effect.2 == "Hair loss or Visual impairment" |
                      chemo.side.effect.2 == "Hair loss & Visual impairment"],
    chemo.side.effect.2[chemo.side.effect.2 == "Hair loss or Visual impairment" |
                          chemo.side.effect.2 == "Hair loss & Visual impairment"],
    seed = 1)$pval.mc, 0.00086)
  expect_equal(lik.ratio(
    ch13$chemo.drug[chemo.side.effect.2 == "Hair loss or Visual impairment" |
                 chemo.side.effect.2 == "Hair loss & Visual impairment"],
    chemo.side.effect.2[chemo.side.effect.2 == "Hair loss or Visual impairment" |
                          chemo.side.effect.2 == "Hair loss & Visual impairment"],
    do.exact = FALSE, do.asymp = TRUE)$pval.asymp, 0.00046737958)

  chemo.side.effect.3 <- ch13$chemo.side.effect
  levels(chemo.side.effect.3) <-
    list("Side-effect" = c("Hair loss", "Visual impairment",
                           "Hair loss & Visual impairment"), "None" = "None")
  expect_equal(lik.ratio(ch13$chemo.drug, chemo.side.effect.3,
                         seed = 1)$pval.mc, 0.83606)
  expect_equal(lik.ratio(ch13$chemo.drug, chemo.side.effect.3, do.exact = FALSE,
                         do.asymp = TRUE)$pval.asymp, 0.6830428)
})

test_that("Exercise 13.3", {
  expect_equal(
    lik.ratio(ch13$breakfast.eaten[ch13$boys.girls == "Boys"],
              ch13$VEL[ch13$boys.girls == "Boys"], do.exact = FALSE,
              do.asymp = TRUE)$pval.asymp, 0.66599628)
  expect_equal(
    lik.ratio(ch13$breakfast.eaten[ch13$boys.girls == "Girls"],
              ch13$VEL[ch13$boys.girls == "Girls"], do.exact = FALSE,
              do.asymp = TRUE)$pval.asymp, 0.029181087)
})

test_that("Exercise 13.7", {
  expect_equal(
    lik.ratio(ch13$medicine[ch13$location == "Urban"],
              ch13$response[ch13$location == "Urban"], seed = 1)$pval.mc, 0)
  expect_equal(
    lik.ratio(ch13$medicine[ch13$location == "Rural"],
              ch13$response[ch13$location == "Rural"], seed = 1)$pval.mc,
    0.0032)
})
