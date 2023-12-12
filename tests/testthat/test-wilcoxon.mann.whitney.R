test_that("Examples 6.1 and 6.2", {
  expect_equal(
    wilcoxon.mann.whitney(ch6data$groupA, ch6data$groupB)$pval.exact.stat,
    paste0("\n","76 (rank sum from ch6data$groupA), ",
           "155 (rank sum from ch6data$groupB)", "\n",
           "21 (Mann-Whitney U from ch6data$groupA), ",
           "89 (Mann-Whitney U from ch6data$groupB)"))
  expect_equal(wilcoxon.mann.whitney(ch6data$groupA, ch6data$groupB)$pval.exact,
               0.015871126)
})

test_that("Example 6.3", {
  expect_equal(
    wilcoxon.mann.whitney(ch6data$groupA, ch6data$groupB)$CI.exact.lower, -13)
  expect_equal(
    wilcoxon.mann.whitney(ch6data$groupA, ch6data$groupB)$CI.exact.upper, -2)
  expect_equal(wilcoxon.mann.whitney(ch6data$groupA, ch6data$groupB,
                                     CI.width = 0.99)$CI.exact.lower, -16)
  expect_equal(wilcoxon.mann.whitney(ch6data$groupA, ch6data$groupB,
                                     CI.width = 0.99)$CI.exact.upper, 1)
})

test_that("Example 6.4", {
  expect_equal(
    wilcoxon.mann.whitney(ch6data$groupA.sch2, ch6data$groupB.sch2)$pval.exact,
    0.03586652)
})

test_that("Example 6.5", {
  expect_equal(
    wilcoxon.mann.whitney(groupA.sch2.grp, groupB.sch2.grp)$pval.exact,
    0.044156106)
})

test_that("Example 6.6", {
  expect_equal(wilcoxon.mann.whitney(ch6data$McGamma, ch6data$McBeta,
                                     do.exact = FALSE, do.asymp = TRUE,
                                     cont.corr = FALSE)$pval.asymp, 0.41896844)
  expect_equal(wilcoxon.mann.whitney(ch6data$McGamma, ch6data$McBeta,
                                     do.exact = FALSE,
                                     do.asymp = TRUE)$pval.asymp, 0.425550392)
  set.seed(1)
  expect_equal(mean(
    sapply(1:10000, function(x){
      wilcoxon.mann.whitney(
        jitter(ch6data$McGamma),jitter(ch6data$McBeta))$pval.exact})),
    0.42960798)
})

test_that("Example 6.7", {
  expect_equal(wilcoxon.mann.whitney(ch6data$males, ch6data$females)$pval.exact,
               0.12812405)
})

test_that("Example 6.8", {
  expect_equal(wilcoxon.mann.whitney(ch6data$males, ch6data$females,
                                     do.asymp = TRUE)$CI.asymp.lower,
               -1.00002198)
  expect_equal(wilcoxon.mann.whitney(ch6data$males, ch6data$females,
                                     do.asymp = TRUE)$CI.asymp.upper, 11.99999)
})

test_that("Example 6.11", {
  expect_equal(
    wilcoxon.mann.whitney(ch6data$typeA, ch6data$typeB)$pval.exact.stat,
               paste0("\n","112.5 (rank sum from ch6data$typeA), ",
                      "97.5 (rank sum from ch6data$typeB)", "\n",
                      "76.5 (Mann-Whitney U from ch6data$typeA), ",
                      "19.5 (Mann-Whitney U from ch6data$typeB)"))
  expect_equal(wilcoxon.mann.whitney(ch6data$typeA, ch6data$typeB)$pval.exact,
               0.026768278)
})

test_that("Example 6.17", {
  expect_equal(
    wilcoxon.mann.whitney(ch6data$groupA, ch6data$groupB)$pval.exact.stat,
               paste0("\n","76 (rank sum from ch6data$groupA), ",
                      "155 (rank sum from ch6data$groupB)", "\n",
                      "21 (Mann-Whitney U from ch6data$groupA), ",
                      "89 (Mann-Whitney U from ch6data$groupB)"))
  expect_equal(wilcoxon.mann.whitney(ch6data$groupA, ch6data$groupB)$pval.exact,
               0.015871126)
})

test_that("Example 8.9", {
  Sequence <- c("AB", "AB", "AB", "AB", "AB", "BA", "BA", "BA", "BA", "BA")
  PeriodI <- c(1.75, 0.3, 0.35, 0.2, 0.3, 7.2, 7.1, 0.75, 2.15, 3.35)
  PeriodII <- c(0.55, 1.05, 0.63, 1.55, 8.2, 0.35, 1.55, 0.25, 0.35, 1.5)
  GroupAB.sum <- (PeriodI + PeriodII)[Sequence == "AB"]
  GroupBA.sum <- (PeriodI + PeriodII)[Sequence == "BA"]
  GroupAB.diff <- (PeriodI - PeriodII)[Sequence == "AB"]
  GroupBA.diff <- (PeriodI - PeriodII)[Sequence == "BA"]
  GroupBA.diff2 <- (PeriodII - PeriodI)[Sequence == "BA"]
  expect_equal(wilcoxon.mann.whitney(GroupAB.sum, GroupBA.sum)$pval.exact.stat,
               paste0("\n","22 (rank sum from GroupAB.sum), ",
                      "33 (rank sum from GroupBA.sum)", "\n",
                      "7 (Mann-Whitney U from GroupAB.sum), ",
                      "18 (Mann-Whitney U from GroupBA.sum)"))
  expect_equal(wilcoxon.mann.whitney(GroupAB.sum, GroupBA.sum)$pval.exact,
               0.30952381)
  expect_equal(wilcoxon.mann.whitney(GroupAB.diff, GroupBA.diff)$pval.exact.stat,
               paste0("\n","16 (rank sum from GroupAB.diff), ",
                      "39 (rank sum from GroupBA.diff)", "\n",
                      "1 (Mann-Whitney U from GroupAB.diff), ",
                      "24 (Mann-Whitney U from GroupBA.diff)"))
  expect_equal(wilcoxon.mann.whitney(GroupAB.diff, GroupBA.diff)$pval.exact,
               0.015873016)
  expect_equal(wilcoxon.mann.whitney(GroupAB.diff, GroupBA.diff2)$pval.exact.stat,
               paste0("\n","33 (rank sum from GroupAB.diff), ",
                      "22 (rank sum from GroupBA.diff2)", "\n",
                      "18 (Mann-Whitney U from GroupAB.diff), ",
                      "7 (Mann-Whitney U from GroupBA.diff2)"))
  expect_equal(wilcoxon.mann.whitney(GroupAB.diff, GroupBA.diff2)$pval.exact,
               0.30952381)
})

test_that("Following Example 9.3", {
  SampleI <-c(9, 25, 35, 38, 41)
  SampleIIa <-c(10, 26, 29, 36, 37, 48, 57, 58)
  expect_equal(wilcoxon.mann.whitney(SampleI, SampleIIa,
                                     alternative = "less")$pval.exact.stat,
               paste0("\n","29 (rank sum from SampleI), ",
                      "62 (rank sum from SampleIIa)", "\n",
                      "14 (Mann-Whitney U from SampleI), ",
                      "26 (Mann-Whitney U from SampleIIa)"))
expect_equal(wilcoxon.mann.whitney(SampleI, SampleIIa,
                                     alternative = "less")$pval.exact,
               0.21756022)
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
  side.effect.level.A <- as.numeric(side.effect.level[drug == "Drug A"])
  side.effect.level.B <- as.numeric(side.effect.level[drug == "Drug B"])
  wmw.out <- wilcoxon.mann.whitney(side.effect.level.A, side.effect.level.B,
                                   do.CI = FALSE, seed = 1,
                                   nsims.mc = 1000000)
  expect_equal(wmw.out$pval.mc.stat,
               paste0("\n","2624 (rank sum from side.effect.level.A), ",
                       "2326 (rank sum from side.effect.level.B)", "\n",
                       "1589 (Mann-Whitney U from side.effect.level.A), ",
                       "841 (Mann-Whitney U from side.effect.level.B)"))
  expect_equal(wmw.out$pval.mc, 0.001872)
  wmw.out <- wilcoxon.mann.whitney(side.effect.level.A, side.effect.level.B,
                                   do.CI = FALSE, do.exact = FALSE,
                                   do.asymp = TRUE)
  expect_equal(wmw.out$pval.asymp.stat,
               paste0("\n","2624 (rank sum from side.effect.level.A), ",
                      "2326 (rank sum from side.effect.level.B)", "\n",
                      "1589 (Mann-Whitney U from side.effect.level.A), ",
                      "841 (Mann-Whitney U from side.effect.level.B)"))
  expect_equal(wmw.out$pval.asymp, 0.00184697407)
})
