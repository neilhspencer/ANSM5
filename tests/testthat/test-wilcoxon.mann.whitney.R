test_that("Example 6.1", {
  GroupA <- c(23, 18, 17, 25, 22, 19,  31, 26, 29, 33)
  GroupB <- c(21, 28, 32, 30, 41, 24,  35, 34, 27, 39, 36)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$pval.exact.stat,
               paste0("\n","76 (rank sum from GroupA), ",
                      "155 (rank sum from GroupB)", "\n",
                      "21 (Mann-Whitney U from GroupA), ",
                      "89 (Mann-Whitney U from GroupB)"))
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$pval.exact, 0.015871126)
})

test_that("Example 6.2", {
  GroupA <- c(23, 18, 17, 25, 22, 19,  31, 26, 29, 33)
  GroupB <- c(21, 28, 32, 30, 41, 24,  35, 34, 27, 39, 36)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$pval.exact.stat,
               paste0("\n","76 (rank sum from GroupA), ",
                      "155 (rank sum from GroupB)", "\n",
                      "21 (Mann-Whitney U from GroupA), ",
                      "89 (Mann-Whitney U from GroupB)"))
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$pval.exact, 0.015871126)
})

test_that("Example 6.3", {
  GroupA <- c(23, 18, 17, 25, 22, 19,  31, 26, 29, 33)
  GroupB <- c(21, 28, 32, 30, 41, 24,  35, 34, 27, 39, 36)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$CI.exact.lower, -13)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$CI.exact.upper, -2)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB,
                                     CI.width = 0.99)$CI.exact.lower, -16)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB,
                                     CI.width = 0.99)$CI.exact.upper, 1)
})

test_that("Example 6.4", {
  GroupA <- c(16, 18, 19, 22, 22, 25, 28, 28, 28, 31, 33)
  GroupB <- c(22, 23, 25, 27, 27, 28, 30, 32, 33, 35, 36, 38, 38)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$pval.exact, 0.03586652)
})

test_that("Example 6.5", {
  GroupA <- c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3)
  GroupB <- c(2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$pval.exact, 0.044156106)
})

test_that("Example 6.6", {
  McGamma <- c(13, 13, 22, 26, 33, 33, 59, 72, 72, 72, 77, 78, 78, 80, 81, 82,
               85, 85, 85, 86, 88)
  McBeta <- c(0, 19, 22, 30, 31, 37, 55, 56, 66, 66, 67, 67, 68, 71, 73, 75, 75,
              78, 79, 82, 83, 83, 88, 96)
  expect_equal(wilcoxon.mann.whitney(McGamma, McBeta,
                                     do.exact = FALSE, do.asymp = TRUE,
                                     cont.corr = FALSE)$pval.asymp, 0.41896844)
  expect_equal(wilcoxon.mann.whitney(McGamma, McBeta,
                                     do.exact = FALSE,
                                     do.asymp = TRUE)$pval.asymp, 0.425550392)
  set.seed(1)
  expect_equal(mean(
    sapply(1:10000, function(x){
      wilcoxon.mann.whitney(jitter(McGamma),jitter(McBeta))$pval.exact})),
    0.42960798)
})

test_that("Example 6.11", {
  TypeA <- c(177, 200, 227, 230, 232, 268, 272, 297)
  TypeB <- c(47, 105, 126, 142, 158, 172, 197, 220, 225, 230, 262, 270)
  expect_equal(wilcoxon.mann.whitney(TypeA, TypeB)$pval.exact.stat,
               paste0("\n","112.5 (rank sum from TypeA), ",
                      "97.5 (rank sum from TypeB)", "\n",
                      "76.5 (Mann-Whitney U from TypeA), ",
                      "19.5 (Mann-Whitney U from TypeB)"))
  expect_equal(wilcoxon.mann.whitney(TypeA, TypeB)$pval.exact, 0.026768278)
})

test_that("Example 6.17", {
  GroupA <- c(23, 18, 17, 25, 22, 19, 31, 26, 29, 33)
  GroupB <- c(21, 28, 32, 30, 41, 24, 35, 34, 27, 39, 36)
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$pval.exact.stat,
               paste0("\n","76 (rank sum from GroupA), ",
                      "155 (rank sum from GroupB)", "\n",
                      "21 (Mann-Whitney U from GroupA), ",
                      "89 (Mann-Whitney U from GroupB)"))
  expect_equal(wilcoxon.mann.whitney(GroupA, GroupB)$pval.exact, 0.015871126)
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
