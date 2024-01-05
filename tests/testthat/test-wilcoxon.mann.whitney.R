test_that("Examples 6.1 and 6.2", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$groupA, ch6$groupB)$pval.exact.stat,
    paste0("\n","76 (rank sum from ch6$groupA), ",
           "155 (rank sum from ch6$groupB)", "\n",
           "21 (Mann-Whitney U from ch6$groupA), ",
           "89 (Mann-Whitney U from ch6$groupB)"))
  expect_equal(wilcoxon.mann.whitney(ch6$groupA, ch6$groupB)$pval.exact,
               0.015871126)
})

test_that("Example 6.3", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$groupA, ch6$groupB)$CI.exact.lower, -13)
  expect_equal(
    wilcoxon.mann.whitney(ch6$groupA, ch6$groupB)$CI.exact.upper, -2)
  expect_equal(wilcoxon.mann.whitney(ch6$groupA, ch6$groupB,
                                     CI.width = 0.99)$CI.exact.lower, -16)
  expect_equal(wilcoxon.mann.whitney(ch6$groupA, ch6$groupB,
                                     CI.width = 0.99)$CI.exact.upper, 1)
})

test_that("Example 6.4", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$groupA.sch2, ch6$groupB.sch2)$pval.exact,
    0.03586652)
})

test_that("Example 6.5", {
  expect_equal(
    wilcoxon.mann.whitney(groupA.sch2.grp, groupB.sch2.grp)$pval.exact,
    0.044156106)
})

test_that("Example 6.6", {
  expect_equal(wilcoxon.mann.whitney(ch6$McGamma, ch6$McBeta,
                                     do.exact = FALSE, do.asymp = TRUE,
                                     cont.corr = FALSE)$pval.asymp, 0.41896844)
  expect_equal(wilcoxon.mann.whitney(ch6$McGamma, ch6$McBeta,
                                     do.exact = FALSE,
                                     do.asymp = TRUE)$pval.asymp, 0.425550392)
  set.seed(1)
  expect_equal(mean(
    sapply(1:10000, function(x){
      wilcoxon.mann.whitney(
        jitter(ch6$McGamma),jitter(ch6$McBeta))$pval.exact})),
    0.42960798)
})

test_that("Example 6.7", {
  expect_equal(wilcoxon.mann.whitney(ch6$males, ch6$females)$pval.exact,
               0.12812405)
})

test_that("Example 6.8", {
  expect_equal(wilcoxon.mann.whitney(ch6$males, ch6$females,
                                     do.asymp = TRUE)$CI.asymp.lower,
               -1.00002198)
  expect_equal(wilcoxon.mann.whitney(ch6$males, ch6$females,
                                     do.asymp = TRUE)$CI.asymp.upper, 11.99999)
})

test_that("Example 6.11", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$typeA, ch6$typeB)$pval.exact.stat,
               paste0("\n","112.5 (rank sum from ch6$typeA), ",
                      "97.5 (rank sum from ch6$typeB)", "\n",
                      "76.5 (Mann-Whitney U from ch6$typeA), ",
                      "19.5 (Mann-Whitney U from ch6$typeB)"))
  expect_equal(wilcoxon.mann.whitney(ch6$typeA, ch6$typeB)$pval.exact,
               0.026768278)
})

test_that("Example 6.17", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$groupA, ch6$groupB)$pval.exact.stat,
               paste0("\n","76 (rank sum from ch6$groupA), ",
                      "155 (rank sum from ch6$groupB)", "\n",
                      "21 (Mann-Whitney U from ch6$groupA), ",
                      "89 (Mann-Whitney U from ch6$groupB)"))
  expect_equal(wilcoxon.mann.whitney(ch6$groupA, ch6$groupB)$pval.exact,
               0.015871126)
})


test_that("Exercise 6.1", {
  expect_equal(wilcoxon.mann.whitney(ch6$temp.H, ch6$temp.L)$pval.exact,
               0.054895105)
})

test_that("Exercise 6.2", {
  expect_equal(wilcoxon.mann.whitney(ch5$LVF, ch5$RVF)$pval.exact,
               0.271898515)
})

test_that("Exercise 6.3", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$DMF.M, ch6$DMF.F, do.exact = FALSE,
                          do.asymp = TRUE)$pval.asymp, 0.004461907)
  expect_equal(wilcoxon.mann.whitney(ch6$DMF.M, ch6$DMF.F,
                                     seed = 1)$pval.mc, 0.00408)
})

test_that("Exercise 6.4", {
  expect_equal(wilcoxon.mann.whitney(ch6$weight.diabetic,
                                     ch6$weight.normal)$pval.exact,
               0.00016895687)
})

test_that("Exercise 6.5", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$cooling.time.standard,
                          ch6$cooling.time.cheap)$pval.exact, 0.13879062)
  expect_equal(
    wilcoxon.mann.whitney(ch6$cooling.time.standard,
                          ch6$cooling.time.cheap)$CI.exact.lower, -0.2)
  expect_equal(
    wilcoxon.mann.whitney(ch6$cooling.time.standard,
                          ch6$cooling.time.cheap)$CI.exact.upper, 1.9)
})

test_that("Exercise 6.6", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$wait.1979, ch6$wait.1983,
                          do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
    0.0040175478)
})

test_that("Exercise 6.7", {
  expect_equal(
    wilcoxon.mann.whitney(ch6$activity.boys, ch6$activity.girls,
                          do.exact = FALSE, do.asymp = TRUE)$pval.asymp,
    3.139189e-22)
})

test_that("Exercise 6.15", {
  expect_equal(wilcoxon.mann.whitney(ch6$doseI, ch6$doseII)$pval.exact,
               0.0106129165)
  expect_equal(wilcoxon.mann.whitney(ch6$doseI,
                                     ch6$doseII)$CI.exact.lower, -17.42)
  expect_equal(wilcoxon.mann.whitney(ch6$doseI,
                                     ch6$doseII)$CI.exact.upper, -2.4)
})

test_that("Exercise 6.16", {
  expect_equal(wilcoxon.mann.whitney(ch6$travel,
                                     ch6$politics)$pval.exact, 0.254519474)
})

test_that("Example 8.9", {
  groupAB.sum <- (ch8$periodI + ch8$periodII)[ch8$sequence == "AB"]
  groupBA.sum <- (ch8$periodI + ch8$periodII)[ch8$sequence == "BA"]
  groupAB.diff <- (ch8$periodI - ch8$periodII)[ch8$sequence == "AB"]
  groupBA.diff <- (ch8$periodI - ch8$periodII)[ch8$sequence == "BA"]
  groupBA.diff2 <- (ch8$periodII - ch8$periodI)[ch8$sequence == "BA"]
  expect_equal(wilcoxon.mann.whitney(groupAB.sum, groupBA.sum)$pval.exact.stat,
               paste0("\n","22 (rank sum from groupAB.sum), ",
                      "33 (rank sum from groupBA.sum)", "\n",
                      "7 (Mann-Whitney U from groupAB.sum), ",
                      "18 (Mann-Whitney U from groupBA.sum)"))
  expect_equal(wilcoxon.mann.whitney(groupAB.sum, groupBA.sum)$pval.exact,
               0.30952381)
  expect_equal(wilcoxon.mann.whitney(groupAB.diff, groupBA.diff)$pval.exact.stat,
               paste0("\n","16 (rank sum from groupAB.diff), ",
                      "39 (rank sum from groupBA.diff)", "\n",
                      "1 (Mann-Whitney U from groupAB.diff), ",
                      "24 (Mann-Whitney U from groupBA.diff)"))
  expect_equal(wilcoxon.mann.whitney(groupAB.diff, groupBA.diff)$pval.exact,
               0.015873016)
  expect_equal(wilcoxon.mann.whitney(groupAB.diff, groupBA.diff2)$pval.exact.stat,
               paste0("\n","33 (rank sum from groupAB.diff), ",
                      "22 (rank sum from groupBA.diff2)", "\n",
                      "18 (Mann-Whitney U from groupAB.diff), ",
                      "7 (Mann-Whitney U from groupBA.diff2)"))
  expect_equal(wilcoxon.mann.whitney(groupAB.diff, groupBA.diff2)$pval.exact,
               0.30952381)
})

test_that("Exercise 8.6", {
  expect_equal(wilcoxon.mann.whitney(
    ch8$periodI.mistakes.AB + ch8$periodII.mistakes.AB,
    ch8$periodI.mistakes.BA + ch8$periodII.mistakes.BA)$pval.exact, 0.74125874)
  expect_equal(wilcoxon.mann.whitney(
    ch8$periodI.mistakes.AB - ch8$periodII.mistakes.AB,
    ch8$periodI.mistakes.BA - ch8$periodII.mistakes.BA)$pval.exact, 0.21037296)
  expect_equal(wilcoxon.mann.whitney(
    ch8$periodI.mistakes.AB - ch8$periodII.mistakes.AB,
    ch8$periodII.mistakes.BA - ch8$periodI.mistakes.BA)$pval.exact, 0.47086247)
})

test_that("Exercise 8.7", {
  expect_equal(wilcoxon.mann.whitney(ch8$periodI.time.AB + ch8$periodII.time.AB,
    ch8$periodI.time.BA + ch8$periodII.time.BA)$pval.exact, 0.82362082)
  expect_equal(wilcoxon.mann.whitney(ch8$periodI.time.AB - ch8$periodII.time.AB,
    ch8$periodI.time.BA - ch8$periodII.time.BA)$pval.exact, 0.0013986014)
  expect_equal(wilcoxon.mann.whitney(ch8$periodI.time.AB - ch8$periodII.time.AB,
    ch8$periodII.time.BA - ch8$periodI.time.BA)$pval.exact, 1)
})

test_that("Exercise 8.9", {
  expect_equal(wilcoxon.mann.whitney(
    ch8$seizure.score[ch8$hospital == "HospitalA"],
    ch8$seizure.score[ch8$hospital == "HospitalB"])$pval.exact, 0.78018576)
  expect_equal(wilcoxon.mann.whitney(
    ch8$seizure.score[ch8$hospital == "HospitalA"],
    ch8$seizure.score[ch8$hospital == "HospitalC"])$pval.exact, 0.01896796)
  expect_equal(wilcoxon.mann.whitney(
    ch8$seizure.score[ch8$hospital == "HospitalB"],
    ch8$seizure.score[ch8$hospital == "HospitalC"])$pval.exact, 0.027396133)
})

test_that("Example 9.1", {
  asymp.survtime.2 <- ch9$asymp.survtime * (ch9$asymp.censor == 0) +
    362 * (ch9$asymp.censor == 1)
  expect_equal(
    wilcoxon.mann.whitney(ch9$symp.survtime, asymp.survtime.2,
                          alternative = "less", do.exact = FALSE,
                          do.asymp = TRUE)$pval.exact, 0.0040800767)
  expect_equal(
    wilcoxon.mann.whitney(ch9$symp.survtime, asymp.survtime.2,
                          alternative = "less", do.exact = FALSE,
                          do.asymp = TRUE)$pval.asymp, 0.0045926817)
})

test_that("Following Example 9.3", {
  expect_equal(
    wilcoxon.mann.whitney(ch9$sampleI.survtime, ch9$sampleII.survtime.2,
                          alternative = "less")$pval.exact, 0.21756022)
})

test_that("Exercise 9.2", {
  expect_equal(
    wilcoxon.mann.whitney(ch9$boys.toothtime, ch9$girls.toothtime)$pval.exact,
    0.85181485)
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
