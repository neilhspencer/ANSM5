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
