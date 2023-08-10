test_that("Example 8.10", {
  sentences <- c(13, 27, 26, 22, 26,
                 43, 35, 47, 32, 31, 37,
                 33, 37, 33, 26, 44, 33, 54)
  authors <- factor(c(rep("Vulliamy", 5), rep("Queen", 6), rep("McCloy", 7)))
  expect_equal(kruskal.wallis.lsd(sentences, authors,
                                  c("Vulliamy", "Queen"))$pval.asymp.stat,
               2.7934198)
  expect_equal(kruskal.wallis.lsd(sentences, authors,
                                  c("Vulliamy", "Queen"))$pval.asymp,
               0.0068197883)
})
