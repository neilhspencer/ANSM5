test_that("Example 8.10", {
  expect_equal(kruskal.wallis.lsd(ch8$sentences, ch8$authors,
                                  c("Vulliamy", "Queen"))$pval.asymp.stat,
               3.7294898)
  expect_equal(kruskal.wallis.lsd(ch8$sentences, ch8$authors,
                                  c("Vulliamy", "Queen"))$pval.asymp,
               0.00100687047)
})
