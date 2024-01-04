test_that("Example 6.10", {
  expect_equal(
    normal.scores.test(ch6$groupA, ch6$groupB)$pval.exact.stat,
    -4.9861603)
  expect_equal
  (normal.scores.test(ch6$groupA, ch6$groupB)$pval.exact, 0.011686456)
  expect_equal(normal.scores.test(ch6$groupA, ch6$groupB,
                                  do.asymp = TRUE)$pval.asymp.stat, -4.9861603)
  expect_equal(normal.scores.test(ch6$groupA, ch6$groupB,
                                  do.asymp = TRUE)$pval.asymp, 0.010654119)
})

test_that("Exercise 6.15", {
  expect_equal(normal.scores.test(ch6$doseI, ch6$doseII)$pval.exact,
               0.0080213904)
})

test_that("Example 8.9", {
  groupAB.sum <- (ch8$periodI + ch8$periodII)[ch8$sequence == "AB"]
  groupBA.sum <- (ch8$periodI + ch8$periodII)[ch8$sequence == "BA"]
  expect_equal(normal.scores.test(groupAB.sum, groupBA.sum)$pval.exact.stat,
               -1.4942462)
  expect_equal(normal.scores.test(groupAB.sum, groupBA.sum)$pval.exact,
               0.27777778)
})
