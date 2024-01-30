test_that("Section 13.2.5", {
  expect_equal(zelen(ch13$drug, ch13$side.effects,
                     ch13$age.group)$pval.exact.stat, 0.0082126222)
  expect_equal(zelen(ch13$drug, ch13$side.effects, ch13$age.group)$pval.exact,
               0.068931048)
})

test_that("Example 13.3", {
  expect_equal(zelen(ch13$machine, ch13$output.status,
                     ch13$material.source)$pval.exact.stat, 0.14161837)
  expect_equal(zelen(ch13$machine, ch13$output.status,
                     ch13$material.source)$pval.exact, 1)
})
