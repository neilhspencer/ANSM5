test_that("Example 6.14", {
  expect_equal(
    moses.extreme.reactions(ch6$groupI, ch6$groupII)$pval.exact.stat,
    12)
  expect_equal(
    moses.extreme.reactions(ch6$groupI, ch6$groupII)$pval.exact,
    0.0148844963)
  expect_equal(
    moses.extreme.reactions(ch6$groupI.trimmed, ch6$groupII)$pval.exact,
    0.036404772)
  expect_equal(
    moses.extreme.reactions(ch6$groupI.amended, ch6$groupII)$pval.exact.stat,
    13)
  expect_equal(
    moses.extreme.reactions(ch6$groupI.amended, ch6$groupII)$pval.exact,
    0.03989045)
})
