test_that("Example 3.11", {
  expect_equal(pitman(ch3data$heartrates1, 70, "greater")$pval.exact.stat, 95)
  expect_equal(pitman(ch3data$heartrates1, 70, "greater")$pval.exact, 0.0546875)
  expect_equal(pitman(ch3data$heartrates1, 70, "greater", do.asymp = TRUE)$pval.asymp.stat, 1.63691848888252 )
  expect_equal(pitman(ch3data$heartrates1, 70, "greater", do.asymp = TRUE)$pval.asymp, 0.050823751)
  expect_equal(pitman(ch3data$heartrates1)$CI.mc.lower, 68)
  expect_equal(pitman(ch3data$heartrates1)$CI.mc.upper, 97)
})

test_that("Example 3.12", {
  expect_equal(pitman(ch3data$sampleA, 9)$pval.exact, 0.3515625)
  expect_equal(pitman(ch3data$sampleA, 9, do.asymp = TRUE)$pval.asymp, 0.3051186)
  expect_equal(pitman(ch3data$sampleB, 9)$pval.exact, 0.9159565)
  expect_equal(pitman(ch3data$sampleB, 9, do.asymp = TRUE)$pval.asymp, 0.81653892)
  expect_equal(pitman(ch3data$sampleC, 9)$pval.exact, 0.123046875)
  expect_equal(pitman(ch3data$sampleD, 9)$pval.exact, 0.035497665)
})

test_that("Exercise 3.17", {
  expect_equal(pitman(ch3data$sampleI, 110)$pval.exact, 0.05859375)
  expect_equal(pitman(ch3data$sampleI, 110, do.asymp = TRUE)$pval.asymp, 0.064739074)
  expect_equal(pitman(ch3data$sampleII, 110)$pval.exact, 0.0220565796)
  expect_equal(pitman(ch3data$sampleII, 110, do.asymp = TRUE)$pval.asymp, 0.0241634455)
})
