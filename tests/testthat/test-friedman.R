test_that("Example 7.6", {
  pulse <- c(72, 96, 88, 92, 74, 76, 82, 120, 120, 132, 120, 101, 96, 112, 76,
             95, 104, 96, 84, 72, 76)
  student <- as.factor(c("A", "B", "C", "D", "E", "F", "G", "A", "B", "C", "D",
                         "E", "F", "G", "A", "B", "C", "D", "E", "F", "G"))
  time_period <- as.factor(c("I", "I", "I", "I", "I", "I", "I", "II", "II",
                             "II", "II", "II", "II", "II", "III", "III", "III",
                             "III", "III", "III", "III"))
  expect_equal(friedman(pulse, time_period, student,
                                  do.asymp = TRUE)$pval.asymp.stat, 10.5714286)
  expect_equal(friedman(pulse, time_period, student,
                                  do.asymp = TRUE)$pval.asymp, 0.0050634142)
})

test_that("Example 7.7", {
  nodes <- c(60, 65, 63, 64, 62, 61, 62, 65, 61, 67, 65, 62, 61, 68, 61, 63, 62,
             62, 60, 65, 60, 61, 64, 65)
  treatment <- as.factor(c("Control", "Gibberellic acid", "Kinetin",
                           "Indole acetic acid", "Adenine sulphate",
                           "Maleic hydrazide", "Control", "Gibberellic acid",
                           "Kinetin", "Indole acetic acid", "Adenine sulphate",
                           "Maleic hydrazide", "Control", "Gibberellic acid",
                           "Kinetin", "Indole acetic acid", "Adenine sulphate",
                           "Maleic hydrazide", "Control", "Gibberellic acid",
                           "Kinetin", "Indole acetic acid", "Adenine sulphate",
                           "Maleic hydrazide"))
  blocks <- as.factor(c("I", "I", "I", "I", "I", "I", "II", "II", "II", "II",
                        "II", "II", "III", "III", "III", "III", "III", "III",
                        "IV", "IV", "IV", "IV", "IV", "IV"))
  expect_equal(friedman(nodes, treatment, blocks,
                                  do.asymp = TRUE)$pval.asymp.stat, 6.4035088)
  expect_equal(friedman(nodes, treatment, blocks,
                                  do.asymp = TRUE)$pval.asymp, 0.0022524635)
})
