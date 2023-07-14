test_that("Example 4.15", {
  births <- c("A", "A", "A", "A", "B", "A", "C", "C", "A", "A", "A", "A", "A",
              "D", "D", "A", "A")
  expect_equal(runs.ncat(births, alternative = "less",
                         cont.corr = TRUE)$pval.asymp.stat, -1.15433076)
  expect_equal(runs.ncat(births, alternative = "less",
                         cont.corr = TRUE)$pval.asymp, 0.124182295)
  expect_equal(runs.ncat(births, alternative = "less", do.mc = TRUE,
                         nsims.mc = 100000, seed = 1)$pval.mc, 0.09714)
})

test_that("Example 7.10", {
  Consultants <- c(97, 157, 181, 215, 216, 520, 527, 540, 566, 580)
  Registrars <- c(305, 320, 345, 350, 356, 454, 456, 471, 485, 489)
  Students <- c(414, 422, 423, 451, 467, 544, 550, 599, 661, 665)
  group <- c(rep("C", 10), rep("R", 10), rep("S", 10))
  time <- c(Consultants, Registrars, Students)
  ordered_labels <- group[order(time)]
  expect_equal(runs.ncat(ordered_labels, alternative = "less",
                         do.mc = TRUE, seed = 1, nsims.mc = 100000)$pval.mc,
               0.00003)
  expect_equal(runs.ncat(ordered_labels, alternative = "less",
                         cont.corr = FALSE)$pval.asymp.stat, -4.2602817)
  expect_equal(runs.ncat(ordered_labels, alternative = "less",
                         cont.corr = FALSE)$pval.asymp, 0.0000102084714)
  expect_equal(runs.ncat(ordered_labels, alternative = "less",
                         cont.corr = TRUE)$pval.asymp.stat, -4.0666325)
  expect_equal(runs.ncat(ordered_labels, alternative = "less",
                         cont.corr = TRUE)$pval.asymp, 0.000023848684)
})
