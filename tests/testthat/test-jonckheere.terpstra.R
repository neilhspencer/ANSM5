test_that("Example 7.3", {
  age <- c(66, 66, 69, 77, 77, 78, 40, 52, 56, 58, 66, 67, 55, 63)
  features <- as.factor(c(rep("1/2", 6), rep("3", 6), rep("4", 2)))
  expect_equal(jonckheere.terpstra(age, features)$pval.exact.stat, 9)
  expect_equal(jonckheere.terpstra(age, features)$pval.exact, 0.0046620047)
  expect_equal(jonckheere.terpstra(age, features, do.exact = FALSE,
                                   do.asymp = TRUE,
                                   do.asymp.ties.adjust = FALSE)$pval.asymp.stat, -2.52810291)
  expect_equal(jonckheere.terpstra(age, features, do.exact = FALSE,
                                   do.asymp = TRUE,
                                   do.asymp.ties.adjust = FALSE)$pval.asymp, 0.0057340365)
})

test_that("Example 12.7", {
  dose <- factor(c(rep("100mg", 51), rep("200mg", 61), rep("300mg", 42),
                   rep("400mg", 34)),
                 levels = c("400mg", "300mg", "200mg", "100mg"))
  side.effects <- factor(c(rep("None", 50), "Moderate",
                           rep("None", 60), "Slight",
                           rep("None", 40), "Slight", "Moderate",
                           rep("None", 30), "Slight", "Moderate", rep("Severe", 2)),
                         levels = c("None", "Slight", "Moderate", "Severe"))
  expect_equal(jonckheere.terpstra(as.numeric(side.effects), dose,
                                   do.asymp = TRUE, do.exact = FALSE,
                                   do.asymp.ties.adjust = FALSE)$pval.asymp.stat, -0.74273281)
  expect_equal(jonckheere.terpstra(as.numeric(side.effects), dose, do.asymp = TRUE,
                                   do.exact = FALSE,
                                   do.asymp.ties.adjust = FALSE)$pval.asymp, 0.22882173)
})

test_that("Example 12.8", {
  dose <- factor(c(rep("100mg", 51), rep("200mg", 61), rep("300mg", 42),
                   rep("400mg", 34)),
                 levels = c("400mg", "300mg", "200mg", "100mg"))
  side.effects <- factor(c(rep("None", 50), "Moderate",
                           rep("None", 60), "Slight",
                           rep("None", 40), "Slight", "Moderate",
                           rep("None", 30), "Slight", "Moderate", rep("Severe", 2)),
                         levels = c("None", "Slight", "Moderate", "Severe"))
  jt.out <- jonckheere.terpstra(as.numeric(side.effects), dose, seed = 1)
  expect_equal(jt.out$pval.mc.stat, 6217)
  expect_equal(jt.out$pval.mc, 0.0176)
  expect_equal(jonckheere.terpstra(as.numeric(side.effects), dose,
                                   do.asymp = TRUE,
                                   do.exact = FALSE)$pval.asymp.stat, -2.1248932)
  expect_equal(jonckheere.terpstra(as.numeric(side.effects), dose,
                                   do.asymp = TRUE,
                                   do.exact = FALSE)$pval.asymp, 0.0167977625)
})

test_that("Example 12.9", {
  platelet.count <- factor(c(rep("Normal", 13),
                             rep("Abnormal", 2), rep("Normal", 10),
                             rep("Abnormal", 5), rep("Normal", 6),
                             rep("Abnormal", 3), "Normal"),
                           levels = c("Abnormal", "Normal"))
  spleen.size <- factor(c(rep(0, 13), rep(1, 12), rep(2, 11), rep(3, 4)),
                        levels = c(0, 1, 2, 3))
  expect_equal(jonckheere.terpstra(as.numeric(platelet.count), spleen.size,
                                   do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp.stat, -3.40771709)
  expect_equal(jonckheere.terpstra(as.numeric(platelet.count), spleen.size,
                                   do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp, 0.00032754388)
  expect_equal(jonckheere.terpstra(as.numeric(platelet.count), spleen.size,
                                   seed = 1)$pval.mc.stat, 183)
  expect_equal(jonckheere.terpstra(as.numeric(platelet.count), spleen.size,
                                   seed = 1)$pval.mc, 0.0002)
})
