test_that("Example 7.3", {
  expect_equal(jonckheere.terpstra(ch7$dementia.age, ch7$features,
                                   alternative = "greater")$pval.exact.stat, 9)
  expect_equal(jonckheere.terpstra(ch7$dementia.age, ch7$features,
                                   alternative = "greater")$pval.exact,
               0.0046620047)
  expect_equal(
    jonckheere.terpstra(ch7$dementia.age, ch7$features,
                        alternative = "greater", do.exact = FALSE,
                        do.asymp = TRUE,
                        do.asymp.ties.adjust = FALSE)$pval.asymp.stat,
    -2.52810291)
  expect_equal(
    jonckheere.terpstra(ch7$dementia.age, ch7$features,
                        alternative = "greater", do.exact = FALSE,
                        do.asymp = TRUE,
                        do.asymp.ties.adjust = FALSE)$pval.asymp, 0.0057340365)
  expect_equal(
    jonckheere.terpstra(ch7$dementia.age, ch7$features,
                        alternative = "greater", do.exact = FALSE,
                        do.asymp = TRUE)$pval.asymp, 0.0054947789)
})

test_that("Exercise 7.5", {
  expect_equal(jonckheere.terpstra(ch7$braking.distance,
                                   ch7$speed)$pval.exact.stat, 32.5)
  expect_equal(jonckheere.terpstra(ch7$braking.distance,
                                   ch7$speed)$pval.exact, 0.0011111111)
  expect_equal(jonckheere.terpstra(ch7$braking.distance, ch7$speed,
                                   do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp, 0.00219353184)
})

test_that("Exercise 7.6", {
  expect_equal(jonckheere.terpstra(ch7$platelet.count, ch7$spleen.size,
                                   alternative = "greater",
                                   seed = 1)$pval.mc.stat, 119)
  expect_equal(jonckheere.terpstra(ch7$platelet.count, ch7$spleen.size,
                                   alternative = "greater", seed = 1)$pval.mc, 0)
  expect_equal(jonckheere.terpstra(ch7$platelet.count, ch7$spleen.size,
                                   alternative = "greater", do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp, 0.0000194445645)
})

test_that("Exercise 8.10", {
  expect_equal(jonckheere.terpstra(ch8$silver.content, ch8$dynasty,
                                   alternative = "less", seed = 1)$pval.mc,
               0.0077)
  expect_equal(jonckheere.terpstra(ch8$silver.content, ch8$dynasty,
                                   alternative = "less", do.exact = FALSE,
                                   do.asymp = TRUE)$pval.asymp, 0.0084307191)
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
