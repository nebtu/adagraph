test_that("simulation works as in paper", {
  skip_on_cran()
  design <- get_paper_sim_design()

  eff1 <- c(0, 0, 0, 0, 0)
  eff2 <- c(0, 0, 0, 0, 0.4)

  runs1 <- 10

  set.seed(0)
  fwer <- lapply(c(.75, .50, .25, 0), function(futility) {
    sim_wrap(
      eff1,
      design = design,
      runs1 = runs1,
      runs2 = 100,
      futility = futility
    )$fwer
  })

  power <- lapply(c(.75, .50, .25, 0), function(futility) {
    sim_wrap(
      eff2,
      design = design,
      runs1 = runs1,
      runs2 = 100,
      futility = futility
    )$rej2_4
  })

  expect_equal(sapply(fwer, mean), c(0.011, 0.010, 0.010, 0.049))
  expect_equal(sapply(power, mean), c(0.692, 0.756, 0.768, 0.874))
})
