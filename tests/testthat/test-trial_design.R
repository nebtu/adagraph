test_that("example stays constant", {
  des <- make_example_trial()
  des[["alpha_spending_f"]] <- NULL
  expect_snapshot_value(des, style = "json2", tolerance = 1e-5)
})

test_that("same as multiarm design", {
  m <- rbind(
    H1 = c(0, 1 / 2, 1 / 2, 0),
    H2 = c(1 / 2, 0, 0, 1 / 2),
    H3 = c(0, 1, 0, 0),
    H4 = c(1, 0, 0, 0)
  )
  weights <- c(1 / 2, 1 / 2, 0, 0)
  t <- 0.5
  alpha <- 0.025
  as <- function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))

  des <- trial_design(
    arms = 2,
    endpoints = 2,
    subgroups = 0,
    n_control = 35,
    n_arms = 35,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as,
    names = c("H1", "H2", "H3", "H4")
  )
  des_multiarm <- make_example_multiarm()

  #elements that are supposed to be different
  # and alpha_spending_f, because it's environment can be different
  ignore_trial <- c(
    "alpha_spending_f",
    "n_table",
    "names_arms",
    "names_endpoints",
    "names_subgroups",
    "hyp_assoc",
    "subgroups",
    "arms",
    "endpoints"
  )
  ignore_multiarm <- c(
    "alpha_spending_f",
    "controls",
    "treatment_assoc",
    "n_controls",
    "n_treatments"
  )

  expect_mapequal(
    des[!(names(des) %in% ignore_trial)],
    des_multiarm[!(names(des_multiarm) %in% ignore_multiarm)]
  )
})

test_that("more complicated subgroup structures work", {
  # tests for bugs with building the endpoint - subgroup correlation building,
  # where we had a bug earlier

  #fmt: skip
  n_table <- rbind(
    data.frame(arm = "control", G1 = FALSE, G2 = FALSE, G3 = FALSE, n =  0),
    data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  G3 = FALSE, n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, G3 = FALSE, n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  G3 = FALSE, n = 10),
    data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, G3 = FALSE, n = 20),
    data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  G3 = FALSE, n = 30),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, G3 = FALSE, n = 40),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  G3 = FALSE, n = 50),
    data.frame(arm = "A2",      G1 = FALSE, G2 = FALSE, G3 = FALSE, n = 10),
    data.frame(arm = "A2",      G1 = FALSE, G2 = TRUE,  G3 = FALSE, n = 10),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = FALSE, G3 = FALSE, n = 30),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = TRUE,  G3 = FALSE, n = 10),
    data.frame(arm = "control", G1 = FALSE, G2 = FALSE, G3 = TRUE,  n =  0),
    data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  G3 = TRUE,  n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, G3 = TRUE,  n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  G3 = TRUE,  n = 10),
    data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, G3 = TRUE,  n = 20),
    data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  G3 = TRUE,  n = 10),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, G3 = TRUE,  n = 20),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  G3 = TRUE,  n = 20),
    data.frame(arm = "A2",      G1 = FALSE, G2 = FALSE, G3 = TRUE,  n = 10),
    data.frame(arm = "A2",      G1 = FALSE, G2 = TRUE,  G3 = TRUE,  n = 10),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = FALSE, G3 = TRUE,  n = 30),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = TRUE,  G3 = TRUE,  n = 10)
  )
  names_arms <- c("A1", "A2")
  names_subgroups <- c("G1", "G2", "G3")
  as = function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))

  # Note that we only care about the correlation, not the bounds, hence the
  # simplistic weights
  design <- trial_design(
    arms = 2,
    endpoints = 1,
    subgroups = 3,
    n_table = n_table,
    weights = c(1, rep(0, 7)),
    t = 0.5,
    alpha = 0.025,
    alpha_spending_f = as,
    test_m = diag(8),
    names_arms = names_arms,
    names_endpoints = "E1",
    names_subgroups = names_subgroups
  )

  corr <- get_subgroup_correlation(
    subgroups = 3,
    arms = 2,
    n_table = n_table,
    names_arms = names_arms,
    names_subgroups = names_subgroups
  )

  expect_equal(unname(design[["correlation"]]), corr)

  design[["alpha_spending_f"]] <- NULL
  expect_snapshot_value(design, style = "json2", tolerance = 1e-5)

  # Note that we only care about the correlation, not the bounds, hence the
  # simplistic weights
  # the reason this still takes a while to generate is that
  # gMCPLite::generateWeights() is quite slow with 16 hypotheses
  design <- trial_design(
    arms = 2,
    endpoints = 2,
    subgroups = 3,
    n_table = n_table,
    weights = c(1, rep(0, 15)),
    t = 0.5,
    alpha = 0.025,
    alpha_spending_f = as,
    test_m = diag(16),
    names_arms = names_arms,
    names_endpoints = c("E1", "E2"),
    names_subgroups = names_subgroups
  )

  design[["alpha_spending_f"]] <- NULL
  expect_snapshot_value(design, style = "json2", tolerance = 1e-5)
})
