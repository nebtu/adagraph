test_that("example stays constant", {
  des <- make_example_trial()
  des[["alpha_spending"]] <- NULL
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
    transitions = m,
    alpha_spending = as,
    names = c("H1", "H2", "H3", "H4")
  )
  des_multiarm <- make_example_multiarm()

  #elements that are supposed to be different
  # and alpha_spending, because it's environment can be different
  ignore_trial <- c(
    "alpha_spending",
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
    "alpha_spending",
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
    data.frame(arm = "control", G1 = FALSE, G2 = FALSE, n =  0),
    data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  n = 10),
    data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, n = 20),
    data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  n = 30),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, n = 40),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  n = 50),
    data.frame(arm = "A2",      G1 = FALSE, G2 = FALSE, n = 10),
    data.frame(arm = "A2",      G1 = FALSE, G2 = TRUE,  n = 10),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = FALSE, n = 30),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = TRUE,  n = 10)
  )
  names_arms <- c("A1", "A2")
  names_subgroups <- c("G1", "G2")
  as = function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))

  # Note that we only care about the correlation, not the bounds, hence the
  # simplistic weights
  design <- trial_design(
    arms = 2,
    endpoints = 1,
    subgroups = 2,
    n_table = n_table,
    weights = c(1, rep(0, 5)),
    t = 0.5,
    alpha = 0.025,
    alpha_spending = as,
    transitions = diag(6),
    names_arms = names_arms,
    names_endpoints = "E1",
    names_subgroups = names_subgroups
  )

  corr <- get_subgroup_correlation(
    subgroups = 2,
    arms = 2,
    n_table = n_table,
    names_arms = names_arms,
    names_subgroups = names_subgroups
  )

  expect_equal(unname(design[["correlation"]]), corr)

  design[["alpha_spending"]] <- NULL
  expect_snapshot_value(design, style = "json2", tolerance = 1e-5)

  # Note that we only care about the correlation, not the bounds, hence the
  # simplistic weights
  # the reason this still takes a while to generate is that
  # gMCPLite::generateWeights() is quite slow with 16 hypotheses
  design <- trial_design(
    arms = 2,
    endpoints = 2,
    subgroups = 2,
    n_table = n_table,
    weights = c(1, rep(0, 11)),
    t = 0.5,
    alpha = 0.025,
    alpha_spending = as,
    transitions = diag(12),
    names_arms = names_arms,
    names_endpoints = c("E1", "E2"),
    names_subgroups = names_subgroups
  )

  design[["alpha_spending"]] <- NULL
  expect_snapshot_value(design, style = "json2", tolerance = 1e-5)
})

test_that("naming works as it should", {
  design <- trial_design(
    arms = 2,
    n_control = 50,
    n_arms = c(50, 50),
    weights = c(0.5, 0.5),
    transitions = matrix(c(0, 1, 1, 0), nrow = 2),
    alpha = 0.025
  )
  expect_equal(design$names, c("A1", "A2"))

  design <- trial_design(
    arms = 2,
    n_control = 50,
    n_arms = c(50, 50),
    weights = c(0.5, 0.5),
    transitions = matrix(c(0, 1, 1, 0), nrow = 2),
    alpha = 0.025,
    names = c("B1", "B2")
  )
  expect_equal(design$names, c("B1", "B2"))

  design <- trial_design(
    arms = 2,
    n_control = 50,
    n_arms = c(50, 50),
    weights = c(B1 = 0.5, B2 = 0.5),
    transitions = matrix(c(0, 1, 1, 0), nrow = 2),
    alpha = 0.025,
  )
  expect_equal(design$names, c("B1", "B2"))

  design <- trial_design(
    arms = 2,
    n_control = 50,
    n_arms = c(50, 50),
    weights = c(0.5, 0.5),
    transitions = matrix(
      c(0, 1, 1, 0),
      nrow = 2,
      dimnames = list(c("B1", "B2"))
    ),
    alpha = 0.025,
  )
  expect_equal(design$names, c("B1", "B2"))

  design <- trial_design(
    arms = 2,
    n_control = 50,
    n_arms = c(50, 50),
    weights = c(0.5, 0.5),
    transitions = matrix(c(0, 1, 1, 0), nrow = 2),
    alpha = 0.025,
    names_arms = c("B1", "B2")
  )
  expect_equal(design$names, c("B1", "B2"))
})
