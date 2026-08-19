test_that("specifying graph for different designs works for gMCPLite", {
  m <- rbind(
    H1 = c(0, 1 / 2, 1 / 2, 0),
    H2 = c(1 / 2, 0, 0, 1 / 2),
    H3 = c(0, 1, 0, 0),
    H4 = c(1, 0, 0, 0)
  )
  weights <- c(1 / 2, 1 / 2, 0, 0)
  correlation = matrix(rep(1 / 2, 16), nrow = 4) + 1 / 2 * diag(4)
  correlation[1:2, 3:4] = NA
  correlation[3:4, 1:2] = NA
  diag(correlation) = 1
  t = 0.5
  alpha = 0.025

  graph <- new("graphMCP", m = m, weights = weights)

  des <- adagraph_design(
    weights,
    m,
    alpha = alpha,
    correlation = correlation
  )
  des_graph <- adagraph_design(
    graph = graph,
    alpha = alpha,
    correlation = correlation
  )
  expect_equal(des, des_graph)

  #spending function
  as = function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
  #========

  des <- make_example_design()
  des_graph <- cer_design(
    graph = graph,
    alpha = alpha,
    correlation = correlation,
    alpha_spending = as
  )
  des[["alpha_spending"]] <- NULL
  des_graph[["alpha_spending"]] <- NULL
  expect_equal(des, des_graph)

  des <- make_example_multiarm()
  des_graph = multiarm_cer_design(
    controls = 2,
    treatment_assoc = c(1, 1, 2, 2),
    n_controls = 70,
    n_treatments = 70,
    t = t,
    graph = graph,
    alpha = alpha,
    alpha_spending = as
  )
  des[["alpha_spending"]] <- NULL
  des_graph[["alpha_spending"]] <- NULL
  expect_equal(des, des_graph)

  n_table <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, n = 80, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, n = 80, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, n = 80, check.names = FALSE)
  )
  names_arms <- c("arm1", "arm2")
  names_subgroups <- "HPV+"
  names_endpoints <- c("prim", "sec")
  names <- c(
    "prim_arm1",
    "prim_arm2",
    "sec_arm1",
    "sec_arm2",
    "HPV+_prim_arm1",
    "HPV+_prim_arm2",
    "HPV+_sec_arm1",
    "HPV+_sec_arm2"
  )

  alpha <- 0.025
  t <- 0.5
  weights <- c(0.35, 0.35, 0, 0, 0.15, 0.15, 0, 0)

  sec_to_prim <- 1 / 3
  inter_prim <- 0.2
  prim_to_sec <- 0.4

  #fmt: skip
  transitions <- matrix(
    byrow = TRUE,
    ncol = 8,
    data = c(
              0,   inter_prim, prim_to_sec,          0,  inter_prim, inter_prim,           0,           0,
    inter_prim ,            0,           0,prim_to_sec,  inter_prim, inter_prim,           0,           0,
              0,  sec_to_prim,           0,          0, sec_to_prim,sec_to_prim,           0,           0,
    sec_to_prim,            0,           0,          0, sec_to_prim,sec_to_prim,           0,           0,
    inter_prim ,   inter_prim,           0,          0,           0, inter_prim, prim_to_sec,           0,
    inter_prim ,   inter_prim,           0,          0,  inter_prim,          0,           0, prim_to_sec,
    sec_to_prim,  sec_to_prim,           0,          0,           0,sec_to_prim,           0,           0,
    sec_to_prim,  sec_to_prim,           0,          0, sec_to_prim,          0,           0,            0
  ))
  dimnames(transitions) <- list(names, names)

  des <- make_example_trial()

  graph <- new("graphMCP", m = transitions, weights = weights)

  des_graph <- trial_design(
    arms = 2,
    endpoints = 2,
    subgroups = 1,
    n_table = n_table,
    graph = graph,
    t = t,
    alpha = alpha,
    alpha_spending = function(x, t) {
      2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
    },
    names_arms = names_arms,
    names_subgroups = names_subgroups,
    names_endpoints = names_endpoints
  )
  des[["alpha_spending"]] <- NULL
  des_graph[["alpha_spending"]] <- NULL
  expect_equal(des, des_graph)
})

test_that("specifying graph for different designs works for graphicalMCP", {
  m <- rbind(
    H1 = c(0, 1 / 2, 1 / 2, 0),
    H2 = c(1 / 2, 0, 0, 1 / 2),
    H3 = c(0, 1, 0, 0),
    H4 = c(1, 0, 0, 0)
  )
  weights <- c(1 / 2, 1 / 2, 0, 0)
  correlation = matrix(rep(1 / 2, 16), nrow = 4) + 1 / 2 * diag(4)
  correlation[1:2, 3:4] = NA
  correlation[3:4, 1:2] = NA
  diag(correlation) = 1
  t = 0.5
  alpha = 0.025

  graph <- graphicalMCP::graph_create(
    weights,
    m
  )

  des <- adagraph_design(
    weights,
    m,
    alpha = alpha,
    correlation = correlation
  )
  des_graph <- adagraph_design(
    graph = graph,
    alpha = alpha,
    correlation = correlation
  )
  expect_equal(des, des_graph)

  #spending function
  as = function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
  #========

  des <- make_example_design()
  des_graph <- cer_design(
    graph = graph,
    alpha = alpha,
    correlation = correlation,
    alpha_spending = as
  )
  des[["alpha_spending"]] <- NULL
  des_graph[["alpha_spending"]] <- NULL
  expect_equal(des, des_graph)

  des <- make_example_multiarm()
  des_graph = multiarm_cer_design(
    controls = 2,
    treatment_assoc = c(1, 1, 2, 2),
    n_controls = 70,
    n_treatments = 70,
    t = t,
    graph = graph,
    alpha = alpha,
    alpha_spending = as
  )
  des[["alpha_spending"]] <- NULL
  des_graph[["alpha_spending"]] <- NULL
  expect_equal(des, des_graph)

  n_table <- rbind(
    data.frame(arm = "control", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "control", `HPV+` = TRUE, n = 80, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "arm1", `HPV+` = TRUE, n = 80, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = FALSE, n = 120, check.names = FALSE),
    data.frame(arm = "arm2", `HPV+` = TRUE, n = 80, check.names = FALSE)
  )
  names_arms <- c("arm1", "arm2")
  names_subgroups <- "HPV+"
  names_endpoints <- c("prim", "sec")
  names <- c(
    "prim_arm1",
    "prim_arm2",
    "sec_arm1",
    "sec_arm2",
    "HPV+_prim_arm1",
    "HPV+_prim_arm2",
    "HPV+_sec_arm1",
    "HPV+_sec_arm2"
  )

  alpha <- 0.025
  t <- 0.5
  weights <- c(0.35, 0.35, 0, 0, 0.15, 0.15, 0, 0)

  sec_to_prim <- 1 / 3
  inter_prim <- 0.2
  prim_to_sec <- 0.4

  #fmt: skip
  transitions <- matrix(
    byrow = TRUE,
    ncol = 8,
    data = c(
              0,   inter_prim, prim_to_sec,          0,  inter_prim, inter_prim,           0,           0,
    inter_prim ,            0,           0,prim_to_sec,  inter_prim, inter_prim,           0,           0,
              0,  sec_to_prim,           0,          0, sec_to_prim,sec_to_prim,           0,           0,
    sec_to_prim,            0,           0,          0, sec_to_prim,sec_to_prim,           0,           0,
    inter_prim ,   inter_prim,           0,          0,           0, inter_prim, prim_to_sec,           0,
    inter_prim ,   inter_prim,           0,          0,  inter_prim,          0,           0, prim_to_sec,
    sec_to_prim,  sec_to_prim,           0,          0,           0,sec_to_prim,           0,           0,
    sec_to_prim,  sec_to_prim,           0,          0, sec_to_prim,          0,           0,            0
  ))

  des <- make_example_trial()

  graph <- graphicalMCP::graph_create(
    weights,
    transitions,
    hyp_names = names
  )
  des_graph <- trial_design(
    arms = 2,
    endpoints = 2,
    subgroups = 1,
    n_table = n_table,
    graph = graph,
    t = t,
    alpha = alpha,
    alpha_spending = function(x, t) {
      2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
    },
    names_arms = names_arms,
    names_subgroups = names_subgroups,
    names_endpoints = names_endpoints
  )
  des[["alpha_spending"]] <- NULL
  des_graph[["alpha_spending"]] <- NULL
  expect_equal(des, des_graph)
})
