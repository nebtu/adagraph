test_that("export to gMCPLite", {
  #from the gMCPLite examples
  #https://merck.github.io/gMCPLite/reference/graphMCP-class.html

  m <- rbind(
    H11 = c(0, 0.5, 0, 0.5, 0, 0),
    H21 = c(1 / 3, 0, 1 / 3, 0, 1 / 3, 0),
    H31 = c(0, 0.5, 0, 0, 0, 0.5),
    H12 = c(0, 1, 0, 0, 0, 0),
    H22 = c(0.5, 0, 0.5, 0, 0, 0),
    H32 = c(0, 1, 0, 0, 0, 0)
  )

  weights <- c(1 / 3, 1 / 3, 1 / 3, 0, 0, 0)

  design <- adagraph_design(
    weights = weights,
    test_m = m,
    alpha = 0.05,
    names = rownames(m)
  )

  graph <- new("graphMCP", m = m, weights = weights)
  expect_equal(export_gmcp_lite(design), graph)
})

test_that("export to gMCPLite on example design", {
  design <- make_example_design()

  graph <- export_gmcp_lite(design)

  expect_equal(gMCPLite::getMatrix(graph), design[["test_m"]])
  expect_equal(gMCPLite::getWeights(graph), design[["weights"]])

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  design_adj <- cer_drop_hypotheses(design, 1)
  graph_adj <- export_gmcp_lite(design_adj, adapted = TRUE)
  expect_equal(gMCPLite::getMatrix(graph_adj), design_adj[["ad_test_m"]])
  expect_equal(gMCPLite::getWeights(graph_adj), design_adj[["ad_weights"]])
})

test_that("export to graphicalMCP", {
  hypotheses <- c(0.5, 0.5, 0, 0)

  transitions <- rbind(
    c(0, 0, 1, 0),
    c(0, 0, 0, 1),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )
  hyp_names <- c("H11", "H12", "H21", "H22")

  design <- adagraph_design(
    weights = hypotheses,
    test_m = transitions,
    names = hyp_names,
    alpha = 0.05
  )

  g <- graphicalMCP::graph_create(hypotheses, transitions, hyp_names)

  expect_equal(export_graphical_mcp(design), g)
})

test_that("export to graphicalMCP on example design", {
  design <- make_example_design()

  graph <- export_graphical_mcp(design)

  expect_equal(graph[["transitions"]], design[["test_m"]])
  expect_equal(graph[["hypotheses"]], design[["weights"]])

  design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

  design_adj <- cer_drop_hypotheses(design, 1)
  graph_adj <- export_graphical_mcp(design_adj, adapted = TRUE)
  expect_equal(graph_adj[["transitions"]], design_adj[["ad_test_m"]])
  expect_equal(graph_adj[["hypotheses"]], design_adj[["ad_weights"]])
})
