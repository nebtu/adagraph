#' @export
export_gmcp_lite <- function(
  design,
  adapted = design$adaptions
) {
  if (adapted) {
    weights <- design[["ad_weights"]]
    m <- design[["ad_test_m"]]
  } else {
    weights <- design[["weights"]]
    m <- design[["test_m"]]
  }

  new(
    "graphMCP",
    m = m,
    weights = weights,
    nodeAttr = as.list(design[["names"]])
  )
}

#' @export
export_graphical_mcp <- function(
  design,
  adapted = design$adaptions
) {
  if (adapted) {
    weights <- design[["ad_weights"]]
    m <- design[["ad_test_m"]]
  } else {
    weights <- design[["weights"]]
    m <- design[["test_m"]]
  }

  graphicalMCP::graph_create(weights, m, design[["names"]])
}
