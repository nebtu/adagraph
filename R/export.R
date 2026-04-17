#' Export the closed testing strategy as a gMCPLite graph object
#'
#' Export the original or adapted graph and weights of an adagraph design to an
#' graphMCP object as used by gMCPLite. This allows to use the testing and
#' graphing methods of gMCPLite for the testing strategy.
#'
#' @param design the adagraph_design object that should be exported
#' @param adapted boolean indicating if the adapted version of the testing
#'   strategy should be exported. Defaults to TRUE if an adaption has been made,
#'   FALSE else
#'
#' @return An object with S4 class `graphMCP`, with matrix and weights of the
#'   input design
#' @export
export_gmcp_lite <- function(
  design,
  adapted = design$adaptions
) {
  rlang::check_installed(
    "gMCPLite",
    reason = "The gMCPLite package needs to be installed to export as a gMCPLite object."
  )
  if (adapted) {
    weights <- design[["ad_weights"]]
    m <- design[["ad_test_m"]]
  } else {
    weights <- design[["weights"]]
    m <- design[["test_m"]]
  }

  methods::new(
    "graphMCP",
    m = unname(m),
    weights = unname(weights),
    nodeAttr = as.list(design[["names"]])
  )
}

#' Export the closed testing strategy as a graphicalMCP graph object
#'
#' Export the original or adapted graph and weights of an adagraph design to an
#' graph object as used by graphicalMCP. This allows to use the testing and
#' graphing methods of gMCPLite for the testing strategy.
#'
#' @param design the adagraph_design object that should be exported
#' @param adapted boolean indicating if the adapted version of the testing
#'   strategy should be exported. Defaults to TRUE if an adaption has been made,
#'   FALSE else
#'
#' @return An S3 object of class `initial_graph`, with hypotheses weights and
#'   matrix of the input design
#'
#' @export
#' @examples
#' m <- rbind(
#'   H1 = c(0, 1 / 2, 1 / 2, 0),
#'   H2 = c(1 / 2, 0, 0, 1 / 2),
#'   H3 = c(0, 1, 0, 0),
#'   H4 = c(1, 0, 0, 0)
#' )
#' weights <- c(1 / 2, 1 / 2, 0, 0)
#' as = function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
#' design <- cer_design(
#'   weights = weights,
#'   alpha = 0.05,
#'   test_m = m,
#'   alpha_spending_f = as,
#'   t = 0.5,
#'   correlation = diag(4)
#' )
#'
#' export_graphical_mcp(design)
export_graphical_mcp <- function(
  design,
  adapted = design$adaptions
) {
  rlang::check_installed(
    "graphicalMCP",
    reason = "The graphicalMCP package needs to be installed to export as a graphicalMCP object."
  )
  if (adapted) {
    weights <- design[["ad_weights"]]
    m <- design[["ad_test_m"]]
  } else {
    weights <- design[["weights"]]
    m <- design[["test_m"]]
  }

  graphicalMCP::graph_create(unname(weights), unname(m), design[["names"]])
}
