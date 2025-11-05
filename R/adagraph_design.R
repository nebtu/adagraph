#' Internal function for adagraph_design
#'
#' For documentation on how to generate adagraph_designs, see `adagraph_design()`
#'
#' @param correlation,weights,alpha,test_m Same as for `adagraph_design()`
#' @param class character, makes it possible to add subbclasses
#' @param ... additional parameters, not used
#'
#' @return An object of class "adagraph_design, with the following elements"
#'  * correlation: correlation matrix of the hypotheses, as given
#'  * weights: list of weights of the hypotheses, as given
#'  * alpha: overall FWER, as given
#'  * test_m: transition matrix of the graph, as given
#'  * hyp_matrix: matrix of format #intersection-hypotheses x #hypotheses where
#'      each row specifies which hypothosis is part of the given intersection hypotheses
#'  * weights_matrix: same format as hyp_matrix, but each row specifies
#'      the weights of the hypotheses for the given intersection hypothesis
#'  * closed_matrix: each of the #hypothesis columns specifies which
#'      intersection hypotheses need to be tested to reject the given hypothesis
#' @noRd
new_adagraph_design <- function(
  correlation = matrix(),
  weights = double(),
  alpha = double(),
  test_m = matrix(),
  ...,
  class = character()
) {
  k <- dim(correlation)[1]

  int_hyp <- get_intersection_hypotheses(weights, test_m)
  correlation_components <- gMCPLite:::conn.comp(correlation)

  design <- list(
    correlation = correlation,
    correlation_components = correlation_components,
    weights = weights,
    alpha = alpha,
    hyp_matrix = int_hyp$hyp_matrix,
    weights_matrix = int_hyp$weights_matrix,
    closed_matrix = int_hyp$closed_matrix,
    test_m = test_m,
    interim_test = FALSE,
    adaptions = FALSE,
    final_test = FALSE
  )
  structure(
    design,
    k = k,
    class = c(class, "adagraph_design")
  )
}

#' Internal function for generating intersection hypothesis
#'
#' From given weights and test matrix, generate descriptions of intersection
#' hypotheses in multiple ways
#'
#' Intersection hypothesis in this context means all possible intersection of a
#' given hypothesis sets, which need to be tested at level alpha to guarante the
#' overall FWER according to the closed testing principle.
#'
#' Uses `gMCPLite::generateWeights()` internally
#'
#' @param weights starting weights of the different hypothesis
#' @param test_m transition matrix of the graph used to determine the weights
#'   for the intersection hypothesis
#'
#' @returns a list with the following components:
#'
#' * hyp_matrix: matrix of dimension 2^k-1 x k describing for all 2*(k-1) hypotheses which hypotheses
#'   from 1 to k are included in this intersection hypothesis
#' * weights_matrix: matrix of dimension 2^k-1 x k describing for all 2*(k-1)
#'   hpyotheses what the weight of each hypothesis is for testing this
#'   intersection hypothesis
#' * closed_matrix: matrix of dimension 2^(k-1) * k describing for each
#'   hypothesis (one per column) which intersection hypothesis need to be
#'   rejected (whith numbers to be used for subsetting the previous matrices) to
#'   reject the overall hypothesis
#' @noRd
get_intersection_hypotheses <- function(weights, test_m) {
  k <- length(weights)
  if (k == 1) {
    hyp_matrix <- cbind(1)
    weights_matrix <- cbind(weights)
  } else {
    #generate weights for all sub-hypotheses
    temp <- gMCPLite::generateWeights(test_m, weights)
    hyp_matrix <- temp[, 1:k]
    weights_matrix <- temp[, (k + 1):(2 * k)]
  }
  closed_matrix = matrix(NA, nrow = 2^(k - 1), ncol = k)
  for (i in 1:k) {
    # Fill the result matrix with row indices
    closed_matrix[, i] = which(hyp_matrix[, i] == 1)
  }
  list(
    hyp_matrix = hyp_matrix,
    weights_matrix = weights_matrix,
    closed_matrix = closed_matrix
  )
}


validate_adagraph_design_params <- function(
  correlation = matrix(),
  weights = double(),
  alpha = double(),
  test_m = matrix(),
  call = rlang::caller_env()
) {
  if (!is.matrix(correlation)) {
    cli::cli_abort(
      "correlation has to be a matrix.",
      class = "invalid_argument_correlation"
    )
  } else if (dim(correlation)[1] != dim(correlation)[2]) {
    cli::cli_abort(
      "correlation has to be a quadratic matrix.",
      "x" = "correlation has dimenstion {dim(correlation)[1]} x {dim(correlation)[2]}.",
      class = "invalid_argument_correlation"
    )
  }
  k <- dim(correlation)[1]
  if (length(weights) != k) {
    cli::cli_abort(
      "Need exactly one weight per hypothesis.",
      "i" = "There are {k} hypotheses.",
      "x" = "You suppplied {lenght(weights) weights}.",
      class = "invalid_argument_weights"
    )
  } else if (!is.numeric(alpha)) {
    cli::cli_abort(
      "alpha has to be numeric.",
      "x" = "alpha is {typeof(alpha)}.",
      class = "invalid_argument_alpha"
    )
  } else if (alpha < 0 | alpha > 1) {
    cli::cli_abort(
      "alpha has to be between 0 and 1",
      "x" = "alpha is {alpha}.",
      class = "invalid_argument_alpha"
    )
  } else if (!is.matrix(test_m)) {
    cli::cli_abort(
      "test_m has to be a matrix.",
      class = "invalid_argument_test_m"
    )
  } else if (dim(test_m)[1] != dim(test_m)[2] | dim(test_m)[1] != k) {
    cli::cli_abort(
      "test_m must be a {k}x{k} matrix matching the number of hypotheses",
      "i" = "There are {k} Hypotheses",
      "x" = "test_m has dimenstion {nrow(correlation)} x {ncol(correlation)}.",
      class = "invalid_argument_test_m"
    )
  }
  #TODO: test that weights sum to 1 (or less than one?)
}

#' Make a new (generic) trial design
#'
#' @param correlation Correlation matrix describing the structure of the correlations
#'                    between the different hypotheses, use NA for uncorrelated
#' @param weights List of weights, measuring how important each hypothesis is
#' @param alpha Single number, measuring what total alpha should be spent on the FWER
#' @param test_m Transition matrix describing the graph for the closed test procedure to test the hypotheses
#'
#' @return An object of class adagraph_design
#' @export
#'
#' @examples
#' design <- adagraph_design(
#'  correlation=rbind(H1=c(1, NA),
#'                    H2=c(NA, 1)),
#'  weights=c(2/3, 1/3),
#'  alpha=0.05,
#'  test_m=rbind(c(0, 1),
#'               c(1, 0)))
#' design
adagraph_design <- function(
  correlation = matrix(),
  weights = double(),
  alpha = double(),
  test_m = matrix()
) {
  validate_adagraph_design_params(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = test_m
  )
  new_adagraph_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = test_m
  )
}
