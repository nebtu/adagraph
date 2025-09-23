#' Internal function for multiarm_cer_design
#'
#' For documentation on how to generate multiarm_cer_designs, see [multiarm_cer_design()].
#' The parameters for this function are mostly the same as in [multiarm_cer_design()]
#' Note however that all parameters have to already expanded to vectors if
#' possible
#'
#' @param controls,treatment_assoc,n_controls,n_treatments,weights,t,alpha,test_m,alpha_spending_f,seq_bonf Same as for [multiarm_cer_design()]
#' @param class character, makes it possible to add subclasses
#' @param ... additional parameters, not used
#'
#' @return An object of class c("multiarm_cer_design", "cer_design", "adagraph_design")
#' @noRd
new_multiarm_cer_design <- function(
  controls = integer(),
  treatment_assoc = integer(),
  n_controls = integer(),
  n_treatments = integer(),
  weights = double(),
  t = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {},
  seq_bonf = TRUE,
  ...,
  class = character()
) {
  correlation <- get_multiarm_correlation(
    controls,
    treatment_assoc,
    n_controls,
    n_treatments
  )

  design <- new_cer_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t,
    class = c(class, "multiarm_cer_design")
  )

  design$controls <- controls
  design$treatment_assoc <- treatment_assoc
  design$n_controls <- n_controls
  design$n_treatments <- n_treatments

  design
}

#' Internal function for generating a correlation matrix for a given multiarm design
#'
#' When two arms of a given multiarm design share a control group, we assume
#' that their resulting p-values will be correlated. This function calculates
#' this correlation.
#'
#' @param controls,treatment_assoc,n_controls,n_treatments As provided to
#'   `multiarm_cer_design()`
#'
#' @return A quadratic correlation matrix with dimensions of
#'   length(treatment_assoc), detailing the correlation resulting from shared
#'   controls
#' @noRd
get_multiarm_correlation <- function(
  controls = integer(),
  treatment_assoc = integer(),
  n_controls = integer(),
  n_treatments = integer()
) {
  k <- length(treatment_assoc)
  # Ensure sample sizes are vectors
  if (length(n_controls) == 1) {
    n_controls <- rep(n_controls, controls)
  }
  if (length(n_treatments) == 1) {
    n_treatments <- rep(n_treatments, k)
  }

  correlation <- matrix(NA, nrow = k, ncol = k)
  for (i in 1:controls) {
    n_con <- n_controls[i]
    treats <- (treatment_assoc == i)
    n_treat <- n_treatments[treats]

    si <- sqrt(1 / n_con + 1 / n_treat)
    correlation[treats, treats] <- (1 / n_con) / outer(si, si)
  }
  diag(correlation) <- 1

  correlation
}

validate_multiarm_cer_design_params <- function(
  controls = integer(),
  treatment_assoc = integer(),
  n_controls = integer(),
  n_treatments = integer(),
  weights = double(),
  t = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {},
  seq_bonf = TRUE,
  call = rlang::caller_env()
) {
  # Ensure sample sizes are expanded to vectors for correlation calculation

  if (!is.numeric(controls) || controls <= 0 || controls != round(controls)) {
    cli::cli_abort(
      "controls must be a positive integer.",
      "x" = "controls is {controls}.",
      class = "invalid_argument_controls"
    )
  }

  if (
    !is.numeric(treatment_assoc) ||
      any(treatment_assoc <= 0) ||
      any(treatment_assoc != round(treatment_assoc))
  ) {
    cli::cli_abort(
      "treatment_assoc must be a vector of positive integers.",
      class = "invalid_argument_treatment_assoc"
    )
  }

  if (any(treatment_assoc > controls)) {
    cli::cli_abort(
      "All values in treatment_assoc must be <= controls.",
      "i" = "There are {controls} control groups.",
      "x" = "treatment_assoc contains values > {controls}.",
      class = "invalid_argument_treatment_assoc"
    )
  }

  if (!is.numeric(n_controls) || any(n_controls <= 0)) {
    cli::cli_abort(
      "n_controls must be a positive number or vector of positive numbers.",
      class = "invalid_argument_n_controls"
    )
  }

  if (!is.numeric(n_treatments) || any(n_treatments <= 0)) {
    cli::cli_abort(
      "n_treatments must be a positive number or vector of positive numbers.",
      class = "invalid_argument_n_treatments"
    )
  }
  correlation <- get_multiarm_correlation(
    controls,
    treatment_assoc,
    n_controls,
    n_treatments
  )
  validate_cer_design_params(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t,
    seq_bonf = seq_bonf,
    call = call
  )
}

#' Make a new multiarm CER trial design
#'
#' Returns an object of class `multiarm_cer_design`.
#' This can be used for clinical trials with multiple arms and control groups
#' that might have adaptations controlled for using the conditional error method.
#'
#' @param controls Number of control groups
#' @param treatment_assoc Vector of integer, corresponding to the number of the
#'   control group for each treatment group. The length determines the number of
#'   treatment groups
#' @param n_controls Integer (or vector of integers) determining the number of
#'   patients in each control group
#' @param n_treatments Integer (or vector of integers) determining the number of
#'   patients in each treatment group
#' @param weights List of weights, measuring how important each hypothesis is
#' @param t information fraction, at which fraction of assigned people will the
#'   interim analysis happen
#' @param alpha Single number, measuring what total alpha should be spent on the FWER
#' @param test_m Transition matrix describing the graph for
#'   the closed test procedure to test the hypotheses
#' @param alpha_spending_f alpha spending function, taking parameters
#'   alpha (for overall spent alpha) and t (information fraction at interim test)
#' @param seq_bonf automatically reject hypotheses at the second stage
#'   if the sum of their PCER is greater 1
#'
#' @return An object of class `multiarm_cer_design`
#' @export
#'
#' @examples
#' as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))
#' design <- multiarm_cer_design(
#'  controls = 1,
#'  treatment_assoc = c(1,1),
#'  n_controls = 50,
#'  n_treatments = 50,
#'  weights = c(0.5, 0.5),
#'  alpha = 0.05,
#'  test_m = rbind(c(0, 1),
#'               c(1, 0)),
#'  alpha_spending_f = as,
#'  t = 0.5)
#'
#' design
multiarm_cer_design <- function(
  controls = integer(),
  treatment_assoc = integer(),
  n_controls = integer(),
  n_treatments = integer(),
  weights = double(),
  t = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {},
  seq_bonf = TRUE
) {
  if (length(n_controls) == 1) {
    n_controls <- rep(n_controls, controls)
  }
  if (length(n_treatments) == 1) {
    n_treatments <- rep(n_treatments, length(treatment_assoc))
  }
  validate_multiarm_cer_design_params(
    controls = controls,
    treatment_assoc = treatment_assoc,
    n_controls = n_controls,
    n_treatments = n_treatments,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    seq_bonf = seq_bonf
  )

  new_multiarm_cer_design(
    controls = controls,
    treatment_assoc = treatment_assoc,
    n_controls = n_controls,
    n_treatments = n_treatments,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    seq_bonf = seq_bonf
  )
}
