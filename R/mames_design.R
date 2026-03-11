#' Internal function for mames_design
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
new_mames_design <- function(
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

#' Make a new MAME CER trial design
#'
#' Returns an object of class `mames_design`. This depicts a clinical trial with
#' multiple arms, multiple endpoints, and 2 stages.
#'
#' @param n_arms Number of arms in the trial
#' @param n_endpoints Number of endpoints in the trial
#' @param n_controls Number of patients in the (shared) control group
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
