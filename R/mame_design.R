#' Internal function for mame_design
#'
#' For documentation on how to generate mame_design objects, see [mame_design()].
#' The parameters for this function are mostly the same as in [mame_design()]
#' Note however that all parameters have to already be expanded to vectors if
#' possible
#'
#' @param
#' arms,endpoints,n_control,n_treatments,weights,t,alpha,test_m,alpha_spending_f,seq_bonf,names
#' Same as for [mame_design()]
#' @param class character, makes it possible to add subclasses
#' @param ... additional parameters, not used
#'
#' @return An object of class c("multiarm_cer_design", "cer_design", "adagraph_design")
#' @noRd
new_mame_design <- function(
  arms = integer(),
  endpoints = integer(),
  subgroups = integer(),
  n_control = integer(),
  n_treatments = integer(),
  n_subgroups = data.frame(),
  weights = double(),
  t = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {},
  seq_bonf = TRUE,
  names_arms = NULL,
  names_endpoints = NULL,
  names_subgroups = NULL,
  names = NULL,
  ...,
  class = character()
) {
  if (is.null(names_arms)) {
    if (!is.null(names(n_treatments))) {
      names_arms <- names(n_treatments)
    } else {
      names_arms = paste0("A", 1:arms)
    }
  }

  if (is.null(names_endpoints)) {
    names_endpoints = paste0("E", 1:arms)
  }

  if (is.null(names_subgroups)) {
    names_subgroups = paste0("G", 1:arms)
  }

  #correlation inside one endpoint
  correlation_endpoint <- get_multiarm_correlation(
    controls = 1,
    treatment_assoc = rep(1:endpoints, each = arms),
    n_control,
    n_treatments
  )

  if (subgroups == 0) {
    # between different endpoints, correlation is unknown
    diag_na <- ifelse(diag(endpoints) == 1, 1, NA)
    correlation <- kronecker(diag_na, correlation_endpoint)
  } else {
    correlation_subgroups <- get_subgroup_correlation(
      subgroups,
      arms,
      n_subgroups,
      names_arms,
      correlation_endpoint
    )
  }

  design <- new_cer_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t,
    names = names,
    class = c(class, "mame_design")
  )

  design
}

validate_multiarm_cer_design_params <- function(
  arms = integer(),
  endpoints = integer(),
  n_control = integer(),
  n_treatments = integer(),
  weights = double(),
  t = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {},
  seq_bonf = TRUE,
  names = names,
  call = rlang::caller_env()
) {}

#' Make a new MAME trial design
#'
#' Returns an object of class `mame_design`, representing a multi-arm
#' multi-endpoint clinical trial design, using the CER method
#'
#' The ordering of of the hypothesis is automatically determined in the
#' following way: All hypotheses associated with on endpoint are grouped
#' together, so first all arms with their first endpoint, than all arms with the
#' second endpoint, etc. If there are multiple subgroups, these subgroups follow
#' (with the same structure) after the full population.
#'
#' If no names for some structure are provided, they are determined
#' automatically. For endpoints, the default names are E1, E2, ..., for arms,
#' they are A1, A2, ... and for subgroups G1, G2, .. are used.
#'
#' To provide the subgroup correlation structure, subgroup structure can be provided
#' either as proportions or case number either with this function or when doing
#' the interim test.
#' The structure (in argument n_subgroups) should be given as a dataframe, where
#' each row specifies a specific subgroup in a specific arm (or the control).
#' Therefore the first column should be names arm, and have values of
#' "control" and the names of the arms. Then should be columns for each of the
#' subgroups (using the subgroup name as a column name), with logical values,
#' specifying the exact combination of subgroups that are being specified. The
#' last column should be either names 'n' (for case numbers) or 'prop' (for
#' proportions) and give the given value for this exact intersection of
#' subgroups.
#' A dataframe with this structure can also be generated with the helper
#' function [get_n_subgroup()].
#'
#' @param arms Number of arms
#' @param endpoints Number of endpoints
#' @param n_control Integer determining the number of
#'   patients in the control group
#' @param n_treatments Integer (or vector of integers) determining the number of
#'   patients in each treatment group
#' @param n_subgroups A data.frame specifying the structure of the subgroups,
#'   see details for more information
#' @param weights List of weights, measuring how important each hypothesis is.
#'   See details for numbering of hypotheses
#' @param t information fraction, at which fraction of assigned people will the
#'   interim analysis happen
#' @param alpha Single number, measuring what total alpha should be spent on the FWER
#' @param test_m Transition matrix describing the graph for
#'   the closed test procedure to test the hypotheses
#' @param alpha_spending_f alpha spending function, taking parameters
#'   alpha (for overall spent alpha) and t (information fraction at interim test)
#' @param seq_bonf automatically reject hypotheses at the second stage
#'   if the sum of their PCER is greater 1
#' @param names optional names for the hypotheses. IF NULL, the names are of the
#'   form E1_A1 for enpoint 1, arm 1, etc.
#'
#' @return An object of class `mame_design`
#' @export
#'
#' @examples
#' as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))
#' design <- mames_cer_design(
#'  arms = 2,
#'  enpoints = 2,
#'  n_control = 50,
#'  n_treatments = c(50, 50),
#'  weights = c(0.5, 0.5, 0, 0), #all weight is at first on the first endpoint,
#'                                #on both arms equally
#'  alpha = 0.05,
#'  test_m = rbind(c(0, 1),
#'               c(1, 0)),
#'  alpha_spending_f = as,
#'  t = 0.5)
#'
#' design
mame_design <- function(
  arms = 1,
  endpoints = 1,
  subgroups = 0,
  n_control = integer(),
  n_treatments = integer(),
  n_subgroups = data.frame(),
  weights = double(),
  t = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {},
  seq_bonf = TRUE,
  names = NULL
) {
  if (length(n_treatments) == 1) {
    n_treatments <- rep(n_treatments, length())
  }
  validate_mame_design_params(
    arms = arms,
    endpoints = endpoints,
    n_control = n_control,
    n_treatments = n_treatments,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    seq_bonf = seq_bonf,
    names = names
  )

  new_multiarm_design(
    arms = arms,
    endpoints = endpoints,
    n_control = n_control,
    n_treatments = n_treatments,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    seq_bonf = seq_bonf,
    names = names
  )
}
