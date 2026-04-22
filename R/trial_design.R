#' Internal function for trial_design
#'
#' For documentation on how to generate trial_design objects, see [trial_design()].
#' The parameters for this function are mostly the same as in [trial_design()]
#' Note however that all parameters have to already be expanded to vectors if
#' possible. Additionally, only n_table is allowed for giving the
#' correlation structure for arms and subgroups
#'
#' @param
#' arms,endpoints,,weights,t,alpha,test_m,alpha_spending_f,seq_bonf,names
#' Same as for [trial_design()]
#' @param class character, makes it possible to add subclasses
#' @param ... additional parameters, not used
#'
#' @return An object of class c("multiarm_cer_design", "cer_design", "adagraph_design")
#' @noRd
new_trial_design <- function(
  arms = integer(),
  endpoints = integer(),
  subgroups = integer(),
  n_table = data.frame(),
  weights = double(),
  test_m = matrix(),
  alpha = double(),
  alpha_spending_f = function() {},
  t = double(),
  seq_bonf = TRUE,
  names_arms = NULL,
  names_endpoints = NULL,
  names_subgroups = NULL,
  names = NULL,
  ...,
  class = character()
) {
  correlation <- get_trial_correlation(
    arms,
    endpoints,
    subgroups,
    n_table,
    names_arms,
    names_subgroups
  )

  hyp_assoc <- data.frame(
    group = rep(c("Total", names_subgroups), each = arms * endpoints),
    endpoint = rep(names_endpoints, each = arms, times = subgroups + 1),
    arm = rep(names_arms, times = (subgroups + 1) * endpoints)
  )

  if (is.null(names)) {
    if (endpoints == 1) {
      name_part_endpoints <- ""
    } else {
      name_part_endpoints <- paste0(
        hyp_assoc[["endpoint"]],
        ifelse(arms > 1, "_", "")
      )
    }
    if (arms == 1) {
      name_part_arms <- ""
    } else {
      name_part_arms <-
        hyp_assoc[["arm"]]
    }
    names <- paste0(
      ifelse(
        hyp_assoc[["group"]] == "Total",
        "",
        paste0(hyp_assoc[["group"]], ifelse(arms > 1 || subgroups > 1, "_", ""))
      ),
      name_part_endpoints,
      name_part_arms
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
    class = c(class, "trial_design")
  )

  design$subgroups <- subgroups
  design$arms <- arms
  design$endpoints <- endpoints
  design$n_table <- n_table
  design$names_arms <- names_arms
  design$names_endpoints <- names_endpoints
  design$names_subgroups <- names_subgroups
  design$hyp_assoc <- hyp_assoc

  design
}

get_trial_correlation <- function(
  arms,
  endpoints,
  subgroups,
  n_table,
  names_arms,
  names_subgroups
) {
  correlation_endpoint <- get_subgroup_correlation(
    subgroups,
    arms,
    n_table,
    names_arms,
    names_subgroups
  )

  # We now need to combine the correlation into a complete correlation matrix
  # for all endpoints. The correlation inside each endpoint is given by
  # correlation_endpoint, and across different endpoints it is not specified.
  # However, the blocks don't stay together, since they the structure is
  # (groups, endpoints, arms). The operation is therefore as follows:
  # Given the correlation_endpoint matrix (where each A is a block matrix with
  # the different arms)
  #
  #           Total   G1
  #   Total  [ A11  A12 ]
  #    G1    [ A21  A22 ]
  #
  # We produce for two endpoints the matrix
  #
  #                     Total       G1
  #                   E1   E2    E1   E2
  #   Tot-    E1   [ A11   NA | A12   NA ]
  #    al     E2   [  NA  A11 |  NA  A12 ]
  #                [ ---------+--------- ]
  #    G      E1   [ A21   NA | A22   NA ]
  #    1      E2   [  NA  A21 |  NA  A22 ]
  #
  k <- arms * endpoints * (subgroups + 1)
  correlation <- matrix(NA_real_, k, k)

  for (ep in seq_len(endpoints)) {
    # idx <- unlist(lapply(1:(subgroups + 1), \(g) {
    #   ((g - 1) * (subgroups + 1) + (ep - 1)) * arms + 1:arms
    # }))
    idx <- unlist(lapply(1:(subgroups + 1), \(g) {
      (g - 1) * (arms * endpoints) + (ep - 1) * arms + 1:arms
    }))

    correlation[idx, idx] <- correlation_endpoint
  }

  correlation
}

validate_trial_design_params <- function(
  arms = integer(),
  endpoints = integer(),
  subgroups = integer(),
  n_control = integer(),
  n_arms = integer(),
  n_table = integer(),
  weights = double(),
  t = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {},
  seq_bonf = TRUE,
  names_arms = NULL,
  names_endpoints = NULL,
  names_subgroups = NULL,
  names = names,
  call = rlang::caller_env()
) {}

#' Make a new trial design with multiple arms, endpoints or subgroups
#'
#' Returns an object of class `trial_design`, representing a two-stage
#' clinical trial design, allowing adaptation using the CER method
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
#' To calculate the subgroup and arm correlation structure, subgroups can be provided
#' either as proportions or patient numbers.
#' Note that when using the n_control and n_arm, this is translated to the
#' following structure internally as well.
#' The structure (in argument n_table) should be given as a dataframe, where
#' each row specifies a specific subgroup in a specific arm (or the control).
#' Therefore the first column should be named arm, and have values of
#' "control" and the names of the arms. Then should be columns for each of the
#' subgroups (using the subgroup name as a column name), with logical values,
#' specifying the exact combination of subgroups that are being specified. The
#' last column should be either names 'n' (for case numbers) or 'prop' (for
#' proportions) and give the given value for this exact intersection of
#' subgroups.
#' Those are the numbers/proportions for the first stage. Assuming that the
#' proprortions do not stay exactly the same in the second stage, adapting for
#' the new proportions is necessary.
#' The pre-planned test without adaptations assumes that the group
#' and arm patient numbers stay the same relative to each other.
#' The size of the second trial in comparison to the first is determined by t.
#'
#' @param arms Number of arms
#' @param endpoints Number of endpoints
#' @param subgroups Number of subgroups in addition to the whole collective (can be 0)
#' @param n_control Integer determining the number of
#'   patients in the control group, if there are no subgroups
#' @param n_arms Integer (or vector of integers) determining the number of
#'   patients in each arm, if there are no subgroups
#' @param n_table A data.frame specifying the structure of the subgroups,
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
#' @param names_arms names for the different arms. If not provided, they will be
#'   automatically generated to A1, A2, etc.
#' @param names_endpoints names for the different endpoints If not provided, they will be
#'   automatically generated to E1, E2, etc.
#' @param names_subgroups names for the different subgroups. If not provided, they will be
#'   automatically generated to G1, G2, etc.
#' @param names optional names for the hypotheses. If not provided, they are
#'   generated from the names of the endpoints, arms and subgroups
#'
#' @return An object of class `trial_design`
#' @export
#'
#' @examples
#' as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))
#'
#' m <- rbind(
#'   H1 = c(0, 1 / 2, 1 / 2, 0),
#'  H2 = c(1 / 2, 0, 0, 1 / 2),
#'   H3 = c(0, 1, 0, 0),
#'   H4 = c(1, 0, 0, 0)
#' )
#' design <- trial_design(
#'  arms = 2,
#'  endpoints = 2,
#'  n_control = 50,
#'  n_arms = c(50, 50),
#'  weights = c(0.5, 0.5, 0, 0), #all weight is at first on the first endpoint,
#'                                #on both arms equally
#'  alpha = 0.05,
#'  test_m = m,
#'  alpha_spending_f = as,
#'  t = 0.5
#' )
#'
#' design
trial_design <- function(
  arms = 1,
  endpoints = 1,
  subgroups = 0,
  n_control = NULL,
  n_arms = NULL,
  n_table = NULL,
  weights = double(),
  test_m = matrix(),
  alpha = double(),
  alpha_spending_f = function() {},
  t = 1 / 2,
  seq_bonf = TRUE,
  names_arms = NULL,
  names_endpoints = NULL,
  names_subgroups = NULL,
  names = NULL
) {
  if (length(n_arms) == 1) {
    n_arms <- rep(n_arms, arms)
  }

  if (is.null(names_arms)) {
    if (!is.null(names(n_arms))) {
      names_arms <- names(n_arms)
    } else {
      names_arms <- paste0("A", 1:arms)
    }
  }

  if (is.null(names_endpoints)) {
    names_endpoints <- paste0("E", 1:endpoints)
  }

  if (is.null(names_subgroups)) {
    if (subgroups == 0) {
      names_subgroups <- character(0)
    } else {
      names_subgroups <- paste0("G", 1:subgroups)
    }
  }

  validate_trial_design_params(
    arms = arms,
    endpoints = endpoints,
    subgroups = subgroups,
    n_control = n_control,
    n_arms = n_arms,
    n_table = n_table,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    seq_bonf = seq_bonf,
    names_arms = names_arms,
    names_endpoints = names_endpoints,
    names_subgroups = names_subgroups,
    names = names
  )

  if (!is.null(n_control) || !is.null(n_arms)) {
    if (subgroups != 0) {
      cli::cli_warn(
        "The {.arg n_control} and {.arg n_arms} arguments are only supported for designs without subgroups."
      )
    } else if (!is.null(n_table)) {
      cli::cli_warn(
        "{.arg n_control} and {.arg n_arms} are ignored since {.arg n_table} was supplied"
      )
    } else {
      n_table <- data.frame(
        arm = c("control", names_arms),
        n = c(n_control, n_arms)
      )
    }
  }

  new_trial_design(
    arms = arms,
    endpoints = endpoints,
    subgroups = subgroups,
    n_table = n_table,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    seq_bonf = seq_bonf,
    names_arms = names_arms,
    names_endpoints = names_endpoints,
    names_subgroups = names_subgroups,
    names = names
  )
}
