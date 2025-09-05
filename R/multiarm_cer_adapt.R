#' Get the time fractions for given sample sizes
#'
#' Get the information time fractions for the planned interim test for
#' given trial arms, given the sample sizes in treatment and control groups
#'
#' @param n_cont_1,n_cont_2 vector containing sample sizes for control groups in
#'   the first and second stage of the trial
#' @param n_treat_1,n_treat_2 vector containing sample sizes for treatment
#'   groups in the first and second stage of the trial
#'
#' @examples
#' #trivial case with equal ratios throughout
#' get_t(50, 50, 50, 50)
#'
#' #different sample sizes in the second trial stage, but constant allocation ratio
#' get_t(
#'   n_cont_1 = c(50, 50, 50, 50),
#'   n_cont_2 = c(50, 75, 100, 150),
#'   n_treat_1 = c(50, 50, 50, 50),
#'   n_treat_2 = c(50, 75, 100, 150)
#' )
#'
#' #changing allocation ratio in the second stage:
#' # the rate and number of control group patients stays the same, but treatment
#' # group allocation is different
#' get_t(
#'   n_cont_1 = c(50, 50, 50, 50),
#'   n_cont_2 = c(50, 50, 50, 50),
#'   n_treat_1 = c(50, 50, 50, 50),
#'   n_treat_2 = c(25, 50, 100, 150)
#' )
#'
#'@export
get_t <- function(n_cont_1, n_cont_2, n_treat_1, n_treat_2) {
  inv_var_1 <- ifelse(
    n_cont_1 != 0 | n_treat_1 != 0,
    n_treat_1 * n_cont_1 / (n_treat_1 + n_cont_1),
    0
  )

  inv_var_2 <- ifelse(
    n_cont_2 != 0 | n_treat_2 != 0,
    n_treat_2 * n_cont_2 / (n_treat_2 + n_cont_2),
    0
  )

  inv_var_1 / (inv_var_1 + inv_var_2)
}

#' Get the amount of people for each hypotheses in the 2. stage of a trial
#' assuming a fixed population
#'
#' @param n vector of number of people for each arm of the trial
#' @param t time fraction at which the second stage begins
#' @param n1 number of people used in the first stage, instead of t
#' @param which_drop list of arms to drop, by their index in n
#' @param simple_redistribute if the distribution of the dropped arms to the
#'   rest has remainders, wether those remainders are distributed to the largest
#'   ramainders or simply to the first in the list
#'
#' Either t or n1 needs to be specified
#' If $t * n$ is not an integer, the number of people in the first stage is
#' rounded down
#'
#' @return list of number of people in the second stage for all the arms
#'   (including dropped ones)
#' @export
#' @examples
#' redistribute_n(c(100, 100), t = 1/2, which_drop = 1)
redistribute_n <- function(
  n,
  t = NA,
  n1 = NA,
  which_drop = NA,
  simple_redistribute = FALSE
) {
  if (!is.na(t)) {
    n1 <- floor(n * t)
  }
  keep <- !(seq_along(n) %in% which_drop)
  n2 <- n - n1
  if (any(!keep)) {
    weights <- n[keep]

    n_reassign <- sum(n2[!keep])

    quotas <- n_reassign * weights / sum(weights)
    reassign <- floor(quotas)
    remainders <- quotas - reassign

    remaining <- n_reassign - sum(reassign)
    if (remaining > 0) {
      if (simple_redistribute) {
        top_indices <- 1:remaining
      } else {
        top_indices <- order(remainders, decreasing = TRUE)[1:remaining]
      }
      reassign[top_indices] <- reassign[top_indices] + 1
    }
    n2[!keep] <- 0
    n2[keep] <- n2[keep] + reassign
  }
  n2
}

#' Drop arm from a multiarm design
#'
#' This adapts a multiarm design by dropping a treatment arm and adjusting the
#' number of datapoints
#'
#' The weights of the dropped hypotheses are set to 0 and distributed according
#' to the prespecified graph, similarly to `cer_drop_hypotheses()`
#' If specified, the time fraction of the interim test will also be
#' adapted according to the number of data points in the second stage, assuming
#' the first stage n was as planned accoring to t.
#'
#' @param design multiarm_design object
#' @param arms list of with integers specifing which arms should be dropped
#' @param n_cont_2,n_treat_2 list of number of datapoints for the control and
#'   treatment groups
#'   Can be different for different groups. The lenght should be equal to the
#'   number of control/treatment groups, including dropped treatments, which
#'   will always be set to 0
#' @param alt_adj uses `cer_alt_drop_hypotheses()` for dropping the hypotheses
#'   instead of `cer_drop_hypotheses()`
#' @param adapt_bounds Adapt the bounds for rejecting a hypotheses to keep the
#'   FWER with the new adaptions, see `cer_adapt()`
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
#' design <- cer_interim_test(design, c(0.1, 0.02))
#'
#' design <- design |> multiarm_drop_arms(1)
#' design
#'
#' @export
multiarm_drop_arms <- function(
  design,
  arms,
  n_cont_2 = NA,
  n_treat_2 = NA,
  alt_adj = FALSE,
  adapt_bounds = TRUE
) {
  treatment_assoc <- design$treatment_assoc
  keep_arms <- rep(TRUE, attr(design, "k"))
  keep_arms[arms] <- FALSE

  n_treat_1 <- floor(design$n_treat * design$t)
  n_cont_1 <- floor(design$n_controls * design$t)

  if (any(!is.na(n_cont_2)) || any(!is.na(n_treat_2))) {
    t <- get_t(
      n_cont_1[treatment_assoc],
      n_cont_2[treatment_assoc],
      n_treat_1,
      n_treat_2
    )
  } else {
    n_cont_2 <- design$n_controls - n_cont_1
    n_treat_2 <- design$n_treatments - n_treat_1

    t <- design$t
  }

  n_treat_2[arms] <- 0

  design$n_cont_2 <- n_cont_2
  design$n_treat_2 <- n_treat_2
  design$ad_n_treatments <- n_treat_1 + n_treat_2
  design$ad_n_controls <- n_cont_1 + n_cont_2

  if (alt_adj) {
    design <- cer_alt_drop_hypotheses(
      design,
      (seq_along(treatment_assoc) %in% arms),
      adapt_bounds = FALSE
    )
  } else {
    design <- cer_drop_hypotheses(
      design,
      (seq_along(treatment_assoc) %in% arms),
      adapt_bounds = FALSE
    )
  }

  correlation <- get_multiarm_correlation(
    design$controls,
    design$treatment_assoc,
    n_cont_2,
    n_treat_2
  )

  design |>
    cer_adapt(time = t, correlation = correlation, adapt_bounds = adapt_bounds)
}
