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

#' @param adapt_bounds Adapt the bounds for rejecting a hypotheses to keep the
#'   FWER with the new adaptions, see `cer_adapt()`
# for now this assumes that n1, n of the first stage, was like planned according
# to t
# n_cont_2 and n_treat_2 still has the same lenght as before, so dropped arms
# should proabbly, but not automatically have n of 0
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
