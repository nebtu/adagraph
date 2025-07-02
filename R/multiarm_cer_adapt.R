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

multiarm_drop_arms <- function(
  design,
  arms,
  redistribute_n = TRUE,
  alt_testing = FALSE,
  simple_redistribute = FALSE
) {
  treatment_assoc <- design$treatment_assoc
  keep_arms <- rep(TRUE, attr(design, "k"))
  keep_arms[arms] <- FALSE

  # Since we assume that n_treat and n_cont accumulate proportional,
  # the amount of assumed n is proportional as well
  n_cont_1 <- floor(design$t * design$n_controls)
  n_cont_2 <- design$n_control - n_cont_1

  n_treat_1 <- floor(design$t * design$n_treatments)
  n_treat_2 <- design$n_treatments - n_treat_1

  if (redistribute_n) {
    # boolean vector indicating which control groups are still needed
    keep_cont <- 1:design$controls %in% treatment_assoc[keep_arms]

    weigths_cont <- design$n_controls[keep_cont]
    weigths_treat <- design$n_treatments[keep_arms]

    #amount of people to be reassigned
    n_reassign <- sum(n_treat_2[!keep_arms]) + sum(n_cont_2[!keep_cont])

    # how many people are reassigned to c(kept_conts, kept_treats)
    quotas <- n_reassign *
      (c(weigths_cont, weigths_treat) / sum(c(weigths_cont, weigths_treat)))
    reassign <- floor(quotas)
    remainders <- quotas - reassign

    # assign rounding errors to the ones with highest remainder
    remaining <- n_reassign - sum(reassign)
    if (remaining > 0) {
      if (simple_redistribute) {
        top_indices <- remainders[1:remaining]
      } else {
        top_indices <- order(remainders, decreasing = TRUE)[1:remaining]
      }
      remainders[top_indices] <- remainders[top_indices] + 1
    }

    n_cont_2[keep_cont] <- n_cont_2[keep_cont] +
      reassign[seq_along(n_cont_2)]
    n_treat_2[keep_arms] <- n_treat_2[keep_arms] +
      reassign[-seq_along(n_cont_2)]

    n_cont_2[!keep_cont] <- 0
    n_treat_2[!keep_arms] <- 0

    t <- get_t(
      n_cont_1[treatment_assoc],
      n_cont_2[treatment_assoc],
      n_treat_1,
      n_treat_2
    )
  } else {
    n_cont_2[!keep_cont] <- 0
    n_treat_2[!keep_arms] <- 0

    t[!keep_arms] <- 1
  }

  design$n_cont_2 <- n_cont_2
  design$n_treat_2 <- n_treat_2
  design$ad_n_treatments <- n_treat_1 + n_treat_2
  design$ad_n_controls <- n_cont_1 + n_cont_2

  if (alt_testing) {
    design <- cer_alt_drop_hypotheses(
      design,
      (seq_along(treatment_assoc) %in% arms)
    )
  } else {
    design <- cer_drop_hypotheses(
      design,
      (seq_along(treatment_assoc) %in% arms)
    )
  }

  correlation <- get_multiarm_correlation(
    design$controls,
    design$treatment_assoc,
    n_cont_2,
    n_treat_2
  )

  design |> cer_adapt(t = t, correlation = correlation)
}
