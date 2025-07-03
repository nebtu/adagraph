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

redistribute_n <- function(
  n,
  which_drop,
  t = 1 / 2,
  n1 = NA,
  simple_redistribute = FALSE
) {
  if (!is.na(t)) {
    n1 <- floor(n * t)
  }
  keep <- !(seq_along(n) %in% which_drop)
  n2 <- n - n1
  if (any(!keep)) {
    weights <- n[keep]

    n_reassign <- sum(n[!keep])

    quotas <- n_reassign * weights / sum(weights)
    reassign <- floor(quotas)
    remainders <- quotas - reassign

    remaining <- n_reassign - sum(reassign)
    if (remaining > 0) {
      if (simple_redistribute) {
        top_indices <- remainders[1:remaining]
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

# for now this assumes that n1, n of the first stage, was like planned according
# to t
# n_cont_2 and n_treat_2 still has the same lenght as before, so dropped arms
# should proabbly, but not automatically have n of 0
multiarm_drop_arms <- function(
  design,
  arms,
  n_cont_2 = NA,
  n_treat_2 = NA,
  alt_adj = FALSE
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
