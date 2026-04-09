mame_adapt_n <- function(
  design,
  n_subgroups_2,
  calculate_t = TRUE,
  adapt_bounds = TRUE
) {
  ad_correlation <- get_mame_correlation(
    design[["arms"]],
    design[["endpoints"]],
    design[["subgroups"]],
    n_subgroups_2,
    design[["names_arms"]],
    design[["names_subgroups"]]
  )

  if (calculate_t) {
    total_subgroup_2 <- get_total_subgroup(
      n_subgroups_2,
      design[["names_arms"]],
      design[["names_subgroups"]]
    )

    hyp_assoc <- design[["hyp_assoc"]]
    total_subgroup_1 <- get_total_subgroup(
      design[["n_subgroups"]],
      design[["names_arms"]],
      design[["names_subgroups"]]
    )

    #NOTE: this is not terribly efficient, since we calculate the same thing
    #multiple times for each endpoint, but that shouldn't matter much
    ad_t <- vapply(
      seq_len(nrow(hyp_assoc)),
      \(i) {
        subgroup <- hyp_assoc[[i, "group"]]
        arm <- hyp_assoc[[i, "arm"]]
        n_control_1 <- total_subgroup_1[
          total_subgroup_1[, "arm"] == "control" &
            total_subgroup_1[, "subgroup"] == subgroup,
          "n"
        ]
        n_control_2 <- total_subgroup_2[
          total_subgroup_2[, "arm"] == "control" &
            total_subgroup_2[, "subgroup"] == subgroup,
          "n"
        ]
        n_treat_1 <- total_subgroup_1[
          total_subgroup_1[, "arm"] == arm &
            total_subgroup_1[, "subgroup"] == subgroup,
          "n"
        ]
        n_treat_2 <- total_subgroup_2[
          total_subgroup_2[, "arm"] == arm &
            total_subgroup_2[, "subgroup"] == subgroup,
          "n"
        ]
        get_t(n_control_1, n_control_2, n_treat_1, n_treat_2)
      },
      numeric(1)
    )
  } else {
    ad_t <- NULL
  }

  design_ad <- cer_adapt(
    design,
    time = ad_t,
    correlation = ad_correlation,
    adapt_bounds = adapt_bounds
  )
  design_ad$n_subgroups_2 <- n_subgroups_2

  design_ad
}

