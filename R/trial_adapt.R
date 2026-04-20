#' Adapt the patient numbers for the second stage of a trial design
#'
#' Without any changes, `adagraph` expects the correlation between the p-values
#' to be the same as in the first stage. If there are changes in the relative
#' sample sizes, either because of changes in the design or random variations,
#' this might not be accurate. Additionally, one might want to use a different t
#' for calculating the combined p-value, using the information fraction of the
#' actual realised patient numbers.
#'
#' @param design The `trial_design` to be adapted
#' @param n_control_2 The patient numbers in the control. Can only be used if
#'   there are no subgroups
#' @param n_arms_2 The patient numbers in the arms (as a numberic vector). Can only be used if
#'   there are no subgroups
#' @param ad_n_table A data.frame specifying the structure of the subgroups,
#'   see details for more information
#' @param calculate_t Calculate a new information fraction t, and adapt the
#'   design to use it.
#' @param adapt_bounds Adapt the bounds for rejecting a hypotheses to keep the
#'   FWER with the new adaptations. If doing multiple adaptations, it is enough to
#'   adapt bounds only for the last one, or call `adapt_bounds()` manually
#'   after.
#'
#' @return An object of class `trial_design`, with the adaptations applied.
#'
#' @examples
#' m <- rbind(
#'   c(0, 1 / 2, 1 / 2, 0),
#'   c(1 / 2, 0, 0, 1 / 2),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' as <- function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
#'
#' des <- trial_design(
#'   arms = 2,
#'   endpoints = 2,
#'   n_control = 35,
#'   n_arms = 35,
#'   weights = c(1 / 2, 1 / 2, 0, 0),
#'   t = 0.5,
#'   alpha = 0.025,
#'   test_m = m,
#'   alpha_spending_f = as
#' ) |>
#'   cer_interim_test(c(
#'    0.00045,
#'    0.0952,
#'    0.0225,
#'    0.1104
#'  ))
#'
#' des_ad <- des |>
#'   trial_adapt_n(n_control_2 = 50, n_arms = c(20, 30))
#'
#' des_ad
#' des_ad$ad_correlation #changed correlation structure
#' des_ad$ad_t #changed t (different for each hypothesis)
#'
#' n_table <- rbind(
#'   data.frame(arm = "control", `G1` = FALSE, n = 20),
#'   data.frame(arm = "control", `G1` = TRUE, n = 15),
#'   data.frame(arm = "A1", `G1` = FALSE, n = 20),
#'   data.frame(arm = "A1", `G1` = TRUE, n = 15)
#' )
#' des <- trial_design(
#'   endpoints = 2,
#'   subgroups = 1,
#'   n_table = n_table,
#'   weights = c(1 / 2, 1 / 2, 0, 0),
#'   t = 0.5,
#'  alpha = 0.025,
#'  test_m = m,
#'  alpha_spending_f = as
#' ) |>
#'   cer_interim_test(c(
#'     0.00045,
#'     0.0952,
#'     0.0225,
#'     0.1104
#'   ))
#'
#' ad_n_table <- rbind(
#'   data.frame(arm = "control", `G1` = FALSE, n = 30),
#'   data.frame(arm = "control", `G1` = TRUE, n = 20),
#'   data.frame(arm = "A1", `G1` = FALSE, n = 10),
#'   data.frame(arm = "A1", `G1` = TRUE, n = 15)
#' )
#' des_ad <- des |>
#'   trial_adapt_n(ad_n_table = ad_n_table)
#'
#' des_ad
#'
#' des_ad$ad_correlation #changed correlation structure
#' des_ad$ad_t #changed t (different for each hypothesis)
#' @export
trial_adapt_n <- function(
  design,
  n_control_2 = NULL,
  n_arms_2 = NULL,
  ad_n_table = NULL,
  calculate_t = TRUE,
  adapt_bounds = TRUE
) {
  if (length(n_arms_2) == 1) {
    n_arms_2 <- rep(n_arms_2, design[["arms"]])
  }

  if (!is.null(n_control_2) || !is.null(n_arms_2)) {
    if (design[["subgroups"]] != 0) {
      cli::cli_warn(
        "The {.arg n_control_2} and {.arg n_arms_2} arguments are only supported for designs without subgroups."
      )
    } else if (!is.null(ad_n_table)) {
      cli::cli_warn(
        "{.arg n_control_2} and {.arg n_arms_2} are ignored since {.arg ad_n_table} was supplied"
      )
    } else {
      ad_n_table <- data.frame(
        arm = c("control", design[["names_arms"]]),
        n = c(n_control_2, n_arms_2)
      )
    }
  }

  ad_correlation <- get_trial_correlation(
    design[["arms"]],
    design[["endpoints"]],
    design[["subgroups"]],
    ad_n_table,
    design[["names_arms"]],
    design[["names_subgroups"]]
  )

  if (calculate_t) {
    total_subgroup_2 <- get_total_subgroup(
      ad_n_table,
      design[["names_arms"]],
      design[["names_subgroups"]]
    )

    hyp_assoc <- design[["hyp_assoc"]]
    total_subgroup_1 <- get_total_subgroup(
      design[["n_table"]],
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
  design_ad$ad_n_table <- ad_n_table

  design_ad
}

#' Dropp groups, arms or endpoints from a trial design
#'
#' Functions for dropping all hypotheses associated with some subgroups, arms or
#' endpoints. The weights of the dropped hypotheses are set to 0 and distributed
#' according to the prespecified graph. The graph is also adapted such that
#' weights are never assigned to the dropped hypotheses.
#'
#' @name trial_drop
#'
#' @param design The `trial_design` of which to drop the
#' @param groups The subgroups to be dropped, by their name or integer indices
#' @param arms The arms to be dropped, by their name or integer indices
#' @param endpoints The endpoints to be dropped, by their name or integer indices
#' @param adapt_bounds Adapt the bounds for rejecting a hypotheses to keep the
#'   FWER with the new adaptations. If doing multiple adaptations, it is enough to
#'   adapt bounds only for the last one, or call `adapt_bounds()` manually
#'   after.
#'
#' @return design with the specified hypotheses (according to groups, arms or
#'   endpoints) dropped
#'
#' @examples
#' m <- rbind(
#'   c(0, 1 / 2, 1 / 2, 0),
#'   c(1 / 2, 0, 0, 1 / 2),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' as <- function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
#'
#' des <- trial_design(
#'   arms = 4,
#'   n_control = 35,
#'   n_arms = 35,
#'   weights = c(1/2, 1/2, 0, 0),
#'   t = 0.5,
#'   alpha = 0.025,
#'   test_m = m,
#'   alpha_spending_f = as
#' ) |>
#'  cer_interim_test(c(
#'    0.00045,
#'    0.0952,
#'    0.0225,
#'    0.1104
#'  ))
#'
#' des_ad <- des |>
#'   trial_drop_arms(c("A1", "A4"))
#'
#' des_ad
#' des_ad$ad_test_m
#'
#' n_table <- rbind(
#'   data.frame(arm = "control", `G1` = FALSE, n = 20),
#'   data.frame(arm = "control", `G1` = TRUE, n = 15),
#'   data.frame(arm = "A1", `G1` = FALSE, n = 20),
#'   data.frame(arm = "A1", `G1` = TRUE, n = 15)
#' )
#' des <- trial_design(
#'   endpoints = 2,
#'   subgroups = 1,
#'   n_table = n_table,
#'   weights = c(1/2, 1/2, 0, 0),
#'  t = 0.5,
#'  alpha = 0.025,
#'  test_m = m,
#'  alpha_spending_f = as
#' ) |>
#'   cer_interim_test(c(
#'     0.00045,
#'     0.0952,
#'     0.0225,
#'     0.1104
#'   ))
#'
#' des_ad <- des |>
#'   trial_drop_groups("G1") |>
#'   trial_drop_endpoints("E1")
#'
#' des_ad
#' des_ad$ad_test_m

#' @rdname trial_drop
#' @export
trial_drop_groups <- function(
  design,
  groups,
  adapt_bounds = TRUE
) {
  drop_hyp <- which(design[["hyp_assoc"]][, "group"] %in% groups)
  cer_drop_hypotheses(design, drop_hyp, adapt_bounds = adapt_bounds)
}

#' @rdname trial_drop
#' @export
trial_drop_arms <- function(
  design,
  arms,
  adapt_bounds = TRUE
) {
  drop_hyp <- which(design[["hyp_assoc"]][, "arm"] %in% arms)
  cer_drop_hypotheses(design, drop_hyp, adapt_bounds = adapt_bounds)
}

#' @rdname trial_drop
#' @export
trial_drop_endpoints <- function(
  design,
  endpoints,
  adapt_bounds = TRUE
) {
  drop_hyp <- which(design[["hyp_assoc"]][, "endpoint"] %in% endpoints)
  cer_drop_hypotheses(design, drop_hyp, adapt_bounds = adapt_bounds)
}
