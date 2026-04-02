#' Get correlation between different subgroups and arms
#'
#' Using the exact proportions/case numbers, calculate the correlations between
#' different subgroups and study arms
#'
#' To calculate the subgroup and arm correlation structure, subgroups can be provided
#' either as proportions or patient numbers.
#' Note that when using the n_control and n_arm, this is translated to the
#' following structure internally as well.
#' The structure (in argument n_subgroups) should be given as a dataframe, where
#' each row specifies a specific subgroup in a specific arm (or the control).
#' Therefore the first column should be named arm, and have values of
#' "control" and the names of the arms. Then should be columns for each of the
#' subgroups (using the subgroup name as a column name), with logical values,
#' specifying the exact combination of subgroups that are being specified. The
#' last column should be either names 'n' (for case numbers) or 'prop' (for
#' proportions) and give the given value for this exact intersection of
#' subgroups.
#'
#' @param arms Number of arms
#' @param subgroups Number of subgroups in addition to the whole collective (can be 0)
#' @param n_subgroups A data.frame specifying the structure of the subgroups,
#'   see details for more information
#' @param names_arms names for the different arms.
#' @param names_subgroups names for the different subgroups
#'
#' @export
get_subgroup_correlation <- function(
  subgroups,
  arms,
  n_subgroups,
  names_arms,
  names_subgroups
) {
  corr <- matrix(
    nrow = (subgroups + 1) * arms,
    ncol = (subgroups + 1) * arms
  )
  n_name <- ifelse("n" %in% colnames(n_subgroups), "n", "prop")

  #number of people in each (subgroup, arm) combination
  n_total_subgroups <- do.call(
    rbind,
    lapply(c(names_arms, "control"), \(arm_name) {
      do.call(
        rbind,
        lapply(names_subgroups, \(name) {
          n = sum(n_subgroups[
            n_subgroups[, name] == TRUE & n_subgroups[, "arm"] == arm_name,
            n_name
          ])
          data.frame(arm = arm_name, subgroup = name, n = n)
        })
      )
    })
  )

  # add total n per arm
  n_total_subgroups <- rbind(
    n_total_subgroups,
    do.call(
      rbind,
      lapply(c(names_arms, "control"), \(arm_name) {
        n = sum(n_subgroups[n_subgroups[, "arm"] == arm_name, n_name])
        data.frame(arm = arm_name, subgroup = "Total", n = n)
      })
    )
  )
  names_subgroups <- c("Total", names_subgroups)

  #### We want to iterate through all entries of the upper diagonal of the
  ###correlation matrix, i.e. all unordered combinations of (group, arm) tuples
  # All (group, arm) combinations
  cells <- expand.grid(group = seq_len(subgroups + 1), arm = seq_len(arms))
  # All unordered pairs of those cells (including diagonal)
  idx <- expand.grid(i = seq_len(nrow(cells)), j = seq_len(nrow(cells)))
  #need only upper left of matrix
  idx <- idx[idx$i < idx$j, ]

  entries <- cbind(
    cells[idx$i, ],
    cells[idx$j, ]
  )
  rownames(entries) <- NULL
  colnames(entries) <- c("group_1", "arm_1", "group_2", "arm_2")

  diag(corr) <- 1
  for (idx in seq_len(nrow(entries))) {
    group_1 <- entries[idx, "group_1"]
    group_2 <- entries[idx, "group_2"]
    arm_1 <- entries[idx, "arm_1"]
    arm_2 <- entries[idx, "arm_2"]
    idx_1 <- (group_1 - 1) * (arms) + arm_1
    idx_2 <- (group_2 - 1) * (arms) + arm_2

    if (group_1 == 1) {
      group_1_filter <- TRUE
    } else {
      group_1_filter <- n_subgroups[, names_subgroups[group_1]] == 1
    }
    if (group_2 == 1) {
      group_2_filter <- TRUE
    } else {
      group_2_filter <- n_subgroups[, names_subgroups[group_2]] == 1
    }

    if (arm_1 != arm_2) {
      n_treatment_shared <- 0
    } else {
      filter_rows <- (n_subgroups[, "arm"] == names_arms[arm_1]) &
        group_1_filter &
        group_2_filter
      n_treatment_shared <- sum(n_subgroups[filter_rows, n_name])
    }

    filter_rows <- (n_subgroups[, "arm"] == "control") &
      group_1_filter &
      group_2_filter
    n_control_shared <- sum(n_subgroups[filter_rows, n_name])

    n_control_1 <- n_total_subgroups[
      n_total_subgroups[, "arm"] == "control" &
        n_total_subgroups[, "subgroup"] == names_subgroups[group_1],
      n_name
    ]
    n_control_2 <- n_total_subgroups[
      n_total_subgroups[, "arm"] == "control" &
        n_total_subgroups[, "subgroup"] == names_subgroups[group_2],
      n_name
    ]

    n_treatment_1 <- n_total_subgroups[
      n_total_subgroups[, "arm"] == names_arms[arm_1] &
        n_total_subgroups[, "subgroup"] == names_subgroups[group_1],
      n_name
    ]

    n_treatment_2 <- n_total_subgroups[
      n_total_subgroups[, "arm"] == names_arms[arm_2] &
        n_total_subgroups[, "subgroup"] == names_subgroups[group_2],
      n_name
    ]

    corr_entry <- get_Z_correlation(
      n_control_1 = n_control_1,
      n_control_2 = n_control_2,
      n_control_shared = n_control_shared,
      n_treatment_1 = n_treatment_1,
      n_treatment_2 = n_treatment_2,
      n_treatment_shared = n_treatment_shared
    )

    corr[idx_1, idx_2] <- corr_entry
    corr[idx_2, idx_1] <- corr_entry
  }

  corr
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

get_Z_correlation <- function(
  n_control_1,
  n_control_2,
  n_control_shared,
  n_treatment_1,
  n_treatment_2,
  n_treatment_shared
) {
  #These are the standard deviations of the differences in means, ignoring the shared
  #sigma of all groups
  sd_1 <- sqrt((1 / n_control_1) + (1 / n_treatment_1))
  sd_2 <- sqrt((1 / n_control_2) + (1 / n_treatment_2))

  covar <- (n_control_shared / (n_control_1 * n_control_2)) +
    (n_treatment_shared / (n_treatment_1 * n_treatment_2))

  return(covar / (sd_1 * sd_2))
}
