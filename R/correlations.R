#' Get correlation between different subgroups and arms
#'
#' Using the exact proportions/case numbers, calculate the correlations between
#' different subgroups and study arms
get_subgroup_correlation <- function(
  subgroups,
  arms,
  n_subgroups,
  names_arms,
  names_subgroups,
  correlation_endpoint
) {
  corr <- matrix(
    nrow = (subgroups + 1) * arms,
    ncol = (subgroups + 1) * arms
  )
  n_name = ifelse("n" %in% colnames(n_subgroups), "n", "prop")

  n_total_subgroups = lapply(names_subgroups, \(name) {
    sum(n_subgroups[n_subgroups[name], n_name])
  })
  entries <- expand.grid(
    arms = seq_len(arms),
    subgroups = seq_len(subgroups + 1)
  )

  for (idx in seq_along(entries)) {
    name_group
    shared_n <- sum(n_subgroups[n_subgroups[names_]])
    #separate case if full group included, else:
    #get name of group before (1 - idx for index)
    #get shared n
    #call get_Z_correlation()
    #add to matrix
  }
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
