test_that("multiarm and direct calculation are similar", {
  direct_multiarm <- function(n_controls, n_treatments) {
    corr <- matrix(1, nrow = length(n_treatments), ncol = length(n_treatments))
    for (t1 in seq_along(n_treatments)) {
      for (t2 in seq_along(n_treatments)) {
        corr[t1, t2] <- get_Z_correlation(
          n_control_1 = n_controls,
          n_control_2 = n_controls,
          n_control_shared = n_controls,
          n_treatment_1 = n_treatments[t1],
          n_treatment_2 = n_treatments[t2],
          n_treatment_shared = ifelse(t1 == t2, n_treatments[t1], 0)
        )
      }
    }

    return(corr)
  }

  options <- expand.grid(
    control = 1:200,
    treatment_1 = 1:100,
    treatment_2 = 1:100,
    treatment_3 = 1:500
  )

  for (i in nrow(options)) {
    expect_equal(
      direct_multiarm(
        n_controls = options$control[i],
        n_treatments = c(
          options$treatment_1[i],
          options$treatment_2[i],
          options$treatment_3[i]
        )
      ),
      get_multiarm_correlation(
        controls = 1,
        treatment_assoc = c(1, 1, 1),
        n_controls = options$control[i],
        n_treatments = c(
          options$treatment_1[i],
          options$treatment_2[i],
          options$treatment_3[i]
        )
      )
    )
  }
})
