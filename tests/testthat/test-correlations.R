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
    treatment_1 = 1:50,
    treatment_2 = 1:100,
    treatment_3 = 1:200
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

test_that("subgroup correlation calculations, with 2 groups and 2 arms", {
  n_subgroups <- tribble(
    ~"arm"    , ~"G1" , ~"G2" , ~"n" ,
    "control" , F     , F     ,    0 ,
    "control" , F     , T     ,   10 ,
    "control" , T     , F     ,   10 ,
    "control" , T     , T     ,   10 ,
    "A1"      , F     , F     ,   20 ,
    "A1"      , F     , T     ,   20 ,
    "A1"      , T     , F     ,   20 ,
    "A1"      , T     , T     ,   20 ,
    "A2"      , F     , F     ,   10 ,
    "A2"      , F     , T     ,   10 ,
    "A2"      , T     , F     ,   30 ,
    "A2"      , T     , T     ,   10 ,
  )
  names_arms <- c("A1", "A2")
  names_subgroups <- c("G1", "G2")

  corr <- get_subgroup_correlation(
    subgroups = 2,
    arms = 2,
    n_subgroups = n_subgroups,
    names_arms = names_arms,
    names_subgroups = names_subgroups
  )

  expect_snapshot_value(corr, style = "json2")
})


test_that("subgroup correlation calculations, with 3 groups and 2 arms", {
  n_subgroups <- tribble(
    ~"arm"    , ~"G1" , ~"G2" , ~"G3" , ~"n" ,
    "control" , F     , F     , F     ,    0 ,
    "control" , F     , T     , F     ,   10 ,
    "control" , T     , F     , F     ,   10 ,
    "control" , T     , T     , F     ,   10 ,
    "A1"      , F     , F     , F     ,   20 ,
    "A1"      , F     , T     , F     ,   30 ,
    "A1"      , T     , F     , F     ,   40 ,
    "A1"      , T     , T     , F     ,   50 ,
    "A2"      , F     , F     , F     ,   10 ,
    "A2"      , F     , T     , F     ,   10 ,
    "A2"      , T     , F     , F     ,   30 ,
    "A2"      , T     , T     , F     ,   10 ,
    "control" , F     , F     , T     ,    0 ,
    "control" , F     , T     , T     ,   10 ,
    "control" , T     , F     , T     ,   10 ,
    "control" , T     , T     , T     ,   10 ,
    "A1"      , F     , F     , T     ,   20 ,
    "A1"      , F     , T     , T     ,   10 ,
    "A1"      , T     , F     , T     ,   20 ,
    "A1"      , T     , T     , T     ,   20 ,
    "A2"      , F     , F     , T     ,   10 ,
    "A2"      , F     , T     , T     ,   10 ,
    "A2"      , T     , F     , T     ,   30 ,
    "A2"      , T     , T     , T     ,   10 ,
  )
  names_arms <- c("A1", "A2")
  names_subgroups <- c("G1", "G2", "G3")

  corr <- get_subgroup_correlation(
    subgroups = 3,
    arms = 2,
    n_subgroups = n_subgroups,
    names_arms = names_arms,
    names_subgroups = names_subgroups
  )

  expect_snapshot_value(corr, style = "json2")
})

test_that("subgroup correlation calculations, simulate arms as subgroups", {
  n_subgroups <- tribble(
    ~"arm"    , ~"G1" , ~"G2" , ~"n" ,
    "control" , F     , F     ,    0 ,
    "control" , F     , T     ,    0 ,
    "control" , T     , F     ,    0 ,
    "control" , T     , T     ,   50 ,
    "A1"      , F     , F     ,    0 ,
    "A1"      , F     , T     ,   50 ,
    "A1"      , T     , F     ,   50 ,
    "A1"      , T     , T     ,    0 ,
  )
  names_arms <- "A1"
  names_subgroups <- c("G1", "G2")

  corr <- get_subgroup_correlation(
    subgroups = 2,
    arms = 1,
    n_subgroups = n_subgroups,
    names_arms = names_arms,
    names_subgroups = names_subgroups
  )

  expect_equal(corr[c(2, 3), c(2, 3)], rbind(c(1, 1 / 2), c(1 / 2, 1)))
})
