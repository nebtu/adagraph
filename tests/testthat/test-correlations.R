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

    corr
  }

  options <- expand.grid(
    control = seq(1, 100, by = 10),
    treatment_1 = seq(1, 500, by = 100),
    treatment_2 = seq(1, 100, by = 10),
    treatment_3 = seq(1, 200, by = 50) + 3
  )

  for (i in seq_len(nrow(options))) {
    #direct correlation
    corr_direct <- direct_multiarm(
      n_controls = options$control[i],
      n_treatments = c(
        options$treatment_1[i],
        options$treatment_2[i],
        options$treatment_3[i]
      )
    )

    #treatments as subgroups
    #fmt: skip
    n_subgroups <- rbind(
      data.frame(arm = "control", G1 = FALSE, G2 = FALSE, G3 = FALSE, n = 0),
      data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  G3 = FALSE, n = 0),
      data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, G3 = FALSE, n = 0),
      data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  G3 = FALSE, n = 0),
      data.frame(arm = "control", G1 = FALSE, G2 = FALSE, G3 = TRUE,  n = 0),
      data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  G3 = TRUE,  n = 0),
      data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, G3 = TRUE,  n = 0),
      data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  G3 = TRUE,  n = options$control[i]),
      data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, G3 = TRUE,  n = options$treatment_3[i]),
      data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  G3 = TRUE,  n = 0),
      data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, G3 = TRUE,  n = 0),
      data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  G3 = TRUE,  n = 0),
      data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, G3 = FALSE, n = 0),
      data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  G3 = FALSE, n = options$treatment_2[i]),
      data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, G3 = FALSE, n = options$treatment_1[i]),
      data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  G3 = FALSE, n = 0)
    )

    names_arms <- "A1"
    names_subgroups <- c("G1", "G2", "G3")
    corr_subgroup <- get_subgroup_correlation(
      subgroups = 3,
      arms = 1,
      n_subgroups = n_subgroups,
      names_arms = names_arms,
      names_subgroups = names_subgroups
    )

    expect_equal(
      corr_subgroup[c(2, 3, 4), c(2, 3, 4)],
      corr_direct
    )

    #multiarm correlation
    expect_equal(
      corr_direct,
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

    #multiarm from subgroup_correlation without subgroups
    n_subgroups <- rbind(
      data.frame(arm = "control", n = options$control[i]),
      data.frame(arm = "A1", n = options$treatment_1[i]),
      data.frame(arm = "A2", n = options$treatment_2[i]),
      data.frame(arm = "A3", n = options$treatment_3[i])
    )
    names_arms <- c("A1", "A2", "A3")
    names_subgroups <- c()

    corr <- get_subgroup_correlation(
      subgroups = 0,
      arms = 3,
      n_subgroups = n_subgroups,
      names_arms = names_arms,
      names_subgroups = names_subgroups
    )
    expect_equal(
      corr_direct,
      corr
    )
  }
})

test_that("subgroup correlation calculations, with 2 groups and 2 arms", {
  # n_subgroups <- tribble(
  #   ~"arm"    , ~"G1" , ~"G2" , ~"n" ,
  #   "control" , F     , F     ,    0 ,
  #   "control" , F     , T     ,   10 ,
  #   "control" , T     , F     ,   10 ,
  #   "control" , T     , T     ,   10 ,
  #   "A1"      , F     , F     ,   20 ,
  #   "A1"      , F     , T     ,   20 ,
  #   "A1"      , T     , F     ,   20 ,
  #   "A1"      , T     , T     ,   20 ,
  #   "A2"      , F     , F     ,   10 ,
  #   "A2"      , F     , T     ,   10 ,
  #   "A2"      , T     , F     ,   30 ,
  #   "A2"      , T     , T     ,   10 ,
  # )
  #fmt: skip
  n_subgroups <- rbind(
    data.frame(arm = "control", G1 = FALSE, G2 = FALSE, n =  0),
    data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  n = 10),
    data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, n = 20),
    data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  n = 20),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, n = 20),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  n = 20),
    data.frame(arm = "A2",      G1 = FALSE, G2 = FALSE, n = 10),
    data.frame(arm = "A2",      G1 = FALSE, G2 = TRUE,  n = 10),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = FALSE, n = 30),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = TRUE,  n = 10)
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
  # n_subgroups <- tribble(
  #   ~"arm"    , ~"G1" , ~"G2" , ~"G3" , ~"n" ,
  #   "control" , F     , F     , F     ,    0 ,
  #   "control" , F     , T     , F     ,   10 ,
  #   "control" , T     , F     , F     ,   10 ,
  #   "control" , T     , T     , F     ,   10 ,
  #   "A1"      , F     , F     , F     ,   20 ,
  #   "A1"      , F     , T     , F     ,   30 ,
  #   "A1"      , T     , F     , F     ,   40 ,
  #   "A1"      , T     , T     , F     ,   50 ,
  #   "A2"      , F     , F     , F     ,   10 ,
  #   "A2"      , F     , T     , F     ,   10 ,
  #   "A2"      , T     , F     , F     ,   30 ,
  #   "A2"      , T     , T     , F     ,   10 ,
  #   "control" , F     , F     , T     ,    0 ,
  #   "control" , F     , T     , T     ,   10 ,
  #   "control" , T     , F     , T     ,   10 ,
  #   "control" , T     , T     , T     ,   10 ,
  #   "A1"      , F     , F     , T     ,   20 ,
  #   "A1"      , F     , T     , T     ,   10 ,
  #   "A1"      , T     , F     , T     ,   20 ,
  #   "A1"      , T     , T     , T     ,   20 ,
  #   "A2"      , F     , F     , T     ,   10 ,
  #   "A2"      , F     , T     , T     ,   10 ,
  #   "A2"      , T     , F     , T     ,   30 ,
  #   "A2"      , T     , T     , T     ,   10 ,
  # )
  #fmt: skip
  n_subgroups <- rbind(
    data.frame(arm = "control", G1 = FALSE, G2 = FALSE, G3 = FALSE, n =  0),
    data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  G3 = FALSE, n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, G3 = FALSE, n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  G3 = FALSE, n = 10),
    data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, G3 = FALSE, n = 20),
    data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  G3 = FALSE, n = 30),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, G3 = FALSE, n = 40),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  G3 = FALSE, n = 50),
    data.frame(arm = "A2",      G1 = FALSE, G2 = FALSE, G3 = FALSE, n = 10),
    data.frame(arm = "A2",      G1 = FALSE, G2 = TRUE,  G3 = FALSE, n = 10),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = FALSE, G3 = FALSE, n = 30),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = TRUE,  G3 = FALSE, n = 10),
    data.frame(arm = "control", G1 = FALSE, G2 = FALSE, G3 = TRUE,  n =  0),
    data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  G3 = TRUE,  n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, G3 = TRUE,  n = 10),
    data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  G3 = TRUE,  n = 10),
    data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, G3 = TRUE,  n = 20),
    data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  G3 = TRUE,  n = 10),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, G3 = TRUE,  n = 20),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  G3 = TRUE,  n = 20),
    data.frame(arm = "A2",      G1 = FALSE, G2 = FALSE, G3 = TRUE,  n = 10),
    data.frame(arm = "A2",      G1 = FALSE, G2 = TRUE,  G3 = TRUE,  n = 10),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = FALSE, G3 = TRUE,  n = 30),
    data.frame(arm = "A2",      G1 = TRUE,  G2 = TRUE,  G3 = TRUE,  n = 10)
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
  # n_subgroups <- tribble(
  #   ~"arm"    , ~"G1" , ~"G2" , ~"n" ,
  #   "control" , F     , F     ,    0 ,
  #   "control" , F     , T     ,    0 ,
  #   "control" , T     , F     ,    0 ,
  #   "control" , T     , T     ,   50 ,
  #   "A1"      , F     , F     ,    0 ,
  #   "A1"      , F     , T     ,   50 ,
  #   "A1"      , T     , F     ,   50 ,
  #   "A1"      , T     , T     ,    0 ,
  # )
  #fmt: skip
  n_subgroups <- rbind(
    data.frame(arm = "control", G1 = FALSE, G2 = FALSE, n =  0),
    data.frame(arm = "control", G1 = FALSE, G2 = TRUE,  n =  0),
    data.frame(arm = "control", G1 = TRUE,  G2 = FALSE, n =  0),
    data.frame(arm = "control", G1 = TRUE,  G2 = TRUE,  n = 50),
    data.frame(arm = "A1",      G1 = FALSE, G2 = FALSE, n =  0),
    data.frame(arm = "A1",      G1 = FALSE, G2 = TRUE,  n = 50),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = FALSE, n = 50),
    data.frame(arm = "A1",      G1 = TRUE,  G2 = TRUE,  n =  0)
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
