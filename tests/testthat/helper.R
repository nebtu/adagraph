local_future_plan <- function(strategy, ..., .local_envir = parent.frame()) {
  old_plan <- future::plan()
  withr::local_options(
    list("adagraph.use_future" = TRUE),
    .local_envir = .local_envir
  )

  withr::defer(
    future::plan(old_plan)
  )

  future::plan(strategy, ...)
}

make_example_design <- function() {
  #=======
  m <- rbind(
    H1 = c(0, 1 / 2, 1 / 2, 0),
    H2 = c(1 / 2, 0, 0, 1 / 2),
    H3 = c(0, 1, 0, 0),
    H4 = c(1, 0, 0, 0)
  )
  weights <- c(1 / 2, 1 / 2, 0, 0)
  correlation = matrix(rep(1 / 2, 16), nrow = 4) + 1 / 2 * diag(4)
  correlation[1:2, 3:4] = NA
  correlation[3:4, 1:2] = NA
  diag(correlation) = 1
  t = 0.5
  alpha = 0.025

  #spending function
  as = function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
  #========

  design <- cer_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as,
    t = t
  )

  design
}
make_example_multiarm <- function() {
  #=======
  m <- rbind(
    H1 = c(0, 1 / 2, 1 / 2, 0),
    H2 = c(1 / 2, 0, 0, 1 / 2),
    H3 = c(0, 1, 0, 0),
    H4 = c(1, 0, 0, 0)
  )
  weights <- c(1 / 2, 1 / 2, 0, 0)
  t <- 0.5
  alpha <- 0.025
  #spending function
  as <- function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
  #========

  design <- multiarm_cer_design(
    controls = 2,
    treatment_assoc = c(1, 2, 1, 2),
    n_controls = 70,
    n_treatments = 70,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as
  )

  design
}
