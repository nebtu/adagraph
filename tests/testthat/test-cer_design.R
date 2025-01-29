# TODO: test that giving weight 0 gives correct bounds
#       test that single hypotheses give correct
#       test how correlation NA or 0 works
test_that("Simple CER design works", {
    correlation <- rbind(H1=c(1, NA),
                         H2=c(NA, 1))
    weights <- c(2/3, 1/3)
    test_m <- rbind(c(0, 1),
                    c(1, 0))
    alpha <- 0.025
    alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
    prep_t <- 0.5
    design <- cer_design(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        test_m = test_m,
        alpha_spending_f = alpha_spending_f,
        prep_t = prep_t
    )
    expect_equal(design$correlation, correlation)
    expect_equal(design$weights, weights)
    expect_equal(design$test_m, test_m)
    expect_equal(design$alpha, alpha)
    expect_equal(design$alpha_spending_f, alpha_spending_f)
    expect_equal(design$prep_t, prep_t)
})

test_that("Correct validation of cer_design", {
    correlation <- rbind(H1=c(1, NA),
                         H2=c(NA, 1))
    weights <- c(2/3, 1/3)
    test_m <- rbind(c(0, 1),
                    c(1, 0))
    alpha <- 0.025
    alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
    prep_t <- 0.5
    expect_error(cer_design(correlation = rbind(c(1, NA)),
                                                    weights = weights,
                                                    alpha = alpha,
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t),
                             class = "invalid_argument_correlation")
    expect_error(cer_design(correlation = "correlation",
                                                    weights = weights,
                                                    alpha = alpha,
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t),
                             class = "invalid_argument_correlation")
    expect_error(cer_design(correlation = correlation,
                                                    weights = 1,
                                                    alpha = alpha,
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t),
                             class = "invalid_argument_weights")
    expect_error(cer_design(correlation = correlation,
                                                    weights = weights,
                                                    alpha = "0.05",
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t),
                             class = "invalid_argument_alpha")
    expect_error(cer_design(correlation = correlation,
                                                    weights = weights,
                                                    alpha = 1.05,
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t),
                             class = "invalid_argument_alpha")
    expect_error(cer_design(correlation = correlation,
                                                    weights = weights,
                                                    alpha = alpha,
                                                    test_m = "test_m",
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t),
                             class = "invalid_argument_test_m")
    expect_error(cer_design(correlation = correlation,
                                                    weights = weights,
                                                    alpha = alpha,
                                                    test_m = rbind(c(1, 0, 0), c(1, 0, 0), c(1, 0, 0)),
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t),
                             class = "invalid_argument_test_m")
    expect_error(cer_design(correlation = correlation,
                                                    weights = weights,
                                                    alpha = alpha,
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = "0.5"),
                             class = "invalid_argument_prep_t")
    expect_error(cer_design(correlation = correlation,
                                                    weights = weights,
                                                    alpha = alpha,
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = 1.5),
                             class = "invalid_argument_prep_t")
    expect_error(cer_design(correlation = correlation,
                                                    weights = weights,
                                                    alpha = alpha,
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t,
                                                    seq_bonf = "TRUE"),
                             class = "invalid_argument_seq_bonf")
    expect_error(cer_design(correlation = correlation,
                                                    weights = weights,
                                                    alpha = alpha,
                                                    test_m = test_m,
                                                    alpha_spending_f = alpha_spending_f,
                                                    prep_t = prep_t,
                                                    parallelize = "FALSE"),
                             class = "invalid_argument_parallelize")
})