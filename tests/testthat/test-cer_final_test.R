test_that("example from paper works", {
    design <- make_example_design()

    design <- cer_interim_test(design, c(0.00045, 0.0952, 0.0225, 0.1104))

    reallocated_t <- (1/(2/35))/(1/(2/35) + 1/(1/52 + 1/53))
    ad_t <- c(1, reallocated_t, 1, reallocated_t)
    design_adj <- cer_drop_hypotheses(design, c(TRUE, FALSE, TRUE, FALSE)) |>
        cer_adapt(weights = c(0, 0.5, 0, 0.5), t=ad_t)
    
    design_adj <- cer_adapt_bounds(design_adj)

    design_tested <- cer_final_test(design_adj, c(NA, 0.0111, NA, 0.0234))
    expect_equal(design_tested$rej, c(T, T, F, T))


})
