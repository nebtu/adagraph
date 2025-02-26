# TODO: test that giving weight 0 gives correct bounds
#       test that single hypotheses give correct bounds
#       test how correlation NA or 0 works



test_that("Example from paper gives correct bounds", {
    #=======
    m <- rbind(H1=c(0,1/2,1/2,0),
               H2=c(1/2,0,0,1/2),
               H3=c(0,1,0,0),
               H4=c(1,0,0,0))
    weights <- c(rep(1/2,2),rep(0,2))
    correlation=matrix(rep(1/2,16),nrow=4)+1/2*diag(4)
    correlation[1:2,3:4]=NA
    correlation[3:4,1:2]=NA
    diag(correlation)=1
    t=0.5
    alpha=0.025
    as=function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))#spending function
    #========

    design <- cer_design(
         correlation=correlation,
         weights=weights,
         alpha=alpha,
         test_m=m,
         alpha_spending_f=as,
         prep_t=1/2)

    hyp_bound_1_pairs <- list(  #see table 6 in paper
        "1,1,1,1" = c(0.000782, 0.000782, 0, 0),
        "0,1,1,1" = c(0, 0.001144, 0.000381, 0),
        "1,0,1,1" = c(0.001144,0, 0, 0.000381),
        "1,1,0,1" = c(0.000782,0.000782,0,0),
        "1,1,1,0" = c(0.000782,0.000782,0,0),
        "0,0,1,1" = c(0, 0, 0.000782, 0.000782),
        "0,1,0,1" = c(0,0.001525, 0,0),
        "0,1,1,0" = c(0,0.001144,0.000381, 0),
        "1,0,0,1" = c(0.001144, 0, 0, 0.000381),
        "1,0,1,0" = c(0.001525, 0, 0, 0),
        "1,1,0,0" = c(0.000782, 0.000782, 0, 0),
        "0,0,0,1" = c(0, 0, 0, 0.001525),
        "0,0,1,0" = c(0, 0, 0.001525,0),
        "0,1,0,0" = c(0, 0.001525, 0, 0),
        "1,0,0,0" = c(0.001525, 0, 0, 0)
    )

    expect_equal(dim(design$bounds_1), c(15, 4))
    for (i in 1:dim(design$bounds_1)[1]) {
        hyp <- paste(design$hyp_matrix[i,], collapse = ",")
        expect_equal(round(design$bounds_1[i,], 6), hyp_bound_1_pairs[[hyp]])
    }

    hyp_bound_2_pairs <- list(  #see table 6 in paper
        "1,1,1,1" = c(0.0132, 0.0132, 0, 0),
        "0,1,1,1" = c(0, 0.0183, 0.00610, 0),
        "1,0,1,1" = c(0.0183,0, 0, 0.00610),
        "1,1,0,1" = c(0.0132,0.0132,0,0),
        "1,1,1,0" = c(0.0132,0.0132,0,0),
        "0,0,1,1" = c(0, 0, 0.0132, 0.0132),
        "0,1,0,1" = c(0,0.0245, 0,0),
        "0,1,1,0" = c(0,0.0183,0.00610, 0),
        "1,0,0,1" = c(0.0183, 0, 0, 0.00610),
        "1,0,1,0" = c(0.0245, 0, 0, 0),
        "1,1,0,0" = c(0.0132, 0.0132, 0, 0),
        "0,0,0,1" = c(0, 0, 0, 0.0245),
        "0,0,1,0" = c(0, 0, 0.0245,0),
        "0,1,0,0" = c(0, 0.0245, 0, 0),
        "1,0,0,0" = c(0.0245, 0, 0, 0)
    )
    
    expect_equal(dim(design$bounds_2), c(15, 4))
    for (i in 1:dim(design$bounds_2)[1]) {
        hyp <- paste(design$hyp_matrix[i,], collapse = ",")
        expect_equal(round(design$bounds_2[i,], 4), hyp_bound_2_pairs[[hyp]])
    }

})

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
    expect_s3_class(design, c("cer_design", "adagraph_design"))
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