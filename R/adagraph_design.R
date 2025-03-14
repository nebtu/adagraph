
new_adagraph_design <- function(
    correlation=matrix(),
    weights=double(),
    alpha=double(),
    test_m=matrix(),
    ...,
    class = character()
) {
    k <- dim(correlation)[1]

    int_hyp <- get_intersection_hypotheses(weights, test_m)

    design <- list(
        correlation=correlation,
        weights=weights,
        alpha=alpha,
        hyp_matrix=int_hyp$hyp_matrix,
        weights_matrix=int_hyp$weights_matrix,
        closed_matrix=int_hyp$closed_matrix,
        test_m=test_m
    )
    structure(
        design,
        k = k,
        class = c(class, "adagraph_design")
    )
}

get_intersection_hypotheses <- function(weights, test_m) {
    k <- length(weights)
    if (k==1) {
        hyp_matrix <- cbind(1)
        weights_matrix <- cbind(weights)
    } else {
        #generate weights for all sub-hypotheses
        temp <- gMCPLite::generateWeights(test_m, weights)
        hyp_matrix <- temp[,1:k]
        weights_matrix <- temp[,(k+1):(2*k)]
    }
    closed_matrix=matrix(NA,nrow=2^(k-1),ncol=k)
    for (i in 1:k) {
        # Fill the result matrix with row indices
        closed_matrix[,i]=which(hyp_matrix[,i]==1)
    }
    list(
        hyp_matrix = hyp_matrix,
        weights_matrix = weights_matrix,
        closed_matrix = closed_matrix
    )
}


validate_adagraph_design_params <- function(
    correlation=matrix(),
    weights=double(),
    alpha=double(),
    test_m=matrix(),
    call=rlang::caller_env()
) {
    if (!is.matrix(correlation)) {
        cli::cli_abort("correlation has to be a matrix.",
                        class = "invalid_argument_correlation")
    } else if (dim(correlation)[1] != dim(correlation)[2]) {
        cli::cli_abort("correlation has to be a quadratic matrix.",
                       "x" = "correlation has dimenstion {dim(correlation)[1]} x {dim(correlation)[2]}.",
                       class = "invalid_argument_correlation")
    } 
    k <- dim(correlation)[1]
    if (length(weights) != k) {
        cli::cli_abort("Need exactly one weight per hypothesis.",
                        "i" = "There are {k} hypotheses.",
                        "x" = "You suppplied {lenght(weights) weights}.",
                        class="invalid_argument_weights")
    } else if (!is.numeric(alpha)) {
        cli::cli_abort("alpha has to be numeric.",
                       "x" = "alpha is {typeof(alpha)}.",
                       class = "invalid_argument_alpha")
    } else if (alpha<0 | alpha>1) {
        cli::cli_abort("alpha has to be between 0 and 1",
                        "x" = "alpha is {alpha}.",
                       class = "invalid_argument_alpha")
    } else if (!is.matrix(test_m)) {
        cli::cli_abort("test_m has to be a matrix.",
                        class = "invalid_argument_test_m")
    } else if (dim(test_m)[1] != dim(test_m)[2] | dim(test_m)[1] != k) {
        cli::cli_abort("test_m has to be a quadratic matrix with lenght/height equal to number of hypotheses.",
                       "i" = "There are {k} Hypotheses",
                       "x" = "test_m has dimenstion {dim(correlation)[1]} x {dim(correlation)[2]}.",
                       class = "invalid_argument_test_m")
    }
    #TODO: test that weights sum to 1 (or less than one?)
}

#' Make a new (generic) trial design
#'
#' @param correlation Correlation matrix describing the structure of the correlations
#'                    between the different hypotheses, use NA for uncorrelated
#' @param weights List of weights, measuring how important each hypothesis is
#' @param alpha Single number, measuring what total alpha should be spent on the FWER
#' @param test_m Transition matrix describing the graph for the closed test procedure to test the hypotheses
#'
#' @return An object of class adagraph_design
#' @export
#'
#' @examples
#' design <- adagraph_design(
#'  correlation=rbind(H1=c(1, NA),
#'                    H2=c(NA, 1)),
#'  weights=c(2/3, 1/3),
#'  alpha=0.05,
#'  test_m=rbind(c(0, 1),
#'               c(1, 0)))
#' design
adagraph_design <- function(
    correlation=matrix(),
    weights=double(),
    alpha=double(),
    test_m=matrix()
) {
    validate_adagraph_design_params(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        test_m = test_m
    )
    new_adagraph_design(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        test_m = test_m
    )
}
