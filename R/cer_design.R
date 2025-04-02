#' Internal function for cer_design
#' 
#' For documentation on how to generate cer_designs, see [cer_design].
#' The parameters for this fucntion are the same as in [cer_design], with the exception of 
#' @param correlation,weights,alpha,test_m,alpha_spending_f,t,seq_bonf,parallelize Same as for [cer_design]
#' @param class character, makes it possible to add subbclasses
#' @param ... additional parameters, not used
#'
#' @return An object of class c("cer_design", "adagraph_design"), whith the following elements: 
#'  * correlation: correlation matrix of the hypotheses, as given
#'  * weights: list of weights of the hypotheses, as given
#'  * alpha: overall FWER, as given
#'  * hyp_matrix: matrix of format #intersection-hypotheses x #hypotheses where 
#'      each row specifies which hypothosis is part of the given intersection hypotheses
#'  * weights_matrix: same format as hyp_matrix, but each row specifies
#'      the weights of the hypotheses for the given intersection hypothesis
#'  * closed_matrix: each of the #hypothesis columns specifies which
#'      intersection hypotheses need to be tested to reject the given hypothesis
#'  * test_m: transition matrix of the graph, as given
#'  * alpha_spending_f: alpha spending function, as given
#'  * seq_bonf: as given
#'  * t: as given
#'  * bounds_1: matrix of same format as hyp_matrix, where each row gives the bounds for rejection of the intersection hypothesis at the first stage
#'  * bounds_2: same as bounds_1, but for rejection at the second stage according to the preplanned design
#'  * cJ1: values used for calulation of bounds_1, bounds_1 := cJ1 * weights (with rowwise multiplication)
#'  * cJ2: as cJ1, but for bounds_2
#'  
new_cer_design <- function(
    correlation=matrix(),
    weights=double(),
    alpha=double(),
    test_m=matrix(),
    alpha_spending_f=function() {},
    t=double(),
    seq_bonf=TRUE,
    parallelize=FALSE,
    ...,
    class = character()
) {
    design <- new_adagraph_design(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        test_m = test_m,
        class = c(class, "cer_design")
    )
    design$alpha_spending_f <- alpha_spending_f
    design$seq_bonf <- seq_bonf
    design$t <- t
    design$parallelize <- parallelize
    k <- attr(design, "k")

    prep_alpha_1 <- alpha_spending_f(alpha, t)
    get_bounds <- function(index) {
        #this function takes the index instead of directly the weights because 
        # parallel only provides mclapply, and no equivalent of apply
        weight_list = design$weights_matrix[index,]
        cer_prep_bounds(correlation, weight_list, c(prep_alpha_1, alpha), t)
    }
    if (parallelize == TRUE) {
        boundslist <- parallel::mclapply(1:(2^k - 1), get_bounds)
    } else {
        boundslist <- lapply(1:(2^k - 1), get_bounds)
    }
    design$bounds_1 <- t(sapply(boundslist,`[[`, "bounds_1"))
    design$bounds_2 <- t(sapply(boundslist,`[[`, "bounds_2"))
    design$cJ1 <- t(sapply(boundslist,`[[`, "cJ1"))
    design$cJ2 <- t(sapply(boundslist,`[[`, "cJ2"))

    design
}

validate_cer_design_params <- function(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = test_m,
    alpha_spending_f = alpha_spending_f,
    t = t,
    seq_bonf = seq_bonf,
    parallelize = parallelize,
    call = rlang::caller_env()
) {
    validate_adagraph_design_params(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        test_m = test_m,
        call = call
    )
    if (!is.numeric(t)) {
        cli::cli_abort("t has to be numeric.",
                       "x" = "t is {typeof(t)}.",
                       class = "invalid_argument_t")
    } else if (t < 0 | t > 1) {
        cli::cli_abort("t has to be between 0 and 1",
                       "x" = "t is {t}.",
                       class = "invalid_argument_t")
    } else if (!is.logical(seq_bonf)) {
        cli::cli_abort("seq_bonf has to be a boolean.",
                       "x" = "seq_bonf is {typeof(seq_bonf)}.",
                       class = "invalid_argument_seq_bonf")
    } else if (!is.logical(parallelize)) {
        cli::cli_abort("parallelize has to be a boolean.",
                       "x" = "parallelize is {typeof(parallelize)}.",
                       class = "invalid_argument_parallelize")
    }
}

#' Make a new CER trial design
#'
#' Returns an object of class cer_design. 
#' This can be used for clinical trials with potential adaptions that are controlled
#' for using the conditional error method.
#' 
#' @param correlation Correlation matrix describing the structure of the correlations
#'                    between the different hypotheses, use NA for uncorrelated
#' @param weights List of weights, measuring how important each hypothesis is
#' @param alpha Single number, measuring what total alpha should be spent on the FWER
#' @param test_m Transition matrix describing the graph for the closed test procedure to test the hypotheses
#' @param alpha_spending_f alpha spending function, taking parameters alpha (for overall spent alpha) and t (information fraction at interim test)
#' @param t numeric between 0 and 1 specifing the planned time fraction for the interim test
#' @param seq_bonf to automatically reject hypotheses at the second stage if the sum of their PCER is greater 1
#' @param parallelize set TRUE to use parallization, for now only available on unix systems
#'
#' @return An object of class cer_design
#' @export
#'
#' @examples
#' as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))
#' design <- cer_design(
#'  correlation=rbind(H1=c(1, NA),
#'                    H2=c(NA, 1)),
#'  weights=c(2/3, 1/3),
#'  alpha=0.05,
#'  test_m=rbind(c(0, 1),
#'               c(1, 0)),
#'  alpha_spending_f=as,
#'  t=0.5)
#' 
#' design
cer_design <- function(
    correlation=matrix(),
    weights=double(),
    alpha=double(),
    test_m=matrix(),
    alpha_spending_f=function() {},
    t=double(),
    seq_bonf=TRUE,
    parallelize=FALSE
) {

    validate_cer_design_params(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        test_m = test_m,
        alpha_spending_f = alpha_spending_f,
        t = t,
        seq_bonf = seq_bonf,
        parallelize = parallelize
    )

    new_cer_design(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        test_m = test_m,
        alpha_spending_f = alpha_spending_f,
        t = t,
        seq_bonf = seq_bonf,
        parallelize = parallelize
    )
}