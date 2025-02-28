#' Adapt the trial design after the interim test
#' 
#' @param design A cer_design object
#' @param weights New weights matrix.
#'  Note that the dimensions should be the same as in the prespecified design
#'  For dropping hypotheses, set the according weights to 0 or use [cer_drop_hypotheses]
#' @param t new list of information fractions (or single information fraction) at which the interim test was conducted
#'
#' @return An object of class cer_design, with the adaptions applied.
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
#' #note that it is not necessary to do an interim test before,
#' #but that an interim test will only be done with the prespecified parameters
#' design <- cer_interim_test(design, c(0.1, 0.02))
#' 
#' design <- cer_adapt(design, weights = c(1/3, 2/3))
#' design
cer_adapt <- function(
    design,
    weights = NA,
    test_m = NA,
    t = NA
) {
    if (!is.na(weights)) {
        design$ad_weights <- weights
    } else if (!is.null(design$ad_weights)) {
        design$ad_weights <- design$weights
    } 
    if (!is.na(test_m)) {
        design$ad_test_m <- test_m
    } else if (!is.null(design$ad_test_m)) {
        design$ad_test_m <- design$test_m
    }
    if (!is.na(t)) {
        design$ad_t <- t
    } else if (!is.null(design$ad_t)) {
        design$ad_t <- design$t
    }

    if (!is.na(weights) or !is.na(test_m)) {
        int_hyp <- get_intersection_hypotheses(design$ad_weights, design$ad_test_m)
        design$ad_weights_matrix <- int_hyp$weights_matrix
    } else if (is.null(design$ad_weights_matrix)) {
        design$ad_weights_matrix <- design$weights_matrix
    }

    design
}

#' Adapt a cer design by dropping hypotheses
#'
#' The weights of the dropped hypotheses are set to 0 and distributed according to the prespecified graph.
#' 
#' @param design cer_design object
#' @param hypotheses vector of booleans indicating for each hypotheses if it should be dropped
#'
#' @return design with specified hypotheses dropped (so TRUE means the hypothesis is dropped)
#'@export
#'
#'@examples
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
#' design <- cer_interim_test(design, c(0.1, 0.02))
#' 
#' design <- cer_drop_hypotheses(design, c(TRUE, FALSE))
#' design
cer_drop_hypotheses <- function(
    design,
    hypotheses
) {
   hyp <- as.integer(!hypotheses)
   intersection_hyp <- which(asplit(design$hyp_matrix, 1) == hyp)
   weights <- design$weights_matrix[intersection_hyp]
   cer_adapt(design, weights = weights)
}