#' Adapt the trial design after the interim test
#' 
#' @param design A cer_design object
#' @param weights New weights vector
#'  Note that the lenght should be the same as in the prespecified design
#'  For dropping hypotheses, set the according weights to 0 or use [cer_drop_hypotheses]
#' @param test_m Adapted test matrix defining the graph for the closed test procedure to test the hypotheses
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
    if (any(!is.na(weights))) {
        design$ad_weights <- weights
    } else if (all(is.null(design$ad_weights))) {
        design$ad_weights <- design$weights
    } 
    if (any(!is.na(test_m))) {
        design$ad_test_m <- test_m
    } else if (is.null(design$ad_test_m)) {
        design$ad_test_m <- design$test_m
    }
    if (any(!is.na(t))) {
        design$ad_t <- t
    } else if (is.null(design$ad_t)) {
        design$ad_t <- design$t
    }

    if (any(!is.na(weights)) || any(!is.na(test_m))) {
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
#' design <- cer_interim_test(design, c(0.1, 0.02))
#' 
#' design <- cer_drop_hypotheses(design, c(TRUE, FALSE))
#' design
cer_drop_hypotheses <- function(
    design,
    hypotheses
) {
   hypotheses <- ifelse(hypotheses, 0, 1)
   hyp_index <- which(sapply(asplit(design$hyp_matrix, 1), function(x) all(x == hypotheses)))
   if (!is.null(design$ad_weights_matrix)) {
    weights <- design$ad_weights_matrix[hyp_index, ]
   } else {
    weights <- design$weights_matrix[hyp_index, ]
   }

   if (any(!is.na(design$ad_test_m))) {
    test_m <- design$ad_test_m
   } else {
    test_m <- design$test_m
   }

   for (hyp in which(hypotheses == 0)) {
    for (i in 1:dim(test_m)[1]) {
        if (test_m[hyp, i] == 1 && test_m[i, hyp] == 1) {
            # no transfer of weight is possible, if it is only transfered to the now obselete hypothesis
            test_m[i, ] <- 0
        } else {
            feedback_loop <- test_m[i, hyp] * test_m[hyp, i]
                # ^ amount of weight that "gets stuck", i.e. would get transfered back to the deleted hypothesis
            test_m[i, ] <- (test_m[i, ] + test_m[i, hyp] * test_m[hyp,]) / (1 - feedback_loop)
        }
    }
    test_m[hyp, ] <- 0
    test_m[ , hyp] <- 0
    diag(test_m) <- 0
   }
   cer_adapt(design, weights = weights, test_m = test_m)
}

#' Adapt a cer design by dropping hypotheses, with a simplified strategy for redistributing weights
#'
#' The weights of the dropped hypotheses are set to 0 and distributed proportionally to the previous weight to the other hypotheses.
#' This means the weights "stay the same", they are only adapted to still sum up to 1.
#' Note that this method does not lead to an adapted graph that is coherent with the actual weight distribution, this may lead to problems down the line
#' 
#' @param design cer_design object
#' @param hypotheses vector of booleans indicating for each hypotheses if it should be dropped
#'
#' @return design with specified hypotheses dropped (so TRUE means the hypothesis is dropped)
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
#' design <- cer_interim_test(design, c(0.1, 0.02))
#' 
#' design <- cer_alt_drop_hypotheses(design, c(TRUE, FALSE))
#' design
cer_alt_drop_hypotheses <- function(
    design,
    hypotheses
) {
    weights <- design$weights
    weights[hypotheses] <- 0
    weights <- weights / sum(weights)
    weights_matrix <- design$weights_matrix
    weights_matrix[, hypotheses] <- 0
    weights_matrix <- t(apply(weights_matrix, 1, function(row) {
        if (sum(row) == 0) {
            return(row)
        } else {
            return(row / sum(row))
        }
    }))

    design$ad_weights <- weights
    design$ad_weights_matrix <- weights_matrix
    design
}

