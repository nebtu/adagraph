#' Adapt the trial design after the interim test
#'
#' @param design A cer_design object
#' @param weights New weights vector
#'  Note that the lenght should be the same as in the prespecified design
#'  For dropping hypotheses, set the according weights to 0 or use [cer_drop_hypotheses()]
#' @param test_m Adapted test matrix defining the graph for the closed test procedure to test the hypotheses
#' @param t adapted information fraction at which the first stage test occured.
#'  Note that this can now be a vector with a different value for different hypotheses or a single value
#' @param correlation adapted correlation matrix
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
  t = NA,
  correlation = NA
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
  if (any(!is.na(correlation))) {
    design$ad_correlation <- correlation
  } else if (is.null(design$ad_correlation)) {
    design$ad_correlation <- design$correlation
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
#' However, the time fraction is not adapted, this needs to be done manually if desired.
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
  if (is.null(design$keep_hyp)) {
    design$keep_hyp <- rep(TRUE, attr(design, "k"))
  }
  design$keep_hyp[hypotheses] <- FALSE
  hypotheses <- ifelse(hypotheses, 0, 1)
  hyp_index <- which(sapply(
    asplit(design$hyp_matrix, 1),
    function(x) all(x == hypotheses)
  ))
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
        test_m[i, ] <- (test_m[i, ] + test_m[i, hyp] * test_m[hyp, ]) /
          (1 - feedback_loop)
      }
    }
    test_m[hyp, ] <- 0
    test_m[, hyp] <- 0
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

  design$keep_hyp <- !hypotheses
  design$ad_weights <- weights
  design$ad_weights_matrix <- weights_matrix
  design
}

#' Adjust bounds after changing some design parameters
#'
#' This function calculates the new bounds for the p-values for the final test.
#' It should be run once after finishing all adaptions after the interim test.
#'
#' @param design A cer_design object
#'
#' @return A cer_design object, with the new bounds calculated
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
#' design <- cer_adapt_bounds(design)
#'
#' design
cer_adapt_bounds <- function(design) {
  to_test <- ((rowSums(design$ad_weights_matrix) > 0) &
    (rowSums(design$hyp_matrix[, design$rej_interim, drop = FALSE]) == 0))

  get_ad_cJ2 <- function(index) {
    if (sum(design$ad_weights_matrix[index, ] > 0) <= design$cer_vec[index]) {
      Inf
    } else {
      stats::uniroot(
        function(ad_cJ2) {
          get_cer(
            p_values = design$p_values_interim,
            weights = design$ad_weights_matrix[index, ],
            cJ2 = ad_cJ2,
            correlation = design$ad_correlation,
            t = design$ad_t
          ) -
            design$cer_vec[index]
        },
        c(0, 1 / max(design$ad_weights_matrix[index, ])),
        tol = getOption("adagraph.precision")
      )$root
    }
  }

  ad_cJ2 <- numeric(dim(design$ad_weights_matrix)[1])
  ad_cJ2[to_test] <- lapply(which(to_test), get_ad_cJ2)
  ad_cJ2 <- simplify2array(ad_cJ2)

  design$ad_cJ2 <- ad_cJ2
  design$ad_bounds_2 <- apply(design$ad_weights_matrix, 2, function(w) {
    ad_cJ2 * w
  })

  design
}
