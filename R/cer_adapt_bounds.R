#' Adjust bounds after changing some design parameters
#'
#' This function calculates the new bounds for the p-values for the final test.
#' It should be run once after finishing all adaptions after the interim test,
#' if the `adapt_bounds` option was not true for the last adaption anyway.
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
  #get bounds only for hypotheses that still have any weight and that aren't
  #rejected already
  to_test <- (rowSums(design$ad_weights_matrix) > 0) &
    !design$intersection_rej_interim

  get_ad_cJ2 <- function(index) {
    if (sum(design$ad_weights_matrix[index, ] > 0) <= design$cer_vec[index]) {
      # If the number of hypotheses is smaller than the cer, we can not get the
      # new bound computationally, and reject always
      Inf
    } else {
      I <- which(design$ad_weights_matrix[index, ] > 0)
      weights <- design$ad_weights_matrix[index, ]
      correlation <- design$ad_correlation[I, I, drop = FALSE]
      p_values <- design$p_values_interim[I]

      t <- design[["ad_t"]]
      if (length(t) == 1) {
        t <- rep(t, length(weights))
      }
      weights <- weights[I]
      t <- t[I]

      #### Connected components for the reduced correlation matrix
      # Create mapping from old indices to new indices
      old_to_new <- stats::setNames(seq_along(I), I)

      # Filter and remap components
      components <- lapply(design$correlation_components, function(comp) {
        comp_in_subgraph <- comp[comp %in% I]

        # If this component has vertices in the subgraph, remap them
        if (length(comp_in_subgraph) > 0) {
          unname(old_to_new[as.character(comp_in_subgraph)])
        } else {
          NULL
        }
      })

      # Remove empty components (those with no vertices in subgraph)
      components <- Filter(Negate(is.null), components)

      stats::uniroot(
        function(ad_cJ2) {
          .get_cer(
            p_values = p_values,
            bounds = weights * ad_cJ2,
            correlation = correlation,
            t = t,
            conn = components
          ) -
            min(length(components), design$cer_vec[index])
        },
        c(0, 1 / max(weights)),
        tol = getOption("adagraph.precision")
      )$root
    }
  }

  ad_cJ2 <- numeric(dim(design$ad_weights_matrix)[1])
  ad_cJ2[to_test] <- lapply(which(to_test), get_ad_cJ2)
  ad_cJ2 <- simplify2array(ad_cJ2)

  design$ad_cJ2 <- ad_cJ2
  design$ad_bounds_2 <- apply(design$ad_weights_matrix, 2, function(w) {
    #ifelse since 0 * Inf should be 0
    ifelse(w == 0, 0, ad_cJ2 * w)
  })
  design$ad_bounds_outdated <- FALSE

  design
}
