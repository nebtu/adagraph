#' Check if a design is consonant
#'
#' For some CER design, see if the defined test sequence is consonant, either
#' from just it's graph and weights, also using the correlation structure, or
#' after some adaption.
#'
#' @param design A cer_design object
#' @param adapted Boolean, use the adapted or original values
#' @param use_weights Boolean, if TRUE uses the weights instead of the actual
#'   bounds. This is the same when no correlation structure is used, and else
#'   checks consonance of the counterfactual where the graph used is the same,
#'   but no use of correlation structure is made
#' @param stage For which stage should the consonance be tested? Can be one of
#' "both", "interim", "final". No effect if weights = TRUE.
#'
#' @return A boolean value, TRUE if the design (for the specified weights) is
#' consonant, else FALSE
#' @export
#'
#' @details
#' For every combination of hypothesis of a design, a intersection hypothesis is
#' being built that is keeping the specified alpha level, and only when all
#' intersection hypothesis holding a specific hypothesis are rejected, that
#' hypothesis is rejected. Consonance now means that whenever a intersection
#' hypothesis is rejected, at least one intersection hypothesis that is a
#' superset of this one is rejected. If a design is not consonant, it is in some
#' sense not optimal
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
#' check_consonance(design)
#' check_consonance(design, stage = "final")
check_consonance <- function(
  design,
  adapted = design$adaptions,
  stage = c("both", "interim", "final"),
  use_weights = FALSE
) {
  stage <- rlang::arg_match(stage)
  if (!rlang::is_bool(use_weights)) {
    cli::cli_abort(
      "Argument `use_weigths` must be boolean, either TRUE or FALSE.",
      "i" = "`use_weights` was {use_weights}.",
      class = "adagraph_invalid_argument"
    )
  }
  if (!design$adaptions && adapted) {
    cli::cli_abort(
      "No adaptions present",
      "i" = "Design was not adapted, can not perform consonance check on adapted design",
      class = "adagraph_no_adaption"
    )
  }

  if (use_weights) {
    if (adapted) {
      bounds <- design$ad_weights_matrix
    } else {
      bounds <- design$weights_matrix
    }
    return(all(.consonance_intersec(bounds, design$hyp_matrix)))
  } else {
    results <- c(TRUE, TRUE)
    if (stage %in% c("both", "interim")) {
      results[1] <- all(.consonance_intersec(
        design$bounds_1,
        design$hyp_matrix
      ))
    }
    if (stage %in% c("both", "final")) {
      if (adapted) {
        bounds <- design$ad_bounds_2
      } else {
        bounds <- design$bounds_2
      }
      results[2] <- all(.consonance_intersec(bounds, design$hyp_matrix))
    }
    return(all(results))
  }
}

# checks consonance on each intersection hypothesis
# where consonance means that rejection of this intersection hypothesis implies
# rejection of at least one hypothesis that is a superset of this one
# ignores the possibility of any bound being 1
.consonance_intersec <- function(
  bounds,
  hyp_matrix
) {
  vapply(
    seq_len(nrow(bounds)),
    \(idx) {
      J <- (hyp_matrix[idx, ] == 1)
      if (sum(J) == 1) {
        return(TRUE)
      }

      supersets <- which(rowSums(hyp_matrix[, !J, drop = FALSE]) == 0)
      supersets <- setdiff(supersets, idx) # only strict supersets, exclude current idx

      bounds_J <- bounds[idx, J, drop = FALSE] #note that nothing here is 0 by defintion of J
      bounds_super <- bounds[supersets, J, drop = FALSE]

      # for each p-value (each hypothesis), assume this is the one that lead to
      # the rejection. Is any superset also being rejected?
      all(sapply(seq_along(sum(J)), \(j) {
        any(bounds_super[, j, drop = FALSE] >= bounds_J[j])
      }))
    },
    logical(1)
  )
}
