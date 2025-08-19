#' Test a cer design for early rejection of hypotheses and calculate the CER for adaptions
#'
#' @param design A cer_design object
#' @param p_values A list of p-values for the hypotheses
#'
#' @return a cer_design object, which now also includes the rejection status of the hypotheses after the final test
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
#' design <- cer_interim_test(design, c(0.001, 0.02))
#'
#' design <- cer_drop_hypotheses(design, c(TRUE, FALSE))
#' design <- cer_adapt_bounds(design)
#'
#' design <- cer_final_test(design, c(NA, 0.01))
#' design$rej
cer_final_test <- function(
  design,
  p_values
) {
  if (!design$interim_test) {
    cli::cli_abort(
      "Attempting to do a final test before interim test.",
      class = "wrong_sequence_no_interim_test"
    )
  }
  if (design$ad_bounds_outdated) {
    cli::abort(
      "There have been adaptions without adjusting the bounds for rejecting hypotheses.",
      "i" = "Either set adapt_bounds to true for the last adaption to the trial, or use the adapt_bounds function after all adaptions are done.",
      class = "wrong_sequence_bounds_outdated"
    )
  }
  if (design$final_test) {
    cli::cli_warn(
      "Overwriting previous final test.",
      class = "overwrites_final_result"
    )
  }

  #hypotheses which are no further tested can not be rejected
  p_values[is.na(p_values)] <- 1
  k <- attr(design, "k")

  intersection_rej <- pmax(
    design$intersection_rej_interim,
    matrixStats::colMins(p_values - t(design$ad_bounds_2)) < 0,
    na.rm = TRUE
  )

  rej <- sapply(1:k, function(i) {
    all(intersection_rej[design$closed_matrix[, i]])
  })

  design$rej <- rej
  design$p_values_final <- p_values
  design$final_test <- TRUE
  design
}
