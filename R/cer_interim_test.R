#' Test a cer design for early rejection of hypotheses and calculate the CER for adaptions
#'
#' @param design A cer_design object
#' @param p_values A list of p-values for the hypotheses
#' @param t The time fraction for the interim test, defaults to the t of the design
#'
#' @return a cer_design object, which now also includes the CER for each hypothesis and the rejection status of the hypotheses
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
#' design$rej_interim
#' design$cer_vec
cer_interim_test <- function(
  design,
  p_values,
  t = NA
) {
  if (is.na(t)) {
    t <- design$t
  } else {
    design$t <- t
  }
  rej_matrix <- (p_values < t(design$bounds_1))

  cer_vec <- mapply(
    function(weights, cJ2, rej) {
      if (any(rej)) {
        1
      } else {
        get_cer(p_values, weights, cJ2, design$correlation, t)
      }
    },
    asplit(design$weights_matrix, 1),
    design$cJ2,
    asplit(rej_matrix, 2)
  )

  if (design$seq_bonf) {
    rej_matrix[which(cer_vec > 1)] <- TRUE
  }

  intersection_rej <- apply(rej_matrix, 2, any)
  rej <- sapply(1:attr(design, "k"), function(i) {
    all(intersection_rej[design$closed_matrix[, i]])
  })

  design$p_values_interim <- p_values
  design$cer_vec <- cer_vec
  design$rej_interim <- rej
  design$intersection_rej_interim <- intersection_rej
  design
}
