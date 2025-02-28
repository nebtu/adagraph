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
#' design$rej
#' design$cer_vec
cer_interim_test <- function(
    design,
    p_values,
    t = NA
) {
    if (is.na(t)) {
        t <- design$t
    }
    intersection_rej <- (p_values < t(design$bounds_1))

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
        asplit(intersection_rej, 2)
    )

    if (design$seq_bonf) {
        intersection_rej[which(cer_vec > 1)] <- TRUE
    }
    rej <- sapply(1:attr(design, "k"), function(i) {
        all(intersection_rej[i, design$closed_matrix[,i]]) #comparison in a matrix is column-wise, so t is needed
    })
    design$cer_vec <- cer_vec
    design$rej <- rej
    design
}

# gives cer for a single intersection hypothesis
get_cer <- function(
    p_values,
    weights,
    cJ2,
    correlation,
    t
) {
    I <- which(weights > 0)
    pos_weights <- weights[I]
    correlation <- correlation[I,I, drop=FALSE]
    p_values <- p_values[I]

    conn <- gMCPLite:::conn.comp(correlation)

    algorithm <- mvtnorm::Miwa(
                    steps = getOption("adagraph.miwa_steps"),
                    checkCorr = FALSE,
                    maxval = getOption("adagraph.miwa_maxval")
    )

    # compute cer for one connected compononent of the correlation graph
    comp_cer <- function(conn_indices) {
        comp_weights <- pos_weights[conn_indices]
        comp_p_values <- p_values[conn_indices]
        if (length(conn_indices) == 1) {
            cer <- 1 - stats::pnorm((stats::qnorm(1 - min(1, comp_weights * cJ2)) - stats::qnorm(1 - comp_p_values) * sqrt(t)) / sqrt(1 - t)) 
        } else {
            comp_corr <- correlation[conn_indices, conn_indices]
            cer <- 1 - mvtnorm::pmvnorm(
                lower = -Inf,
                upper = (stats::qnorm(1 - pmin(1, comp_weights * cJ2)) - stats::qnorm(1 - comp_p_values) * sqrt(t)) / sqrt(1 - t),
                corr = comp_corr,
                algorithm = algorithm
            )[1]
        }
        return(cer)
    }

    sum(sapply(conn, comp_cer))
}