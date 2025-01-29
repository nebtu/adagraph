# Some functions used to test using the CER method
# The most convenient way to design and test a trial using the CER method
# is using the cer_design methods. The functions here are exported
# anyway to allow more granular application of the methods as well.

#' Calculate bounds for preplanned test
#' 
#' Calculates the bounds and cJ values for the p-values for the interim test and the planned final test
#'
#' @param correlation matrix describing the correlation structure of the hypotheses
#' @param weights list of weights of the given hypotheses, same length as hypotheses
#' @param alpha list of length 2 describing the amount of alpha spent that the test of the hypotheses
#'              should adhere to in the first and second stage respectively
#' @param t information fraction at which the first stage test is planned
#'
#' @return A list with the following elements:
#'  * bounds_1: vector of same length as weights with bounds for the first interim test
#'  * cJ1: number that gets multiplied by the weights to get bounds_1
#'  * bounds_1: vector of same lenght as weights with bounds for the planned final test
#'  * cJ2: number that gets multiplied by the weights to get bounds_2
#' 
#' @export
#'
#' @examples
#'  #simple non-parametric bonferroni
#'  cer_prep_bounds(
#'      correlation = rbind(c(1,0), c(0,1)),
#'      weights = c(0.5,0.5),
#'      alpha = c(0.001525323, 0.025),
#'      t = 0.5)
#' 
#'  #weighted bonferroni with correlation 0.5
#'  cer_prep_bounds(
#'      correlation = rbind(c(1,0.5), c(0.5,1)),
#'      weights = c(2/3,1/3),
#'      alpha = c(0.001525323, 0.025),
#'      t = 0.5)
cer_prep_bounds <- function(correlation, weights, alpha, t) {
    k <- dim(correlation)[1]
    I <- which(weights > 0)

    pos_weights <- weights[I]
    correlation <- correlation[I,I, drop=FALSE]

    conn <- gMCPLite:::conn.comp(correlation)

    algorithm <- mvtnorm::Miwa(
                    steps = getOption("adagraph.miwa_steps"),
                    checkCorr = FALSE,
                    maxval = getOption("adagraph.miwa_maxval")
    )

    #computes error for one connected component (i.e. parametric case)
    comp_err_1 <- function(conn_indices, cJ1) {
        comp_weights <- pos_weights[conn_indices]
        if (length(conn_indices) > 1) {
            comp_corr <- correlation[conn_indices, conn_indices]
            return(1 - mvtnorm::pmvnorm(
                lower = -Inf,
                upper = stats::qnorm(1 - comp_weights * cJ1),
                corr = comp_corr,
                algorithm = algorithm
                )[1]
            )
        } else {
            return(cJ1 * comp_weights)
        }
    }

    err_1 <- function(cJ1) {
        sum(sapply(conn, comp_err_1, cJ1=cJ1))
    }

    cJ1 <- stats::uniroot(
        function(cJ1) {err_1(cJ1) - alpha[1]},
        c(alpha[1] * 0.999, alpha[1] / max(pos_weights)),
        tol = getOption("adagraph.precision")
    )$root

    comp_err_2 <- function(conn_indices, cJ1, cJ2) {
        comp_weights <- pos_weights[conn_indices]
        comp_corr <- correlation[conn_indices, conn_indices]
        combined_comp_corr <- rbind(
            cbind(comp_corr, comp_corr * sqrt(t)),
            cbind(comp_corr * sqrt(t), comp_corr)
        )

        return(1 - mvtnorm::pmvnorm(
                lower = -Inf,
                upper = c(stats::qnorm(1 - comp_weights * cJ1), stats::qnorm(1 - comp_weights * cJ2)),
                corr = combined_comp_corr,
                algorithm = algorithm
            )[1]
        )
    }

    err_2  <- function(cJ1, cJ2) {
        sum(sapply(conn, comp_err_2, cJ1=cJ1, cJ2=cJ2))
    }

    cJ2 <- stats::uniroot(
        function(cJ2) err_2(cJ1, cJ2) - alpha[2],
        c((alpha[2] - alpha[1])*0.999, alpha[2] / max(pos_weights)),
        tol = getOption("adagraph.precision")
    )$root

    return(list(
        bounds_1 = cJ1 * weights,
        bounds_2 = cJ2 * weights,
        cJ1 = cJ1,
        cJ2 = cJ2
    ))
}

