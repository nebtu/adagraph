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
#' #simple non-parametric bonferroni
#' cer_prep_bounds(
#'     correlation = rbind(c(1,0), c(0,1)),
#'     weights = c(0.5,0.5),
#'     alpha = c(0.001525323, 0.025),
#'     t = 0.5)
#'
#' #weighted bonferroni with correlation 0.5
#' cer_prep_bounds(
#'     correlation = rbind(c(1,0.5), c(0.5,1)),
#'     weights = c(2/3,1/3),
#'     alpha = c(0.001525323, 0.025),
#'     t = 0.5)
cer_prep_bounds <- function(correlation, weights, alpha, t) {
  I <- which(weights > 0)

  if (length(I) == 0) {
    return(list(
      bounds_1 = 0 * weights,
      bounds_2 = 0 * weights,
      cJ1 = 0,
      cJ2 = 0
    ))
  }

  pos_weights <- weights[I]
  correlation <- correlation[I, I, drop = FALSE]

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
      return(
        1 -
          mvtnorm::pmvnorm(
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
    sum(sapply(conn, comp_err_1, cJ1 = cJ1))
  }

  cJ1 <- stats::uniroot(
    function(cJ1) {
      err_1(cJ1) - alpha[1]
    },
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

    return(
      1 -
        mvtnorm::pmvnorm(
          lower = -Inf,
          upper = c(
            stats::qnorm(1 - comp_weights * cJ1),
            stats::qnorm(1 - comp_weights * cJ2)
          ),
          corr = combined_comp_corr,
          algorithm = algorithm
        )[1]
    )
  }

  err_2 <- function(cJ1, cJ2) {
    sum(sapply(conn, comp_err_2, cJ1 = cJ1, cJ2 = cJ2))
  }

  cJ2 <- stats::uniroot(
    function(cJ2) err_2(cJ1, cJ2) - alpha[2],
    c((alpha[2] - alpha[1]) * 0.999, alpha[2] / max(pos_weights)),
    tol = getOption("adagraph.precision")
  )$root

  return(list(
    bounds_1 = cJ1 * weights,
    bounds_2 = cJ2 * weights,
    cJ1 = cJ1,
    cJ2 = cJ2
  ))
}

#' Get the Conditional Error Rate for a intersection of hypotheses
#'
#' Gives the CER (condtional error rate) for a given set of hypotheses, with arbitrary
#' weights and correlation between the hypotheses.
#' This is an upper bound on the probabilty of rejecting all the hypotheses
#' with weight greater 0 under the null hypothesis conditional on the stage one
#' data, assuming we reject whenever a final p-value is smaller than cJ2 * weight
#'
#' Note that if the correlation between some values is unkown, the result may be
#' greater than 1, see also the examples
#'
#' @param p_values vector of stage 1 p-values for the hypothesis
#' @param weights weight to give to each hypothesis, ignoring all with weight 0
#' @param cJ2 factor used for deciding if the hypothesis should be rejected
#' @param correlation matrix describing a potential known correlation structure
#'   between some hypotheses. Use NA for unkown correlations
#' @param t information time fraction at which the interim test is performed
#'
#' @return a single number greater than 0, the CER
#'
#' @export
#'
#' @examples
#' #the CER is high (even >1) if the p_values of the first stage are already low
#' get_cer(
#'  c(0.01, 0.01, 0.9, 0.9),
#'  c(1, 1, 0, 0),
#'  0.05,
#'  matrix(rep(NA, 16), nrow = 4),
#'  0.5
#' )
#'
#' #and lower otherwise
#' get_cer(
#'  c(0.01, 0.01, 0.9, 0.9),
#'  c(0, 0, 1, 1),
#'  0.05,
#'  matrix(rep(NA, 16), nrow = 4),
#'  0.5
#' )
get_cer <- function(
  p_values,
  weights,
  cJ2,
  correlation,
  t
) {
  I <- weights > 0
  pos_weights <- weights[I]
  correlation <- correlation[I, I, drop = FALSE]
  p_values <- p_values[I]
  if (length(t) == 1) {
    t <- rep(t, length(weights))
  }
  t <- t[I]

  if (length(correlation) > 1) {
    conn <- gMCPLite:::conn.comp(correlation)
  } else {
    conn <- 1
  }

  .get_cer(
    p_values,
    bounds = pos_weights * cJ2,
    correlation,
    t,
    conn
  )
}

#this internal function does the same as get_cer, but assumes that every weight
#is greater than 0 and that the connected component is already available
.get_cer <- function(
  p_values,
  bounds,
  correlation,
  t,
  conn
) {
  algorithm <- mvtnorm::Miwa(
    steps = getOption("adagraph.miwa_steps"),
    checkCorr = FALSE,
    maxval = getOption("adagraph.miwa_maxval")
  )

  # compute cer for one connected compononent of the correlation graph
  comp_cer <- function(conn_indices) {
    comp_bounds <- bounds[conn_indices]
    comp_p_values <- p_values[conn_indices]
    comp_t <- t[conn_indices]
    upper <- ifelse(
      #if the boundary is 1 or greater, we will reject, even if p == 1
      #as.vector is necessary to remove attributes, else upper is not accepted
      #by pmvnorm
      as.vector(comp_bounds >= 1),
      -Inf,
      (stats::qnorm(1 - pmin(1, comp_bounds)) -
        stats::qnorm(1 - comp_p_values) * sqrt(comp_t)) /
        sqrt(1 - comp_t)
    )
    if (length(conn_indices) == 1) {
      cer <- 1 - stats::pnorm(upper)
    } else {
      comp_corr <- correlation[conn_indices, conn_indices]
      cer <- 1 -
        min(
          mvtnorm::pmvnorm(
            lower = -Inf,
            upper = upper,
            corr = comp_corr,
            algorithm = algorithm
          )[1],
          1 # in rare cases, pmvnorm returns values greater than 1
        )
    }
    return(cer)
  }

  sum(sapply(conn, comp_cer))
}
