
new_adagraph_design <- function(
    correlation=matrix(),
    weigths=double(),
    alpha=double(),
    k=int(),
    hypMatrix=matrix(),
    weightsMatrix=matrix(),
    closedMatrix=matrix()
) {
    design <- list(
        correlation=correlation,
        weigths=weights,
        alpha=alpha,
        hypMatrix=hypMatrix,
        weightsMatrix=weightsMatrix,
        closedMatrix=closedMatrix
    )
    structure(
        design,
        k = k,
        class = "adagraph_design"
    )
}

#' Make a new (generic) trial design
#'
#' @param correlation Correlation matrix describing the structure of the correlations between the different hypotheses, use NA for uncorrelated
#' @param weights List of weights, measuring how important each hypothesis is
#' @param alpha Single number, measuring what total alpha should be spent on the FWER
#'
#' @return An object of class adagraph_design
#' @import gMCPLite
#' @export
#'
#' @examples
#' design <- adagraph_design(
#'  correlation=rbind(H1=c(1, NA),
#'                    H2=c(NA, 1)),
#'  weights=c(2/3, 1/3),
#'  alpha=0.05,
#'  test_m=rbind(c(0, 1),
#'               c(1, 0)))
#' design
adagraph_design <- function(
    correlation=matrix(),
    weights=double(),
    alpha=double(),
    test_m=matrix()
) {
    if (dim(correlation)[1] == dim(correlation)[2]) {
        k <- dim(correlation)[1]
    } else {
        stop(
            "correlation has to be a quadratic matrix")
    }
    if (length(weights) != k) {
        stop("Need exactly one weight per hypothesis")
    } else if ((dim(test_m)[1] != dim(test_m)[2]) | (dim(test_m)[1] != k)) {
       stop("test_m needs to be a quadratic matrix with dimension same as the number of hypotheses")
    }
    graph <- new("graphMCP", m=test_m, weights=weights) 
    temp <- generateWeights(graph)
    hypMatrix <- temp[,1:k]
    weightsMatrix <- temp[,(k+1):(2*k)]
    closedMatrix=matrix(NA,nrow=2^(k-1),ncol=k)
    for (i in 1:k) {
        # Fill the result matrix with row indices
        closedMatrix[,i]=which(hypMatrix[,i]==1)
    }
    new_adagraph_design(
        correlation = correlation,
        weigths = weights,
        alpha = alpha,
        k = k,
        hypMatrix = hypMatrix,
        weightsMatrix = weightsMatrix,
        closedMatrix = closedMatrix
    )
}
