
new_cer_design <- function(
    correlation=matrix(),
    weights=double(),
    alpha=double(),
    test_m=matrix(),
    alpha_spending_f=function() {},
    prep_t=double(),
    seqBonf=TRUE,
    parallelize=FALSE
) {
    design <- new_adagraph_design(
        correlation = correlation,
        weights = weights,
        alpha = alpha,
        test_m = test_m,
        class = "cer_design"
    )
    design$alpha_spending_f <- alpha_spending_f
    design$seqBonf <- seqBonf
    k <- attr(design, "k")

    prep_alpha_1 <- alpha_spending_f(alpha, prep_t)
    get_bounds <- function(index) {
        #this function takes the index instead of directly the weights because parallel
        #only provides mclapply, and no equivalent of apply
        weight_list = design$weights_matrix[index,]
        cer_prep_bounds(correlation, weight_list, c(prep_alpha_1, alpha), prep_t)
    }
    if (parallelize == TRUE) {
        boundslist <- parallel::mclapply(1:2^k-1, get_bounds)
    } else {
        boundslist <- lapply(1:(2^k - 1), get_bounds)
    }
    design$bounds_1 <- sapply(boundslist,`[[`, "bounds_1")
    design$bounds_2 <- sapply(boundslist,`[[`, "bounds_2")
    design$cJ1 <- sapply(boundslist,`[[`, "cJ1")
    design$cJ2 <- sapply(boundslist,`[[`, "cJ2")

    design
}
