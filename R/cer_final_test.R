
cer_final_test <- function(
    design,
    p_values
) {
    k <- attr(design, "k")
    intersection_rej <- sapply(1:(2^k-1), function(i) {
        any(p_values < design$ad_bounds_2[i,]) | (design$intersection_rej_interim[i] == 1)
        #intersection hypotheses is rejected if any second stage p-value is below the adjusted bounds
        #or it was rejected at the interim already
    })
    
    rej<- sapply(1:k, function(i) {
        all(intersection_rej[design$closed_matrix[,i]])
    })

    design$rej<- rej
    design
}