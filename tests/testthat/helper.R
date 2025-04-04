
make_example_design <- function() {
    #=======
    m <- rbind(H1=c(0,1/2,1/2,0),
               H2=c(1/2,0,0,1/2),
               H3=c(0,1,0,0),
               H4=c(1,0,0,0))
    weights <- c(rep(1/2,2),rep(0,2))
    correlation=matrix(rep(1/2,16),nrow=4)+1/2*diag(4)
    correlation[1:2,3:4]=NA
    correlation[3:4,1:2]=NA
    diag(correlation)=1
    t=0.5
    alpha=0.025
    as=function(x,t) 2-2*stats::pnorm(stats::qnorm(1-x/2)/sqrt(t))#spending function
    #========

    design <- cer_design(
         correlation=correlation,
         weights=weights,
         alpha=alpha,
         test_m=m,
         alpha_spending_f=as,
         t=1/2)

    design
}