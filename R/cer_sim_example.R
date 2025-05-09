#'@importFrom stats pnorm qnorm pt
get_sim_design <- function() {
    ws=.75# (weight given to secondary endpoint)
    wp=(1-ws)/3 # weight given to the other arms primary endpoint
    m <- rbind(H1=c(0,wp,wp,wp,ws,0,0,0),
            H2=c(wp,0,wp,wp,0,ws,0,0),
            H3=c(wp,wp,0,wp,0,0,ws,0),
            H4=c(wp,wp,wp,0,0,0,0,ws),
            H5=c(0,1/3,1/3,1/3,0,0,0,0),
            H6=c(1/3,0,1/3,1/3,0,0,0,0),
            H7=c(1/3,1/3,0,1/3,0,0,0,0),
            H8=c(1/3,1/3,1/3,0,0,0,0,0)
    )

    weights <- c(rep(1/4,4),rep(0,4))
    correlation <- matrix(rep(1/2,64),nrow=8)+1/2*diag(8)
    correlation[1:4,5:8] <- NA
    correlation[5:8,1:4] <- NA
    diag(correlation) <- 1
    t=0.5 # information time
    alpha <- 0.025 #overall alpha
    as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))#spending function

    design <- cer_design(
        correlation=correlation,
        weights=weights,
        alpha=alpha,
        test_m=m,
        alpha_spending_f=as,
        t=t
    )

    design
}

example_data_gen <- function(corr, eff, n1, n2) {
    mu <- c(eff, eff)

    corr_control <- rbind(
        c(1, corr),
        c(corr, 1)
    )

    corr_treatment <- rbind(
        c(1, 0, 0, 0, corr, 0, 0, 0),
        c(0, 1, 0, 0, 0, corr, 0, 0),
        c(0, 0, 1, 0, 0, 0, corr, 0),
        c(0, 0, 0, 1, 0, 0, 0, corr),
        c(corr, 0, 0, 0, 1, 0, 0, 0),
        c(0, corr, 0, 0, 0, 1, 0, 0),
        c(0, 0, corr, 0, 0, 0, 1, 0),
        c(0, 0, 0, corr, 0, 0, 0, 1)
    )

    cont_treat_association <- c(1, 1, 1, 1, 2, 2, 2, 2)

    data_gen_1 <- get_data_gen(corr_control, corr_treatment, cont_treat_association, mu, n1, n1)
    data_gen_2 <- get_data_gen_2(corr_control, corr_treatment, cont_treat_association, mu, n2, n2)

    return(c(data_gen_1, data_gen_2))

}

get_example_adaption <- function(futility, n1, n2, alt_drop=F) {
    function(design) {
        p <- design$p_values_interim[1:4]

        trt_rej <- design$rej_interim[1:4] & design$rej_interim[5:8]
        if (futility > 0) {
            drop_hyp <- (design$p_values_interim[1:4] > futility) | trt_rej
        } else {
            drop_hyp <- (design$p_values_interim[1:4] != min(design$p_values_interim[1:4][!trt_rej]))
        }

        trt2 <- which(!drop_hyp)
        ntrt <- length(trt2)
        I2 = c(trt2, trt2 + 4)

        tn2base = (n2 * 5) %/% (1 + ntrt) # compute 2nd stage sample size to the selected treatment
        tn2remainder=(n2 * 5) %% (1 + ntrt) # distribute sample size to remaining treatments
        tn2=rep(0,5) 
        tn2[c(1,1+trt2)]=tn2base
        tn2[c(1,1+trt2)][0:tn2remainder]=tn2base+1 # second stage per treatment sample size
        tt = n1 *.5 / (n1*.5 +tn2[1]*tn2[-1]/(tn2[1]+tn2[-1])) #adapted information fractions for the selected treatments
        th=c(tt,tt) # information fractions for all hypotheses (dropped get IF 1)
        si <- sqrt(1 / tn2[-1] + 1 / tn2[1]) # update the correlation matrix
        ctemp=1/tn2[1]/ outer(si, si) 
        corradapt=design$correlation
        corradapt[1:4,1:4]=ctemp
        corradapt[5:8,5:8]=ctemp
        diag(corradapt)=1 # corradapt has only for selected hypothesis a valid entry

        if (alt_drop) {
          design_adapted <- design|>
            cer_alt_drop_hypotheses(drop_hyp)
        } else {
          design_adapted <- design|>
            cer_drop_hypotheses(drop_hyp)
        }

        design_adapted |>
            cer_adapt(
                t = th,
                correlation = corradapt
            )
    }
}

run_example_trial <- function(
    runs1 = 10,
    runs2 = 100,
    n1 = 50,
    n2 = 50,
    corr = 0.8,
    eff = c(0,0,0,0),
    futility = 0.75,
    alt_drop = FALSE
) {
    design <- get_sim_design()
    dat <- example_data_gen(corr, eff, n1, n2)
    data_gen_1 <- dat[[1]]
    data_gen_2 <- dat[[2]]
    adapt_rule <- get_example_adaption(futility, n1, n2, alt_drop = alt_drop)
    sim_trial(design, runs1, runs2, adapt_rule, data_gen_1, data_gen_2)
}