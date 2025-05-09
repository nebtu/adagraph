#' Run a series of simulated trials according to a specified design, with 
#'
#' @param design cer_design object to be used for the simulation
#' @param runs1 Number of trials to run in the first stage
#' @param runs2 Number of second stage to run for every first stage trial
#' @param adapt_rule function that takes a cer_design object after the first interim test and
#'                  returns the same object with appropiate adaptions
#' @param data_gen_1 function for generating first stage data, see details
#' @param data_gen_2 function for generating second stage data, see details
#' @param include_designs boolean indicating whether to include the designs objects in the output
#'
#' @details
#' data_gen_1 should take a single argument, the number of trials to simulate
#' The function is only called a single time and
#' should return a matrix or dataframe with each row being the p-values for a single trial.
#' 
#' data_gen_2 should take two arguments, the number of trials to simulate and the adapted design object,
#' and return in the same format as data_gen_1.
#' 
#' @return A dataframe containing the various results
#' @importFrom future.apply future_lapply
#' @export
sim_trial <- function(design, runs1, runs2, adapt_rule, data_gen_1, data_gen_2, include_designs = FALSE) {
    k <- attr(design, "k")

    data_1 <- data_gen_1(runs1)
    designs_interim <- apply(data_1, 1, function(p_values) {
        cer_interim_test(design, p_values)
    })

    # maybe use smth else than do.call here? vec.rbind from vctrs, or map_dfr from purrr would work
    # or pre-allocating and filling the dataframe if performance is an issue, but then how to parallelize?
    results <- do.call(rbind, future_lapply(1:length(designs_interim), function(i) {
        design_adapted <- adapt_rule(designs_interim[[i]])
        design_adj <- cer_adapt_bounds(design_adapted)
        data_2 <- data_gen_2(runs2, design_adj)

        designs_final <- apply(data_2, 1, function(p_values) {
            cer_final_test(design_adj, p_values)
        })

        df <- do.call(rbind, lapply(designs_final, function(x) {
            df <- data.frame(
                run1 = i
            )

            for (j in 1:k) {
                df[, paste0("rej_", j)] <- x$rej[j]
            }
            df[, "rej_any"] <- any(x$rej)

            for (j in 1:k) {
                df[, paste0("p_", j)] <- x$p_values_final[j]
            }

            if (include_designs) {
                df[, design] <- x
            }
            df
        }))

        for (j in 1:k) {
            df[, paste0("interim_rej_", j)] <- design_adj$rej_interim[j]
        } 

        for (j in 1:k) {
            df[, paste0("interim_p_", j)] <- design_adj$p_values_interim[j]
        }

        df
    },
    future.globals = c("designs_interim", "runs2", "adapt_rule", "data_gen_2", "include_designs"),
    future.packages = "adagraph",
    future.seed = TRUE))

    return(results)
}

#'@importFrom stats pnorm qnorm pt
get_data_gen <- function(corr_control, corr_treatment, cont_treat_association, eff, n_cont, n_treat) {
  controls <- dim(corr_control)[1]
  arms <- dim(corr_treatment)[1]

  if (length(n_cont) == 1) {
    n_cont <- rep(n_cont, controls)
  }
  if (length(n_treat) == 1) {
    n_treat <- rep(n_treat, arms)
  }

  data_gen <- function(n, design=NA) {
    control <- array(
        mvtnorm::rmvnorm(max(n_cont)*n, sigma = corr_control),
        dim = c(n, max(n_cont), controls)
    )

    treatment <- array(
        mvtnorm::rmvnorm(max(n_treat)*n, mean = eff, sigma = corr_treatment),
        dim = c(n, max(n_treat), arms)
    )

    control <- aperm(control, c(2, 3, 1))
    treatment <- aperm(treatment, c(2, 3, 1))
    # now the shape is (number of treated people, number of arms, number of runs)

    #set all values outside of the desired sample size to 0
    for (i in 1:controls) {
        if (n_cont[i] < max(n_cont)) {
        control[n_cont[i]:max(n_cont), i, ] <- NA
        }
    }
    for (i in 1:arms) {
        if (n_treat[i] < max(n_treat)) {
        treatment[n_treat[i]:max(n_treat), i, ] <- NA
        }
    }

    cont_mean <- colMeans(control, na.rm=T)
    treat_mean <- colMeans(treatment, na.rm=T)
    cont_var <- matrixStats::colVars(control, na.rm=T, dim. = c(max(n_cont), length(n_cont) * n))
    dim(cont_var) <- dim(control)[-1]
    treat_var <- matrixStats::colVars(treatment, na.rm=T, dim. = c(max(n_treat), length(n_treat) * n))
    dim(treat_var) <- dim(treatment)[-1]

    p <- sapply(1:length(cont_treat_association), function(arm) {
        n_arm_cont <- n_cont[cont_treat_association[arm]]
        n_arm_treat <- n_treat[arm]
        

        t <- (treat_mean[arm, ] - cont_mean[cont_treat_association[arm], ]) / sqrt(
            ((treat_var[arm, ] * (n_arm_treat - 1)) + (cont_var[cont_treat_association[arm], ] * (n_arm_cont - 1))) /
            (n_arm_treat + n_arm_cont - 2)
            * (1 / n_arm_treat + 1 / n_arm_cont))

        df <- n_arm_treat + n_arm_cont - 2
        return(1 - pt(t, df = df))
    }) 
    return(p)
  }
  return(data_gen)
}

get_data_gen_2 <- function(
    corr_control,
    corr_treatment,
    cont_treat_association,
    eff,
    n_cont,
    n_treat,
    reassign_n = TRUE
) {
  controls <- dim(corr_control)[1]
  arms <- dim(corr_treatment)[1]

  if (length(n_cont) == 1) {
    n_cont <- rep(n_cont, controls)
  }
  if (length(n_treat) == 1) {
    n_treat <- rep(n_treat, arms)
  }

  data_gen_2 <- function(n, design) {
    p <- matrix(NA, nrow = n, ncol = arms)

    hyp <- design$keep_hyp
    if (any(hyp)) {
        if (reassign_n) {
        # boolean vector indicating which control groups are still needed
        cont <- 1:controls %in% cont_treat_association[hyp]

        #amount of people to be reassigned
        n_reassign <- sum(n_treat[!hyp]) + sum(n_cont[!cont])

        n_treat_ad <- n_treat[hyp] + n_reassign %/% (sum(hyp) + sum(cont))
        # to avoid having to change cont_treat_association, even controls that arent needed are kept
        n_cont[cont] <- n_cont[cont] + n_reassign %/% (sum(hyp) + sum(cont))
        rem <- n_reassign %% (sum(hyp) + sum(cont))
        if (!is.na(rem)) {
            rem_cont <- min(rem, length(cont))
            n_cont[1:rem_cont] <- n_cont[1:rem_cont] + 1
            if (rem > length(cont)) {
                n_treat_ad[1:(rem - length(cont))] <- n_treat_ad[1:(rem - length(cont))] + 1
            }
        }
        } else {
        n_cont_ad <- n_cont[hyp]
        }

        data_gen <- get_data_gen(
            corr_control = corr_control,
            corr_treatment = corr_treatment[hyp, hyp],
            cont_treat_association = cont_treat_association[hyp],
            eff = eff[hyp],
            n_cont = n_cont,
            n_treat = n_treat_ad
        )

        p2 <- data_gen(n)
        p[,hyp] <- t(apply(p2, 1, \(x) {1 -pnorm(sqrt(design$ad_t[hyp])*qnorm(1 - design$p_values_interim[hyp]) + sqrt(1 - design$ad_t[hyp])*qnorm(1-x))}))
    }
    return(p)
  }
}