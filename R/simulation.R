#' Run a series of simulated trials according to a specified design
#'
#' @param design cer_design object to be used for the simulation
#' @param runs1 Number of trials to run in the first stage
#' @param runs2 Number of second stage to run for every first stage trial
#' @param adapt_rule function that takes a cer_design object after the
#'   first interim test and returns the same object with appropiate adaptions
#' @param data_gen_1 function for generating first stage data, see details
#' @param data_gen_2 function for generating second stage data, see details
#' @param include_designs boolean indicating whether to include
#'   the designs objects in the output
#'
#' @details
#' data_gen_1 and data_gen_2 should take two arguments,
#'   the number of trials to simulate and the adapted design object,
#'  and return a matrix or dataframe with each row being the p-values
#'  for a single trial.
#'
#' @return A dataframe containing the various results
#' @importFrom future.apply future_lapply
#' @export
sim_trial <- function(
  design,
  runs1,
  runs2,
  adapt_rule,
  data_gen_1,
  data_gen_2,
  include_designs = FALSE
) {
  k <- attr(design, "k")

  data_1 <- data_gen_1(runs1, design)
  designs_interim <- apply(data_1, 1, function(p_values) {
    cer_interim_test(design, p_values)
  })

  # maybe use smth else than do.call here? vec.rbind from vctrs,
  # or map_dfr from purrr would work or pre-allocating and filling
  # the dataframe if performance is an issue, but then how to parallelize?
  results <- do.call(
    rbind,
    future_lapply(
      seq_along(designs_interim),
      function(i) {
        design_adapted <- adapt_rule(designs_interim[[i]])
        design_adj <- cer_adapt_bounds(design_adapted)
        data_2 <- data_gen_2(runs2, design_adj)

        designs_final <- apply(data_2, 1, function(p_values) {
          cer_final_test(design_adj, p_values)
        })

        df <- do.call(
          rbind,
          lapply(designs_final, function(x) {
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
          })
        )

        for (j in 1:k) {
          df[, paste0("interim_rej_", j)] <- design_adj$rej_interim[j]
        }

        for (j in 1:k) {
          df[, paste0("interim_p_", j)] <- design_adj$p_values_interim[j]
        }

        df
      },
      # future.globals = c(
      #   "designs_interim",
      #   "runs2",
      #   "adapt_rule",
      #   "data_gen_2",
      #   "include_designs"
      # ),
      future.packages = "adagraph",
      future.seed = TRUE
    )
  )

  return(results)
}

#'@importFrom stats pnorm qnorm pt
get_data_gen <- function(
  corr_control,
  corr_treatment,
  eff,
  n_cont,
  n_treat
) {
  controls <- dim(corr_control)[1]
  arms <- dim(corr_treatment)[1]

  if (length(n_cont) == 1) {
    n_cont <- rep(n_cont, controls)
  }
  if (length(n_treat) == 1) {
    n_treat <- rep(n_treat, arms)
  }

  which_arms <- n_treat > 0

  arms_used <- sum(which_arms)
  n_treat <- n_treat[which_arms]

  data_gen <- function(n, design) {
    p <- matrix(NA, nrow = n, ncol = arms)

    if (arms_used > 0) {
      treatment_assoc <- design$treatment_assoc
      control <- array(
        mvtnorm::rmvnorm(max(n_cont) * n, sigma = corr_control),
        dim = c(n, max(n_cont), controls)
      )

      treatment <- array(
        mvtnorm::rmvnorm(
          max(n_treat) * n,
          mean = eff[which_arms],
          sigma = corr_treatment[which_arms, which_arms]
        ),
        dim = c(n, max(n_treat), arms_used)
      )

      control <- aperm(control, c(2, 3, 1))
      treatment <- aperm(treatment, c(2, 3, 1))
      # now the shape is (number of treated people, number of arms, number of runs)

      #set all values outside of the desired sample size to NA
      for (i in 1:controls) {
        if (n_cont[i] < max(n_cont)) {
          control[n_cont[i]:max(n_cont), i, ] <- NA
        }
      }
      for (i in 1:arms_used) {
        if (n_treat[i] < max(n_treat)) {
          treatment[n_treat[i]:max(n_treat), i, ] <- NA
        }
      }

      cont_mean <- colMeans(control, na.rm = TRUE)
      treat_mean <- colMeans(treatment, na.rm = TRUE)
      cont_var <- matrixStats::colVars(
        control,
        na.rm = TRUE,
        dim. = c(max(n_cont), length(n_cont) * n)
      )
      dim(cont_var) <- dim(control)[-1]
      treat_var <- matrixStats::colVars(
        treatment,
        na.rm = TRUE,
        dim. = c(max(n_treat), length(n_treat) * n)
      )
      dim(treat_var) <- dim(treatment)[-1]

      p[, which_arms] <- sapply(
        seq_along(treatment_assoc[which_arms]),
        function(arm) {
          n_arm_cont <- n_cont[treatment_assoc[arm]]
          n_arm_treat <- n_treat[arm]

          t <- (treat_mean[arm, ] - cont_mean[treatment_assoc[arm], ]) /
            sqrt(
              ((treat_var[arm, ] * (n_arm_treat - 1)) +
                (cont_var[treatment_assoc[arm], ] * (n_arm_cont - 1))) /
                (n_arm_treat + n_arm_cont - 2) *
                (1 / n_arm_treat + 1 / n_arm_cont)
            )

          df <- n_arm_treat + n_arm_cont - 2

          1 - pt(t, df = df)
        }
      )
    }

    p
  }

  data_gen
}

get_data_gen_2 <- function(
  corr_control,
  corr_treatment,
  eff
) {
  data_gen_2 <- function(n, design) {
    hyp <- design$keep_hyp
    if (any(hyp)) {
      data_gen <- get_data_gen(
        corr_control = corr_control,
        corr_treatment = corr_treatment,
        eff = eff,
        n_cont = design$n_cont_2,
        n_treat = design$n_treat_2
      )

      p2 <- data_gen(n, design)
      p <- t(apply(p2, 1, \(x) {
        1 -
          pnorm(
            sqrt(design$ad_t) *
              qnorm(1 - design$p_values_interim) +
              sqrt(1 - design$ad_t) * qnorm(1 - x)
          )
      }))
    }
    p
  }

  data_gen_2
}
