#' Run a series of simulated trials according to a specified design
#'
#' @param design cer_design or multiarm_design object to be used for the simulation
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
#' See also `get_data_gen()` and `get_data_gen_2()` for further information on
#' how the input and output of data generating functions should look.
#'
#' @return A dataframe containing the various results
#'
#' @examples
#' as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))
#' design <- multiarm_cer_design(
#'  controls = 1,
#'  treatment_assoc = c(1,1),
#'  n_controls = 50,
#'  n_treatments = 50,
#'  weights = c(0.5, 0.5),
#'  alpha = 0.05,
#'  test_m = rbind(c(0, 1),
#'               c(1, 0)),
#'  alpha_spending_f = as,
#'  t = 0.5)
#'
#' adaption <- function(design) {
#'   design |> multiarm_drop_arms(1)
#' }
#'
#' data_gen <- get_data_gen(
#'   matrix(1),
#'   rbind(
#'    c(1, 0.5),
#'    c(0.5, 1)
#'  ),
#'   c(0, 0.3),
#'   100,
#'   100
#' )
#'
#' data_gen_2 <- get_data_gen_2(
#'   matrix(1),
#'   rbind(
#'    c(1, 0.5),
#'    c(0.5, 1)
#'   ),
#'   c(0, 0)
#' )
#'
#' sim_trial(
#'   design,
#'   5,
#'   3,
#'   adaption,
#'   data_gen,
#'   data_gen_2
#' )
#'
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
  sim_second_stage <- function(i) {
    design_adapted <- adapt_rule(designs_interim[[i]])
    data_2 <- data_gen_2(runs2, design_adapted)

    designs_final <- apply(data_2, 1, function(p_values) {
      cer_final_test(design_adapted, p_values)
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
          df$design <- list(x)
        }
        df
      })
    )

    for (j in 1:k) {
      df[, paste0("interim_rej_", j)] <- design_adapted$rej_interim[j]
    }

    for (j in 1:k) {
      df[, paste0("interim_p_", j)] <- design_adapted$p_values_interim[j]
    }

    df
  }
  # maybe use smth else than do.call here? vec.rbind from vctrs,
  # or map_dfr from purrr would work or pre-allocating and filling
  # the dataframe if performance is an issue, but then how to parallelize?
  results <- do.call(
    rbind,
    future.apply::future_lapply(
      seq_along(designs_interim),
      sim_second_stage,
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
