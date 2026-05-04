# note that second stage depends on ad_n_table being set
# TODO: remove n=0 rows in data_spec, make sure that NA is returned for not
# calculated p-values
get_trial_data_gen <- function(
  corr_endpoints,
  effect_sizes,
  binary_endpoints = character(0),
  second_stage = FALSE
) {
  function(n, design) {
    # get the relevant sample sizes
    if (second_stage) {
      n_table <- design[["ad_n_table"]]
    } else {
      n_table <- design[["n_table"]]
    }

    #use effect sizes for the normal distribution, and apply binary cutoff later
    effect_sizes[paste0(binary_endpoints, "_bin")] <- effect_sizes[
      binary_endpoints
    ]
    effect_sizes[binary_endpoints] <- 0

    # match with the effect sizes
    data_spec <- merge(
      n_table,
      effect_sizes,
      by = c("arm", design[["names_subgroups"]])
    )

    # generate data for each group combination
    data_list <- lapply(seq_len(nrow(data_spec)), function(i) {
      if (data_spec[i, "n"] == 0) {
        return(NULL)
      }

      means <- unlist(data_spec[i, design[["names_endpoints"]]])
      data <- array(
        mvtnorm::rmvnorm(
          data_spec[i, "n"] * n,
          mean = means,
          sigma = corr_endpoints
        ),
        dim = c(n, data_spec[i, "n"], design[["endpoints"]])
      )
      data <- aperm(data, c(2, 3, 1))
      # now the shape is (number of treated people, number of endpoints, number of runs)

      for (endpoint in binary_endpoints) {
        binary_index <- which(design[["names_endpoints"]] == endpoint)
        data[, binary_index, ] <- as.numeric(
          #would be coerced to numeric anyway, so make it explicit
          data[, binary_index, ] >
            qnorm(1 - data_spec[i, paste0(endpoint, "_bin")])
        )
      }

      data
    })

    hyp_assoc <- design[["hyp_assoc"]]
    # test, over each hypothesis separately
    p_values <- vapply(
      seq_len(nrow(hyp_assoc)),
      function(i) {
        arm <- hyp_assoc[i, "arm"]
        group <- hyp_assoc[i, "group"]
        endpoint <- hyp_assoc[i, "endpoint"]
        endpoint_index <- which(design[["names_endpoints"]] == endpoint)
        if (group == "Total") {
          group_filter <- TRUE
        } else {
          group_filter <- data_spec[[group]]
        }

        treat_filter <- data_spec[["arm"]] == arm & group_filter
        n_treat <- sum(data_spec[treat_filter, "n"])
        data_treat_list <- data_list[treat_filter]
        data_treat <- do.call(
          rbind,
          lapply(data_treat_list, function(d) {
            d[, endpoint_index, ]
          })
        )

        control_filter <- data_spec[["arm"]] == "control" & group_filter
        n_control <- sum(data_spec[control_filter, "n"])
        data_cont_list <- data_list[
          data_spec[["arm"]] == "control" & group_filter
        ]
        data_cont <- do.call(
          rbind,
          lapply(data_cont_list, function(d) {
            d[, endpoint_index, ]
          })
        )
        if (n_treat == 0 | n_control == 0) {
          return(rep(NA, n))
        }

        if (endpoint %in% binary_endpoints) {
          cont_prop <- colMeans(data_cont)
          treat_prop <- colMeans(data_treat)
          overall_prop <- colMeans(rbind(data_treat, data_cont))

          z <- (treat_prop - cont_prop) /
            sqrt(
              overall_prop *
                (1 - overall_prop) *
                ((1 / n_control) + (1 / n_treat))
            )
          p <- ifelse(is.na(z), 1, pnorm(z, lower.tail = FALSE))
        } else {
          cont_mean <- colMeans(data_cont)
          cont_var <- matrixStats::colVars(data_cont)
          treat_mean <- colMeans(data_treat)
          treat_var <- matrixStats::colVars(data_treat)

          t <- (treat_mean - cont_mean) /
            sqrt(
              (treat_var * (n_treat - 1) + cont_var * (n_control - 1)) /
                ((n_treat + n_control - 2)) *
                (1 / n_treat + 1 / n_control)
            )

          p <- pt(t, df = n_treat + n_control - 2, lower.tail = FALSE)
        }

        p
      },
      numeric(n)
    )

    p_values
  }
}
