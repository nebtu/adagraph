#Note that using this function works exactly as the simrun and wrap functions
# in the original script when using the same seed

get_paper_sim_design <- function() {
  ws = .75 # (weight given to secondary endpoint)
  wp = (1 - ws) / 3 # weight given to the other arms primary endpoint
  m <- rbind(
    H1 = c(0, wp, wp, wp, ws, 0, 0, 0),
    H2 = c(wp, 0, wp, wp, 0, ws, 0, 0),
    H3 = c(wp, wp, 0, wp, 0, 0, ws, 0),
    H4 = c(wp, wp, wp, 0, 0, 0, 0, ws),
    H5 = c(0, 1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0),
    H6 = c(1 / 3, 0, 1 / 3, 1 / 3, 0, 0, 0, 0),
    H7 = c(1 / 3, 1 / 3, 0, 1 / 3, 0, 0, 0, 0),
    H8 = c(1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0, 0)
  )

  weights <- c(rep(1 / 4, 4), rep(0, 4))
  correlation <- matrix(rep(1 / 2, 64), nrow = 8) + 1 / 2 * diag(8)
  correlation[1:4, 5:8] <- NA
  correlation[5:8, 1:4] <- NA
  diag(correlation) <- 1
  t = 0.5 # information time
  alpha <- 0.025 #overall alpha
  as <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t)) #spending function

  design <- cer_design(
    correlation = correlation,
    weights = weights,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as,
    t = t
  )

  design
}

get_paper_sim_design_multiarm <- function() {
  n <- 100
  t <- 0.5
  ws <- .75 # (weight given to secondary endpoint)
  wp <- (1 - ws) / 3 # weight given to the other arms primary endpoint
  m <- rbind(
    H1 = c(0, wp, wp, wp, ws, 0, 0, 0),
    H2 = c(wp, 0, wp, wp, 0, ws, 0, 0),
    H3 = c(wp, wp, 0, wp, 0, 0, ws, 0),
    H4 = c(wp, wp, wp, 0, 0, 0, 0, ws),
    H5 = c(0, 1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0),
    H6 = c(1 / 3, 0, 1 / 3, 1 / 3, 0, 0, 0, 0),
    H7 = c(1 / 3, 1 / 3, 0, 1 / 3, 0, 0, 0, 0),
    H8 = c(1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0, 0)
  )

  weights <- c(rep(1 / 4, 4), rep(0, 4))

  alpha <- 0.025 #overall alpha

  #spending function
  as <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))

  design <- multiarm_cer_design(
    controls = 2,
    treatment_assoc = c(1, 1, 1, 1, 2, 2, 2, 2),
    n_controls = n,
    n_treatments = n,
    weights = weights,
    t = t,
    alpha = alpha,
    test_m = m,
    alpha_spending_f = as
  )

  design
}

get_sim_adaption <- function(futility, alt_drop = FALSE) {
  function(design) {
    p <- design$p_values_interim[1:4]

    trt_rej <- design$rej_interim[1:4] & design$rej_interim[5:8]
    if (futility > 0) {
      drop_hyp <- (p > futility) | trt_rej
    } else {
      # drop all but the best performing one (and that as well if it is rejected)
      drop_hyp <- (p != min(p)) | trt_rej
    }

    drop_arms <- which(drop_hyp)

    new_n <- redistribute_n(
      n = c(design$n_treatments[1:4], design$n_controls[1]),
      t = design$t,
      which_drop = drop_arms,
    )

    design |>
      multiarm_drop_arms(
        c(drop_arms, drop_arms + 4),
        n_cont_2 = rep(new_n[5], 2),
        n_treat_2 = rep(new_n[1:4], 2),
        alt_adj = alt_drop
      )
  }
}

simrun <- function(
  design,
  mu = rep(0, 10),
  corr,
  futility = 0.5,
  n1 = 50,
  n2 = 50,
  runs2 = 1000,
  new_adaption = FALSE
) {
  mueff = c(mu[2:5] - mu[1], mu[7:10] - mu[6])
  X1 = matrix(rnorm(n1 * 5), nrow = 5) #BF generates data
  X2 = corr * X1 + sqrt(1 - corr^2) * matrix(rnorm(n1 * 5), nrow = 5) + mu[6:10] #BF generates data
  X1 = X1 + mu[1:5]
  t1 = (rowMeans(X1[2:5, ]) - mean(X1[1, ])) /
    sqrt((matrixStats::rowVars(X1[2:5, ]) + var(X1[1, ])) / n1)
  t2 = (rowMeans(X2[2:5, ]) - mean(X2[1, ])) /
    sqrt((matrixStats::rowVars(X2[2:5, ]) + var(X2[1, ])) / n1)
  p1 = 1 - pt(c(t1, t2), df = n1 * 2 - 2) #BF actual p-values from data, t-test
  t = n1 / (n1 + n2)

  design_interim <- cer_interim_test(design, p1)

  trt_rej <- design_interim$rej_interim[1:4] & design_interim$rej_interim[5:8]

  if (futility > 0) {
    drop_hyp <- (design_interim$p_values_interim[1:4] > futility) | trt_rej
  } else {
    drop_hyp <- (design_interim$p_values_interim[1:4] !=
      min(design_interim$p_values_interim[1:4][!trt_rej]))
  }

  if (any(!drop_hyp)) {
    trt2 <- which(!drop_hyp)
    ntrt <- length(trt2)

    I2 = c(trt2, trt2 + 4)

    tn2base = (n2 * 5) %/% (1 + ntrt) # compute 2nd stage sample size to the selected treatment
    tn2remainder = (n2 * 5) %% (1 + ntrt) # distribute sample size to remaining treatments
    tn2 = rep(0, 5)
    tn2[c(1, 1 + trt2)] = tn2base
    tn2[c(1, 1 + trt2)][0:tn2remainder] = tn2base + 1 # second stage per treatment sample size
    tt = n1 * .5 / (n1 * .5 + tn2[1] * tn2[-1] / (tn2[1] + tn2[-1])) #adapted information fractions for the selected treatments
    th = c(tt, tt) # information fractions for all hypotheses (dropped get IF 1)
    si <- sqrt(1 / tn2[-1] + 1 / tn2[1]) # update the correlation matrix
    ctemp = 1 / tn2[1] / outer(si, si)
    corradapt = design$correlation
    corradapt[1:4, 1:4] = ctemp
    corradapt[5:8, 5:8] = ctemp
    diag(corradapt) = 1 # corradapt has only for selected hypothesis a valid entry

    if (new_adaption) {
      adaption <- get_sim_adaption(futility = futility, alt_drop = TRUE)
      design_adapted <- design_interim |> adaption()
    } else {
      design_adapted <- design_interim |>
        cer_alt_drop_hypotheses(drop_hyp, adapt_bounds = FALSE) |>
        cer_adapt(
          time = th,
          correlation = corradapt
        )
    }

    p2 = matrix(NA, nrow = runs2, ncol = 8)

    # simulate the second stage data
    tn2s = tn2[c(1, 1 + trt2)] # sample sizes of the selected arms (out of 5)
    ths = th[c(trt2, 4 + trt2)] # information fractions of the selected hypotheses out of 8
    mtn2 = max(tn2s)

    Y1 = array(rnorm(mtn2 * (ntrt + 1) * runs2), dim = c(ntrt + 1, mtn2, runs2)) # primary endpoint data
    Y2 = corr *
      Y1 +
      sqrt(1 - corr^2) *
        array(
          rnorm(mtn2 * (ntrt + 1) * runs2),
          dim = c(ntrt + 1, mtn2, runs2)
        ) +
      mu[c(6, trt2 + 6)] # secondary endpoint data
    Y1 = Y1 + mu[c(1, trt2 + 1)]
    # to have the correct sample sizes we set mtn2-tn2 observations to 0
    ztn2 = mtn2 - tn2s # number of elements to be set to 0 for each treatment to get correct sample size
    for (i in seq_len(ntrt + 1)) {
      Y1[i, seq_len(ztn2[i]), ] <- 0
      Y2[i, seq_len(ztn2[i]), ] <- 0
    }
    Y1p = aperm(Y1, c(1, 3, 2))
    # because rowMeans sums over the last dimension, we need to permute
    # the dimensions of the arrays first
    Y2p = aperm(Y2, c(1, 3, 2))
    my1 = rowMeans(Y1p, dims = 2)
    my2 = rowMeans(Y2p, dims = 2)
    my1s = rowMeans(Y1p * Y1p, dims = 2)
    my2s = rowMeans(Y2p * Y2p, dims = 2)
    vy1 = (my1s - my1 * my1) * tn2s / (tn2s - 1)
    vy2 = (my2s - my2 * my2) * tn2s / (tn2s - 1)
    t1 = (t(my1[-1, , drop = FALSE]) - my1[1, ]) /
      sqrt(
        ((t(vy1[-1, , drop = FALSE] * (tn2s[-1] - 1)) +
          vy1[1, ] * (tn2s[1] - 1)) /
          (tn2s[1] + tn2s[-1] - 2)) *
          (1 / tn2s[1] + 1 / tn2s[-1])
      )
    t2 = (t(my2[-1, , drop = FALSE]) - my2[1, ]) /
      sqrt(
        ((t(vy2[-1, , drop = FALSE] * (tn2s[-1] - 1)) +
          vy2[1, ] * (tn2s[1] - 1)) /
          (tn2s[1] + tn2s[-1] - 2)) *
          (1 / tn2s[1] + 1 / tn2s[-1])
      )
    pp2.1 <- matrix(NA, nrow = runs2, ncol = ntrt, ) # 2nd stage p-values primary endpoint
    pp2.2 <- matrix(NA, nrow = runs2, ncol = ntrt) # 2nd stage p-values secondary endpoint
    # Loop over each treatment group
    for (i in 1:ntrt) {
      # Degrees of freedom for the i-th treatment group
      df_i <- tn2[1] + tn2s[i + 1] - 2
      # Compute p-values for all trials in the i-th treatment group
      pp2.1[, i] <- pt(-t1[, i], df = df_i)
      pp2.2[, i] <- pt(-t2[, i], df = df_i)
    }
    p2[, I2] <- t(
      1 -
        pnorm(
          sqrt(ths) *
            qnorm(1 - p1[c(trt2, trt2 + 4)]) +
            sqrt(1 - ths) * qnorm(1 - t(cbind(pp2.1, pp2.2)))
        )
    )

    designs_final <- apply(p2, 1, function(p_values) {
      cer_final_test(
        design = design_adapted,
        p_values = p_values
      )
    })

    rej2 <- rowMeans(sapply(designs_final, function(x) as.integer(x$rej)))
    fwer <- mean(matrixStats::colMaxs(sapply(
      designs_final,
      function(x) as.integer(x$rej)
    )))

    res <- list(
      rej1 = design_interim$rej_interim,
      rej2 = rej2,
      fwer = fwer
    )
  } else {
    res <- list(
      rej1 = design_interim$rej_interim,
      rej2 = design_interim$rej_interim,
      fwer = as.integer(max(design_interim$rej_interim))
    )
  }
  res
}


sim_wrap <- function(
  design,
  eff = c(0, 0.25, 0.25, 0.25, 0.25),
  futility = .75,
  runs1 = 100,
  runs2 = 100,
  corr = 0.5,
  new_adaption = FALSE
) {
  mu = c(eff, eff)
  results = lapply(
    1:runs1,
    function(i) {
      simrun(
        design,
        mu = mu,
        corr = corr,
        runs2 = runs2,
        n1 = 50,
        n2 = 50,
        futility = futility,
        new_adaption = new_adaption
      )
    }
  )

  res_df <- do.call(
    rbind,
    lapply(results, function(x) {
      df <- data.frame(
        fwer = x$fwer
      )
      for (i in 1:8) {
        df[, paste0("rej2_", i)] <- x$rej2[i]
        df[, paste0("rej1_", i)] <- x$rej1[i]
      }
      df
    })
  )

  res_df
}
