library(matrixStats)
library(tidyverse)
library(mvtnorm)

stage_1_data_old <- function(mu, corr, n) {
  mueff = c(mu[2:5] - mu[1], mu[7:10] - mu[6])
  X1 = matrix(rnorm(n * 5), nrow = 5) #BF generates data
  X2 = corr * X1 + sqrt(1 - corr^2) * matrix(rnorm(n * 5), nrow = 5) + mu[6:10] #BF generates data
  X1 = X1 + mu[1:5]
  t1 = (rowMeans(X1[2:5, ]) - mean(X1[1, ])) /
    sqrt((rowVars(X1[2:5, ]) + var(X1[1, ])) / n)
  t2 = (rowMeans(X2[2:5, ]) - mean(X2[1, ])) /
    sqrt((rowVars(X2[2:5, ]) + var(X2[1, ])) / n)
  p1 = 1 - pt(c(t1, t2), df = n * 2 - 2) #BF actual p-values from data, t-test

  return(p1)
}

gen_data <- function(
  n,
  corr_control,
  corr_treatment,
  cont_treat_association,
  eff,
  n_cont,
  n_treat
) {
  #it is much more efficent to generate all data in only one call to rmvnorm instead of length seperate ones,
  #since else eigenvalues for the correlation matrix are computed each time

  controls <- dim(corr_control)[1]
  arms <- dim(corr_treatment)[1]

  if (length(n_cont) == 1) {
    n_cont <- rep(n_cont, controls)
  }
  if (length(n_treat) == 1) {
    n_treat <- rep(n_treat, arms)
  }

  control <- array(
    rmvnorm(max(n_cont) * n, sigma = corr_control),
    dim = c(n, max(n_cont), controls)
  )

  treatment <- array(
    rmvnorm(max(n_treat) * n, mean = eff, sigma = corr_treatment),
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

  cont_mean <- colMeans(control, na.rm = T)
  treat_mean <- colMeans(treatment, na.rm = T)
  cont_var <- colVars(
    control,
    na.rm = T,
    dim. = c(max(n_cont), length(n_cont) * n)
  )
  dim(cont_var) <- dim(control)[-1]
  treat_var <- colVars(
    treatment,
    na.rm = T,
    dim. = c(max(n_treat), length(n_treat) * n)
  )
  dim(treat_var) <- dim(treatment)[-1]

  p <- sapply(1:length(cont_treat_association), function(arm) {
    n_arm_cont <- n_cont[cont_treat_association[arm]]
    n_arm_treat <- n_treat[arm]

    t <- (treat_mean[arm, ] - cont_mean[cont_treat_association[arm], ]) /
      sqrt(
        ((treat_var[arm, ] * (n_arm_treat - 1)) +
          (cont_var[cont_treat_association[arm], ] * (n_arm_cont - 1))) /
          (n_arm_treat + n_arm_cont - 2) *
          (1 / n_arm_treat + 1 / n_arm_cont)
      )

    df <- n_arm_treat + n_arm_cont - 2
    return(1 - pt(t, df = df))
  })

  p
}

gen_data_naive <- function(
  n,
  corr_control,
  corr_treatment,
  cont_treat_association,
  eff,
  n_cont,
  n_treat
) {
  if (length(n_cont) == 1) {
    n_cont <- rep(n_cont, dim(corr_control)[1])
  }
  if (length(n_treat) == 1) {
    n_treat <- rep(n_treat, dim(corr_treatment)[1])
  }

  control <- array(
    t(rmvnorm(max(n_cont) * n, sigma = corr_control)),
    dim = c(length(n_cont), max(n_cont), n)
  )

  treatment <- array(
    t(rmvnorm(max(n_treat) * n, mean = eff, sigma = corr_treatment)),
    dim = c(length(n_treat), max(n_treat), n)
  )

  p1 <- sapply(1:n, function(i) {
    sapply(1:length(cont_treat_association), function(arm) {
      cont <- cont_treat_association[arm]
      t.test(
        treatment[arm, 1:n_treat[arm], i],
        control[cont, 1:n_cont[cont], i],
        alternative = "greater",
        var.equal = TRUE
      )$p.value
    })
  })
}


stage_2_data_old <- function(mu, corr, n2, trt2, runs2) {
  ntrt <- length(trt2)

  I2 = c(trt2, trt2 + 4)
  ##update the sample sizes
  tn2base = (n2 * 5) %/% (1 + ntrt) # compute 2nd stage sample size to the selected treatment
  tn2remainder = (n2 * 5) %% (1 + ntrt) # distribute sample size to remaining treatments
  tn2 = rep(0, 5)
  tn2[c(1, 1 + trt2)] = tn2base
  tn2[c(1, 1 + trt2)][0:tn2remainder] = tn2base + 1 # second stage per treatment sample size

  tn2s = tn2[c(1, 1 + trt2)] # sample sizes of the selected arms (out of 5)
  mtn2 = max(tn2s)

  Y1 = array(rnorm(mtn2 * (ntrt + 1) * runs2), dim = c(ntrt + 1, mtn2, runs2)) # primary endpoint data
  Y2 = corr *
    Y1 +
    sqrt(1 - corr^2) *
      array(rnorm(mtn2 * (ntrt + 1) * runs2), dim = c(ntrt + 1, mtn2, runs2)) +
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
  return(cbind(pp2.1, pp2.2))
  #p2[,I2] <- t(1-pnorm(sqrt(ths)*qnorm(1-p1[c(trt2,trt2+4)])+sqrt(1-ths)*qnorm(1-t(cbind(pp2.1,pp2.2)))))
}

eff <- c(0, 0, 0, 0, 0)
mu <- c(eff, eff)
corr <- 0.8
p_val_old <- as_tibble(do.call(
  rbind,
  (lapply(1:20000, function(i) {
    as.data.frame(list(
      i = i,
      val = stage_1_data_old(mu, corr, 50),
      index = paste0("H", 1:8)
    ))
  }))
))

trt2 <- c(1, 2, 3)

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
eff <- rep(0, 8)

gen_data_s2 <- function(trt2, n_patients, n) {
  n_cont <- n_patients
  n_arms <- length(trt2)
  n_treat_prim <- rep(n_patients, n_arms + 1)

  n_treat_prim <- n_treat_prim +
    floor(((4 - n_arms) * n_patients) / (n_arms + 1))
  rem <- ((4 - n_arms) * n_patients) %% (n_arms + 1)

  n_treat_prim[1:rem] <- n_treat_prim[1:rem] + 1

  sel_hyp <- c(trt2, trt2 + 4)

  gen_data(
    n = n,
    corr_control = corr_control,
    corr_treatment = corr_treatment[sel_hyp, sel_hyp],
    cont_treat_association = cont_treat_association[sel_hyp],
    eff = eff[sel_hyp],
    n_cont = n_treat_prim[1],
    n_treat = c(n_treat_prim[-1], n_treat_prim[-1])
  )
}

# p_val_old_2 <- as_tibble(do.call(rbind, (lapply(1:200, function(i) {
#   as.data.frame(list(i=i, val=stage_2_data_old(mu, corr, 50, trt2, 100), index=paste0("H",1:8)))
#   }
# ))))

p_long_old_2 <- as_tibble(
  do.call(rbind, lapply(1:200, \(x) stage_2_data_old(mu, corr, 50, trt2, 100))),
  .name_repair = "universal"
)
colnames(p_long_old_2) <- paste0("H", 1:6)

p_long_2 <- as_tibble(
  gen_data_s2(trt2, 50, 20000),
  .name_repair = "universal"
)
colnames(p_long_2) <- paste0("H", 1:6)


p_long <- as_tibble(
  gen_data(
    20000,
    corr_control,
    corr_treatment,
    cont_treat_association,
    eff,
    50,
    50
  ),
  .name_repair = "universal"
)

colnames(p_long) <- paste0("H", 1:8)


p_long_old <- p_val_old |>
  pivot_wider(names_from = index, values_from = val)


for (i in 1:8) {
  for (j in i:8) {
    co <- cor(p_long[paste0("H", i)], p_long[paste0("H", j)])
    co_old <- cor(p_long_old[paste0("H", i)], p_long_old[paste0("H", j)])
    cat(paste0("H", i, "xH", j, " new: ", round(co, 5), "\n"))
    cat(paste0("H", i, "xH", j, " old: ", round(co_old, 5), "\n"))
    cat(paste0("Diff: ", round(co - co_old, 5), "\n\n"))
  }
}

for (i in 1:6) {
  for (j in i:6) {
    co <- cor(p_long_2[paste0("H", i)], p_long_2[paste0("H", j)])
    co_old <- cor(p_long_old_2[paste0("H", i)], p_long_old_2[paste0("H", j)])
    cat(paste0("H", i, "xH", j, " new: ", round(co, 5), "\n"))
    cat(paste0("H", i, "xH", j, " old: ", round(co_old, 5), "\n"))
    cat(paste0("Diff: ", round(co - co_old, 5), "\n\n"))
  }
}

p_long |>
  pivot_longer(cols = H1:H8, names_to = "p_name", values_to = "p") |>
  ggplot(aes(y = p, x = p_name)) +
  geom_boxplot()
