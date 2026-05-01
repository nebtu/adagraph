# Adapt the patient numbers for the second stage of a trial design

Without any changes, `adagraph` expects the correlation between the
p-values to be the same as in the first stage. If there are changes in
the relative sample sizes, either because of changes in the design or
random variations, this might not be accurate. Additionally, one might
want to use a different t for calculating the combined p-value, using
the information fraction of the actual realised patient numbers.

## Usage

``` r
trial_adapt_n(
  design,
  n_control_2 = NULL,
  n_arms_2 = NULL,
  ad_n_table = NULL,
  calculate_t = TRUE,
  adapt_bounds = TRUE
)
```

## Arguments

- design:

  The `trial_design` to be adapted

- n_control_2:

  The patient numbers in the control. Can only be used if there are no
  subgroups

- n_arms_2:

  The patient numbers in the arms (as a numberic vector). Can only be
  used if there are no subgroups

- ad_n_table:

  A data.frame specifying the structure of the subgroups, see details
  for more information

- calculate_t:

  Calculate a new information fraction t, and adapt the design to use
  it.

- adapt_bounds:

  Adapt the bounds for rejecting a hypotheses to keep the FWER with the
  new adaptations. If doing multiple adaptations, it is enough to adapt
  bounds only for the last one, or call `adapt_bounds()` manually after.

## Value

An object of class `trial_design`, with the adaptations applied.

## Examples

``` r
m <- rbind(
  c(0, 1 / 2, 1 / 2, 0),
  c(1 / 2, 0, 0, 1 / 2),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
as <- function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))

des <- trial_design(
  arms = 2,
  endpoints = 2,
  n_control = 35,
  n_arms = 35,
  weights = c(1 / 2, 1 / 2, 0, 0),
  t = 0.5,
  alpha = 0.025,
  test_m = m,
  alpha_spending_f = as
) |>
  cer_interim_test(c(
   0.00045,
   0.0952,
   0.0225,
   0.1104
 ))

des_ad <- des |>
  trial_adapt_n(n_control_2 = 50, n_arms = c(20, 30))

des_ad
#> A Trial Design object, for testing the 4 hypotheses E1_A1, E1_A2, E2_A1, and E2_A2 at FWER 0.025.
#> 
#> There are 2 arms (A1 and A2), 2 endpoints (E1 and E2) and no subgroups.
#> The first stage sample size per arm/group is:
#>      arm  n
#>  control 35
#>       A1 35
#>       A2 35
#> 
#> ── An interim test has been performed. ─────────────────────────────────────────
#> Hypotheses rejected at the interim: E1_A1
#> ── The following characteristics have been adapted: ────────────────────────────
#> • Correlation for parametric test
#> • Time fractions for the hypotheses
#> The second stage sample size per arm/group is:
#>      arm  n
#>  control 50
#>       A1 20
#>       A2 30
#> ── No final test has been performed yet ────────────────────────────────────────
des_ad[["ad_correlation"]] #changed correlation structure
#>           E1_A1     E1_A2     E2_A1     E2_A2
#> E1_A1 1.0000000 0.3273268        NA        NA
#> E1_A2 0.3273268 1.0000000        NA        NA
#> E2_A1        NA        NA 1.0000000 0.3273268
#> E2_A2        NA        NA 0.3273268 1.0000000
des_ad[["ad_t"]] #changed t (different for each hypothesis)
#> [1] 0.5505618 0.4827586 0.5505618 0.4827586

n_table <- rbind(
  data.frame(arm = "control", `G1` = FALSE, n = 20),
  data.frame(arm = "control", `G1` = TRUE, n = 15),
  data.frame(arm = "A1", `G1` = FALSE, n = 20),
  data.frame(arm = "A1", `G1` = TRUE, n = 15)
)
des <- trial_design(
  endpoints = 2,
  subgroups = 1,
  n_table = n_table,
  weights = c(1 / 2, 1 / 2, 0, 0),
  t = 0.5,
 alpha = 0.025,
 test_m = m,
 alpha_spending_f = as
) |>
  cer_interim_test(c(
    0.00045,
    0.0952,
    0.0225,
    0.1104
  ))

ad_n_table <- rbind(
  data.frame(arm = "control", `G1` = FALSE, n = 30),
  data.frame(arm = "control", `G1` = TRUE, n = 20),
  data.frame(arm = "A1", `G1` = FALSE, n = 10),
  data.frame(arm = "A1", `G1` = TRUE, n = 15)
)
des_ad <- des |>
  trial_adapt_n(ad_n_table = ad_n_table)

des_ad
#> A Trial Design object, for testing the 4 hypotheses E1, E2, G1E1, and G1E2 at FWER 0.025.
#> 
#> There are 1 arm, 2 endpoints (E1 and E2) and 1 subgroup (G1).
#> The first stage sample size per arm/group is:
#>      arm    G1  n
#>  control FALSE 20
#>  control  TRUE 15
#>       A1 FALSE 20
#>       A1  TRUE 15
#> 
#> ── An interim test has been performed. ─────────────────────────────────────────
#> Hypotheses rejected at the interim: E1
#> ── The following characteristics have been adapted: ────────────────────────────
#> • Correlation for parametric test
#> • Time fractions for the hypotheses
#> The second stage sample size per arm/group is:
#>      arm    G1  n
#>  control FALSE 30
#>  control  TRUE 20
#>       A1 FALSE 10
#>       A1  TRUE 15
#> ── No final test has been performed yet ────────────────────────────────────────

des_ad[["ad_correlation"]] #changed correlation structure
#>             E1        E2      G1E1      G1E2
#> E1   1.0000000        NA 0.7171372        NA
#> E2          NA 1.0000000        NA 0.7171372
#> G1E1 0.7171372        NA 1.0000000        NA
#> G1E2        NA 0.7171372        NA 1.0000000
des_ad[["ad_t"]] #changed t (different for each hypothesis)
#> [1] 0.5121951 0.5121951 0.4666667 0.4666667
```
