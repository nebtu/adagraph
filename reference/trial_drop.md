# Dropp groups, arms or endpoints from a trial design

Functions for dropping all hypotheses associated with some subgroups,
arms or endpoints. The weights of the dropped hypotheses are set to 0
and distributed according to the prespecified graph. The graph is also
adapted such that weights are never assigned to the dropped hypotheses.

## Usage

``` r
trial_drop_groups(design, groups, adapt_bounds = TRUE)

trial_drop_arms(design, arms, adapt_bounds = TRUE)

trial_drop_endpoints(design, endpoints, adapt_bounds = TRUE)
```

## Arguments

- design:

  The `trial_design` of which to drop the

- groups:

  The subgroups to be dropped, by their name or integer indices

- adapt_bounds:

  Adapt the bounds for rejecting a hypotheses to keep the FWER with the
  new adaptations. If doing multiple adaptations, it is enough to adapt
  bounds only for the last one, or call `adapt_bounds()` manually after.

- arms:

  The arms to be dropped, by their name or integer indices

- endpoints:

  The endpoints to be dropped, by their name or integer indices

## Value

design with the specified hypotheses (according to groups, arms or
endpoints) dropped

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
  arms = 4,
  n_control = 35,
  n_arms = 35,
  weights = c(1/2, 1/2, 0, 0),
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
  trial_drop_arms(c("A1", "A4"))

des_ad
#> A trial Design object, for testing the 4 hypotheses A1, A2, A3, and A4 at FWER 0.025.
#> 
#> There are 4 arms (A1, A2, A3, and A4), 1 endpoint and no subgroups.
#> The first stage sample size per arm/group is:
#>      arm  n
#>  control 35
#>       A1 35
#>       A2 35
#>       A3 35
#>       A4 35
#> 
#> ── An interim test has been performed. ─────────────────────────────────────────
#> Hypotheses rejected at the interim: A1
#> ── The following characteristics have been adapted: ────────────────────────────
#> • Hypotheses weights
#> • Graph Transition Matrix
#> ── No final test has been performed yet ────────────────────────────────────────
des_ad$ad_test_m
#>    A1 A2 A3 A4
#> A1  0  0  0  0
#> A2  0  0  1  0
#> A3  0  1  0  0
#> A4  0  0  0  0

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
  weights = c(1/2, 1/2, 0, 0),
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
  trial_drop_groups("G1") |>
  trial_drop_endpoints("E1")

des_ad
#> A trial Design object, for testing the 4 hypotheses E1, E2, G1E1, and G1E2 at FWER 0.025.
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
#> • Hypotheses weights
#> • Graph Transition Matrix
#> ── No final test has been performed yet ────────────────────────────────────────
des_ad$ad_test_m
#>      E1 E2 G1E1 G1E2
#> E1    0  0    0    0
#> E2    0  0    0    0
#> G1E1  0  0    0    0
#> G1E2  0  0    0    0
```
