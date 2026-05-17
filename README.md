

<!-- README.md is generated from README.qmd. Please edit that file -->

# adagraph

<!-- badges: start -->

<!-- badges: end -->

Design and analysie two-stage adaptive trials using graph-based
closed-testing procedures, while controlling the FWER using the
conditional error rate method.

## Features

- Graph-based multiple testing strategies
- Designs with multiple arms, endpoints and subpopulations
- Semi-parametric testing strategies, taking correlations from shared
  patients into account where sensible and falling back to
  Bonferronni-like correction else
- Interim analysis with early stopping
- Adaptations based on the partial conditional error rate, allowing:
  - Dropping of arms, endpoints and subpopulations
  - Sample size reassesment
  - Changing of the test graph used
- Simulations of many trials

## Installation

You can install the development version of adagraph from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nebtu/adagraph")
```

## Example

We define a simple two-arm trial using the `trial_design()` function.

``` r
library(adagraph)

alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))

design <- trial_design(
  arms = 2,
  n_control = 50,
  n_arms = c(arm1 = 50, arm2 = 50),
  weights = c(0.5, 0.5),
  test_m = rbind(c(0, 1), c(1, 0)),
  alpha = 0.025,
  alpha_spending_f = alpha_spending_f,
  t = 1 / 2,
  names_arms = c("arm1", "arm2")
)

design
#> 
#> ── Trial Design ────────────────────────────────────────────────────────────────
#> Testing the 2 hypotheses arm1 and arm2 at FWER 0.025.
#> There are 2 arms (arm1 and arm2), 1 endpoint and no subgroups.
#> The first stage sample size per arm/group is:
#>      arm  n
#>  control 50
#>     arm1 50
#>     arm2 50
#> 
#> ── No interim test performed ──
#> 
#> ── No adaptations performed ──
#> 
#> ── No final test performed ──
```

The interim analysis can be performed with `cer_interim_test`. The
bounds for rejection are precalculated and take into account the
correlation thanks to the shared control group.

``` r
design <- cer_interim_test(design, c(0.01, 0.02))

design
#> 
#> ── Trial Design ────────────────────────────────────────────────────────────────
#> Testing the 2 hypotheses arm1 and arm2 at FWER 0.025.
#> There are 2 arms (arm1 and arm2), 1 endpoint and no subgroups.
#> The first stage sample size per arm/group is:
#>      arm  n
#>  control 50
#>     arm1 50
#>     arm2 50
#> 
#> ── Interim test ──
#> 
#> No hypotheses were rejected at the interim.
#> 
#> ── No adaptations performed ──
#> 
#> ── No final test performed ──
```

We can adapt by dropping the secondary arm and reallocating the smaple
size. The bounds are adapted automatically and we can perform the final
test immediately.

``` r
design |>
  trial_drop_arms("arm2") |>
  trial_adapt_n(n_control_2 = 75, n_arms_2 = c(0, 75)) |>
  cer_final_test(c(NA, 0.01))
#> 
#> ── Trial Design ────────────────────────────────────────────────────────────────
#> Testing the 2 hypotheses arm1 and arm2 at FWER 0.025.
#> There are 2 arms (arm1 and arm2), 1 endpoint and no subgroups.
#> The first stage sample size per arm/group is:
#>      arm  n
#>  control 50
#>     arm1 50
#>     arm2 50
#> 
#> ── Interim test ──
#> 
#> No hypotheses were rejected at the interim.
#> 
#> ── Adaptations ──
#> 
#> The following characteristics have been adapted:
#> • Hypotheses weights
#> • Graph Transition Matrix
#> • Correlation for parametric test
#> • Time fractions for the hypotheses
#> The second stage sample size per arm/group is:
#>      arm  n
#>  control 75
#>     arm1  0
#>     arm2 75
#> 
#> ── Final test result ──
#> 
#> No hypotheses were rejected.
```

A more thorough walkthrough of a more complicated example is at
`vignette("example_trial_design")`.

## References

The package implements methods explained in:

Mehta, Cyrus, Ajoy Mukhopadhyay, and Martin Posch (2025). “Graph Based,
Adaptive, Multiarm, Multiple Endpoint, Two-Stage Designs”. In:
Statistics in Medicine 44, e70237.

Other references:

Bretz, Frank et al. (2009). “A Graphical Approach to Sequentially
Rejective Multiple Test Procedures”. In: Statistics in Medicine 28,
pp. 586–604.

Bretz, Frank et al. (2011). “Graphical Approaches for Multiple
Comparison Procedures Using Weighted Bonferroni, Simes, or Parametric
Tests”. In: Biometrical Journal. Biometrische Zeitschrift 53,
pp. 894–913.

Klinglmueller, Florian, Martin Posch, and Franz Koenig (2014). “Adaptive
Graph-Based Multiple Testing Procedures”. In: Pharmaceutical Statistics
13, pp. 345–356.

Xi, Dong et al. (2017). “A Unified Framework for Weighted Parametric
Multiple Test Procedures”. In: Biometrical Journal. Biometrische
Zeitschrift 59, pp. 918–931.
