

<!-- README.md is generated from README.qmd. Please edit that file -->

# adagraph

<!-- badges: start -->

<!-- badges: end -->

Design a two-stage adaptive trial using a graph-based closed-testing
sequence, while controlling the FWER using the conditional error rate
method.

## Installation

You can install the development version of adagraph from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nebtu/adagraph")
```

## Example

``` r
library(adagraph)

alpha_spending_f <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
design <- cer_design(
  correlation = rbind(H1 = c(1, NA), H2 = c(NA, 1)),
  weights = c(2 / 3, 1 / 3),
  alpha = 0.05,
  test_m = rbind(c(0, 1), c(1, 0)),
  alpha_spending_f = alpha_spending_f,
  t = 0.5
)


design <- cer_interim_test(design, c(0.001, 0.02)) |>
  cer_drop_hypotheses(c(TRUE, FALSE)) |>
  cer_adapt_bounds() |>
  cer_final_test(c(NA, 0.01))

design
#> A CER Design object, for testing 2 hypotheses at FWER 0.05.
#> 
#> ── Inital design specification ─────────────────────────────────────────────────
#> 
#> Hypotheses weights
#> [1] 0.6666667 0.3333333
#> 
#> Graph Transition Matrix
#>      [,1] [,2]
#> [1,]    0    1
#> [2,]    1    0
#> 
#> Correlation for parametric test
#>    [,1] [,2]
#> H1    1   NA
#> H2   NA    1
#> 
#> Interim test is planned at time fraction 0.5
#> 
#> ── Interim test result ─────────────────────────────────────────────────────────
#> 
#> P-values of interim test are:
#> [1] 0.001 0.020
#> Hypotheses rejected at the interim: 1
#> 
#> ── Adaptions from inital specification ─────────────────────────────────────────
#> 
#> New hypotheses weights
#> [1] 0 1
#> 
#> New graph Transition Matrix
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    0
#> 
#> ── Final test result ───────────────────────────────────────────────────────────
#> 
#> Overall p-values of the hypotheses are:
#> [1] 1.00 0.01
#> Hypotheses rejected: 
#> [1] 1 2
```
