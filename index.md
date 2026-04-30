# adagraph

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
  cer_drop_hypotheses(1) |>
  cer_final_test(c(NA, 0.01))

design
#> A CER Design object, for testing 2 hypotheses at FWER 0.05.
#> 
#> ── An interim test has been performed. ─────────────────────────────────────────
#> Hypotheses rejected at the interim: H1
#> ── The following characteristics have been adapted: ────────────────────────────
#> • Hypotheses weights
#> • Graph Transition Matrix
#> ── Final test result ───────────────────────────────────────────────────────────
#> Hypotheses rejected: H1 and H2
```
