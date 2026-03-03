# Adjust bounds after changing some design parameters

This function calculates the new bounds for the p-values for the final
test. It should be run once after finishing all adaptions after the
interim test, if the `adapt_bounds` option was not true for the last
adaption anyway.

## Usage

``` r
cer_adapt_bounds(design)
```

## Arguments

- design:

  A cer_design object

## Value

A cer_design object, with the new bounds calculated

## Examples

``` r
as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))
design <- cer_design(
 correlation=rbind(H1=c(1, NA),
                   H2=c(NA, 1)),
 weights=c(2/3, 1/3),
 alpha=0.05,
 test_m=rbind(c(0, 1),
              c(1, 0)),
 alpha_spending_f=as,
 t=0.5)

design <- cer_interim_test(design, c(0.1, 0.02))

design <- cer_drop_hypotheses(design, c(TRUE, FALSE))
design <- cer_adapt_bounds(design)

design
#> A CER Design object, for testing 2 hypotheses at FWER 0.05.
#> 
#> ── An interim test has been performed. ─────────────────────────────────────────
#> No Hypotheses were rejected at the interim.
#> ── The following characteristics have been adapted: ────────────────────────────
#> • Hypotheses weights
#> • Graph Transition Matrix
#> ── No final test has been performed yet ────────────────────────────────────────
```
