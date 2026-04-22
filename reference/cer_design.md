# Make a new CER trial design

Returns an object of class `cer_design`. This can be used for clinical
trials with potential adaptations that are controlled for using the
conditional error method.

## Usage

``` r
cer_design(
  weights = double(),
  test_m = matrix(),
  alpha = double(),
  correlation = NA,
  t = 1/2,
  alpha_spending_f = function() {
 },
  seq_bonf = TRUE,
  names = NULL
)
```

## Arguments

- weights:

  List of weights, measuring how important each hypothesis is

- test_m:

  Transition matrix describing the graph for the closed test procedure
  to test the hypotheses

- alpha:

  Single number, measuring what total alpha should be spent on the FWER

- correlation:

  Correlation matrix describing the structure of the correlations
  between the different hypotheses, use NA for uncorrelated

- t:

  numeric between 0 and 1 specifing the planned time fraction for the
  interim test

- alpha_spending_f:

  alpha spending function, taking parameters alpha (for overall spent
  alpha) and t (information fraction at interim test)

- seq_bonf:

  to automatically reject hypotheses at the second stage if the sum of
  their PCER is greater 1

- names:

  optional names for the hypotheses

  If no names are provided in the `names` argument, the names of the
  `weights` arguments are used. If that is also unweighted, the names
  `H1`, `H2`, etc. are used.

## Value

An object of class `cer_design`

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

design
#> A CER Design object, for testing the 2 hypotheses H1 and H2 at FWER 0.05.
#> 
#> ── No interim test has been performed yet. ─────────────────────────────────────
#> ── No adaptations have been performed yet ──────────────────────────────────────
#> ── No final test has been performed yet ────────────────────────────────────────
```
