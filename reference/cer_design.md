# Make a new CER trial design

Returns an object of class `cer_design`. This can be used for clinical
trials with potential adaptions that are controlled for using the
conditional error method.

## Usage

``` r
cer_design(
  correlation = matrix(),
  weights = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {
 },
  t = double(),
  seq_bonf = TRUE
)
```

## Arguments

- correlation:

  Correlation matrix describing the structure of the correlations
  between the different hypotheses, use NA for uncorrelated

- weights:

  List of weights, measuring how important each hypothesis is

- alpha:

  Single number, measuring what total alpha should be spent on the FWER

- test_m:

  Transition matrix describing the graph for the closed test procedure
  to test the hypotheses

- alpha_spending_f:

  alpha spending function, taking parameters alpha (for overall spent
  alpha) and t (information fraction at interim test)

- t:

  numeric between 0 and 1 specifing the planned time fraction for the
  interim test

- seq_bonf:

  to automatically reject hypotheses at the second stage if the sum of
  their PCER is greater 1

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
```
