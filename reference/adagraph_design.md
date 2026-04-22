# Make a new (generic) trial design

Make a new (generic) trial design

## Usage

``` r
adagraph_design(
  weights = double(),
  test_m = matrix(),
  alpha = double(),
  correlation = NA,
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
  between the different hypotheses, use NA for uncorrelated. Defaults to
  no known correlation.

- names:

  optional names for the hypotheses

## Value

An object of class adagraph_design

## Examples

``` r
design <- adagraph_design(
 correlation=rbind(H1=c(1, NA),
                   H2=c(NA, 1)),
 weights=c(2/3, 1/3),
 alpha=0.05,
 test_m=rbind(c(0, 1),
              c(1, 0)))
design
#> A Adagraph Design object, for testing the 2 hypotheses H1 and H2 at FWER 0.05.
#> 
#> ── No interim test has been performed yet. ─────────────────────────────────────
#> ── No adaptations have been performed yet ──────────────────────────────────────
#> ── No final test has been performed yet ────────────────────────────────────────
```
