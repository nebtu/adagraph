# Make a new (generic) trial design

Make a new (generic) trial design

## Usage

``` r
adagraph_design(
  correlation = matrix(),
  weights = double(),
  alpha = double(),
  test_m = matrix()
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
#> A Adagraph Design object, for testing 2 hypotheses at FWER 0.05.
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
```
