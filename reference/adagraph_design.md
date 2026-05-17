# Make a new (generic) trial design

This is a generic S3 class for graph-based closed testing strategies. It
does not implement any testing functions, but can serve as a basis for
implementing different methods for (adaptive) testing strategies. To use
graphical closed testing designs, use
[`cer_design()`](https://nebtu.github.io/adagraph/reference/cer_design.md)
and
[`trial_design()`](https://nebtu.github.io/adagraph/reference/trial_design.md).

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

## Details

If `weights`, `test_m`, or `correlation` are named (via
[`names()`](https://rdrr.io/r/base/names.html) for vectors or
[`rownames()`](https://rdrr.io/r/base/colnames.html)/[`colnames()`](https://rdrr.io/r/base/colnames.html)
for matrices), they are automatically reordered to match the canonical
hypothesis order. The canonical order is determined by the `names`
argument if provided, otherwise by `names(weights)`, otherwise `H1`,
`H2`, etc. An error is raised if the names do not match the expected
hypothesis names.

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
#> 
#> ── Adagraph Design ─────────────────────────────────────────────────────────────
#> Testing the 2 hypotheses H1 and H2 at FWER 0.05.
#> 
#> ── No interim test performed ──
#> 
#> ── No adaptations performed ──
#> 
#> ── No final test performed ──
#> 
```
