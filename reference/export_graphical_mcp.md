# Export the closed testing strategy as a graphicalMCP graph object

Export the original or adapted graph and weights of an adagraph design
to an graph object as used by graphicalMCP. This allows to use the
testing and graphing methods of gMCPLite for the testing strategy.

## Usage

``` r
export_graphical_mcp(design, adapted = design$adaptations)
```

## Arguments

- design:

  the adagraph_design object that should be exported

- adapted:

  boolean indicating if the adapted version of the testing strategy
  should be exported. Defaults to TRUE if an adaptation has been made,
  FALSE else

## Value

An S3 object of class `initial_graph`, with hypotheses weights and
matrix of the input design

## Examples

``` r
m <- rbind(
  H1 = c(0, 1 / 2, 1 / 2, 0),
  H2 = c(1 / 2, 0, 0, 1 / 2),
  H3 = c(0, 1, 0, 0),
  H4 = c(1, 0, 0, 0)
)
weights <- c(1 / 2, 1 / 2, 0, 0)
as = function(x, t) 2 - 2 * stats::pnorm(stats::qnorm(1 - x / 2) / sqrt(t))
design <- cer_design(
  weights = weights,
  alpha = 0.05,
  test_m = m,
  alpha_spending_f = as,
  t = 0.5,
  correlation = diag(4)
)

export_graphical_mcp(design)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.5
#> H3: 0.0
#> H4: 0.0
#> 
#> --- Transition weights ---
#>      H1  H2  H3  H4
#>  H1 0.0 0.5 0.5 0.0
#>  H2 0.5 0.0 0.0 0.5
#>  H3 0.0 1.0 0.0 0.0
#>  H4 1.0 0.0 0.0 0.0
```
