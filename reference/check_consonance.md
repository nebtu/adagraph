# Check if a design is consonant

For some CER design, see if the defined test sequence is consonant,
either from just it's graph and weights, also using the correlation
structure, or after some adaption.

## Usage

``` r
check_consonance(
  design,
  adapted = design$adaptions,
  stage = "both",
  weights = FALSE
)
```

## Arguments

- design:

  A cer_design object

- adapted:

  Boolean, use the adapted or original values

- stage:

  For which stage should the consonance be tested? Can be one of "both",
  "interim", "final". No effect if weights = TRUE.

- weights:

  Boolean, if TRUE uses the weights instead of the actual bounds. This
  is the same when no correlation structure is used, and else checks
  consonance of the counterfactual where the graph used is the same, but
  no use of correlation structure is made

## Value

A boolean value, TRUE if the design (for the specified weights) is
consonant, else FALSE

## Details

For every combination of hypothesis of a design, a intersection
hypothesis is being built that is keeping the specified alpha level, and
only when all intersection hypothesis holding a specific hypothesis are
rejected, that hypothesis is rejected. Consonance now means that
whenever a intersection hypothesis is rejected, at least one
intersection hypothesis that is a superset of this one is rejected. If a
design is not consonant, it is in some sense not optimal

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

check_consonance(design)
#> [1] TRUE
check_consonance(design, stage = "final")
#> [1] TRUE
```
