# Adapt a cer design by dropping hypotheses, with a simplified strategy for redistributing weights

The weights of the dropped hypotheses are set to 0 and distributed
proportionally to the previous weight to the other hypotheses. This
means the weights "stay the same", they are only adapted to still sum up
to 1. Note that this method does not lead to an adapted graph that is
coherent with the actual weight distribution, this may lead to problems
down the line

## Usage

``` r
cer_alt_drop_hypotheses(design, hypotheses, adapt_bounds = TRUE)
```

## Arguments

- design:

  cer_design object

- hypotheses:

  vector of booleans indicating for each hypotheses if it should be
  dropped

- adapt_bounds:

  Adapt the bounds for rejecting a hypotheses to keep the FWER with the
  new adaptations. If doing multiple adaptations, it is enough to adapt
  bounds only for the last one, or call `adapt_bounds()` manually after.

## Value

design with specified hypotheses dropped (so TRUE means the hypothesis
is dropped)

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

design <- cer_alt_drop_hypotheses(design, c(TRUE, FALSE))
design
#> A CER Design object, for testing the 2 hypotheses H1 and H2 at FWER 0.05.
#> 
#> ── An interim test has been performed. ─────────────────────────────────────────
#> No hypotheses were rejected at the interim.
#> ── The following characteristics have been adapted: ────────────────────────────
#> • Hypotheses weights
#> ── No final test has been performed yet ────────────────────────────────────────
```
