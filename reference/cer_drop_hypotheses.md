# Adapt a cer design by dropping hypotheses

The weights of the dropped hypotheses are set to 0 and distributed
according to the prespecified graph. However, the time fraction is not
adapted, this needs to be done manually if desired.

## Usage

``` r
cer_drop_hypotheses(design, drop_hyp, adapt_bounds = TRUE)
```

## Arguments

- design:

  cer_design object

- drop_hyp:

  that should be dropped, identified either by numbers or by their names

- adapt_bounds:

  Adapt the bounds for rejecting a hypotheses to keep the FWER with the
  new adaptations. If doing multiple adaptations, it is enough to adapt
  bounds only for the last one, or call `adapt_bounds()` manually after.

## Value

design with specified hypotheses dropped

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

design <- cer_drop_hypotheses(design, 1)
design
#> A CER Design object, for testing the 2 hypotheses H1 and H2 at FWER 0.05.
#> 
#> ── An interim test has been performed. ─────────────────────────────────────────
#> No hypotheses were rejected at the interim.
#> ── The following characteristics have been adapted: ────────────────────────────
#> • Hypotheses weights
#> • Graph Transition Matrix
#> ── No final test has been performed yet ────────────────────────────────────────
```
