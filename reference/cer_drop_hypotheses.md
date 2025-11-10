# Adapt a cer design by dropping hypotheses

The weights of the dropped hypotheses are set to 0 and distributed
according to the prespecified graph. However, the time fraction is not
adapted, this needs to be done manually if desired.

## Usage

``` r
cer_drop_hypotheses(design, hypotheses, adapt_bounds = TRUE)
```

## Arguments

- design:

  cer_design object

- hypotheses:

  vector of booleans indicating for each hypotheses if it should be
  dropped

- adapt_bounds:

  Adapt the bounds for rejecting a hypotheses to keep the FWER with the
  new adaptions. If doing multiple adaptions, it is enough to adapt
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

design <- cer_drop_hypotheses(design, c(TRUE, FALSE))
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
#> ── Interim test result ─────────────────────────────────────────────────────────
#> 
#> P-values of interim test are:
#> [1] 0.10 0.02
#> No Hypotheses were rejected at the interim
#> 
#> ── Adaptions from inital specification ─────────────────────────────────────────
#> 
#> New hypotheses weights
#> [1] 0 1
#> 
#> New graph Transition Matrix
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    0
#> 
```
