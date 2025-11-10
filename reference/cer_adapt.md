# Adapt the trial design after the interim test

Adapt the trial design after the interim test

## Usage

``` r
cer_adapt(
  design,
  weights = NULL,
  test_m = NULL,
  time = NULL,
  correlation = NULL,
  adapt_bounds = TRUE
)
```

## Arguments

- design:

  A cer_design object

- weights:

  New weights vector Note that the lenght should be the same as in the
  prespecified design For dropping hypotheses, set the according weights
  to 0 or use
  [`cer_drop_hypotheses()`](https://nebtu.github.io/adagraph/reference/cer_drop_hypotheses.md)

- test_m:

  Adapted test matrix defining the graph for the closed test procedure
  to test the hypotheses

- time:

  adapted information fraction at which the first stage test occured.
  Note that this can now be a vector with a different value for
  different hypotheses or a single value

- correlation:

  adapted correlation matrix

- adapt_bounds:

  Adapt the bounds for rejecting a hypotheses to keep the FWER with the
  new adaptions. If doing multiple adaptions, it is enough to adapt
  bounds only for the last one, or call `adapt_bounds()` manually after.

## Value

An object of class cer_design, with the adaptions applied.

## Details

For all adaptions, adapt_bounds needs to be used only once, with or
after the last adaption. For this, either make sure that adapt_bounds is
TRUE, or use the `adapt_bounds()` function manually

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

#note that it is not necessary to do an interim test before,
#but that an interim test will only be done with the prespecified parameters
design <- cer_interim_test(design, c(0.1, 0.02))

design <- cer_adapt(design, weights = c(1/3, 2/3))
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
#> [1] 0.3333333 0.6666667
#> 
```
