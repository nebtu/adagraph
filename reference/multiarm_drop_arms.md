# Drop arm from a multiarm design

This adapts a multiarm design by dropping a treatment arm and adjusting
the number of datapoints

## Usage

``` r
multiarm_drop_arms(
  design,
  arms,
  n_cont_2 = NA,
  n_treat_2 = NA,
  alt_adj = FALSE,
  adapt_bounds = TRUE
)
```

## Arguments

- design:

  multiarm_design object

- arms:

  list of with integers specifing which arms should be dropped

- n_cont_2, n_treat_2:

  list of number of datapoints for the control and treatment groups Can
  be different for different groups. The lenght should be equal to the
  number of control/treatment groups, including dropped treatments,
  which will always be set to 0

- alt_adj:

  uses
  [`cer_alt_drop_hypotheses()`](https://nebtu.github.io/adagraph/reference/cer_alt_drop_hypotheses.md)
  for dropping the hypotheses instead of
  [`cer_drop_hypotheses()`](https://nebtu.github.io/adagraph/reference/cer_drop_hypotheses.md)

- adapt_bounds:

  Adapt the bounds for rejecting a hypotheses to keep the FWER with the
  new adaptions, see
  [`cer_adapt()`](https://nebtu.github.io/adagraph/reference/cer_adapt.md)

## Details

The weights of the dropped hypotheses are set to 0 and distributed
according to the prespecified graph, similarly to
[`cer_drop_hypotheses()`](https://nebtu.github.io/adagraph/reference/cer_drop_hypotheses.md)
If specified, the time fraction of the interim test will also be adapted
according to the number of data points in the second stage, assuming the
first stage n was as planned accoring to t.

## Examples

``` r
as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))
design <- multiarm_cer_design(
 controls = 1,
 treatment_assoc = c(1,1),
 n_controls = 50,
 n_treatments = 50,
 weights = c(0.5, 0.5),
 alpha = 0.05,
 test_m = rbind(c(0, 1),
              c(1, 0)),
 alpha_spending_f = as,
 t = 0.5)

design <- cer_interim_test(design, c(0.1, 0.02))

design <- design |> multiarm_drop_arms(1)
design
#> A Multi-arm Design object, for testing 2 hypotheses at FWER 0.05.
#> 
#> ── Inital design specification ─────────────────────────────────────────────────
#> 
#> Hypotheses weights
#> [1] 0.5 0.5
#> 
#> Graph Transition Matrix
#>      [,1] [,2]
#> [1,]    0    1
#> [2,]    1    0
#> 
#> Correlation for parametric test
#>      [,1] [,2]
#> [1,]  1.0  0.5
#> [2,]  0.5  1.0
#> 
#> Number of control groups:
#> [1] 1
#> 
#> Treatment-to-control assignments (per treatment arm):
#> [1] 1 1
#> 
#> Planned sample sizes per control group:
#> [1] 50
#> 
#> Planned sample sizes per treatment group:
#> [1] 50 50
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
#> New correlation for parametric test
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    1
#> 
#> Second-stage sample sizes (controls):
#> [1] 25
#> 
#> Second-stage sample sizes (treatments):
#> [1]  0 25
#> 
#> Total planned sample sizes after adaptation (controls):
#> [1] 50
#> 
#> Total planned sample sizes after adaptation (treatments):
#> [1] 25 50
#> 
```
