# Make a new multiarm CER trial design

Returns an object of class `multiarm_cer_design`. This can be used for
clinical trials with multiple arms and control groups that might have
adaptations controlled for using the conditional error method.

## Usage

``` r
multiarm_cer_design(
  controls = integer(),
  treatment_assoc = integer(),
  n_controls = integer(),
  n_treatments = integer(),
  weights = double(),
  t = double(),
  alpha = double(),
  transitions = matrix(),
  alpha_spending = 0,
  seq_bonf = TRUE
)
```

## Arguments

- controls:

  Number of control groups

- treatment_assoc:

  Vector of integer, corresponding to the number of the control group
  for each treatment group. The length determines the number of
  treatment groups

- n_controls:

  Integer (or vector of integers) determining the number of patients in
  each control group

- n_treatments:

  Integer (or vector of integers) determining the number of patients in
  each treatment group

- weights:

  List of weights, measuring how important each hypothesis is

- t:

  information fraction, at which fraction of assigned people will the
  interim analysis happen

- alpha:

  Single number, measuring what total alpha should be spent on the FWER

- transitions:

  Transition matrix describing the graph for the closed test procedure
  to test the hypotheses

- alpha_spending:

  either alpha spending function, taking parameters alpha (for overall
  spent alpha) and t (information fraction at interim test), or the
  amount of alpha to spend at interim as a double

- seq_bonf:

  automatically reject hypotheses at the second stage if the sum of
  their PCER is greater 1

## Value

An object of class `multiarm_cer_design`

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
 transitions = rbind(c(0, 1),
              c(1, 0)),
 alpha_spending = as,
 t = 0.5)

design
#> 
#> ── Multi-arm Design ────────────────────────────────────────────────────────────
#> Testing the 2 hypotheses H1 and H2 at FWER 0.05.
#> There are 1 control groups for a total of 2 hypotheses.
#> 
#> ── No interim test performed ──
#> 
#> ── No adaptations performed ──
#> 
#> ── No final test performed ──
#> 
```
