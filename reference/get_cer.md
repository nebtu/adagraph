# Get the Conditional Error Rate for a intersection of hypotheses

Gives the CER (condtional error rate) for a given set of hypotheses,
with arbitrary weights and correlation between the hypotheses. This is
an upper bound on the probabilty of rejecting all the hypotheses with
weight greater 0 under the null hypothesis conditional on the stage one
data, assuming we reject whenever a final p-value is smaller than cJ2 \*
weight This assumes that we did not reject in the interim already, in
which case it should be 1 anyway.

## Usage

``` r
get_cer(p_values, weights, cJ2, correlation, t)
```

## Arguments

- p_values:

  vector of stage 1 p-values for the hypothesis

- weights:

  weight to give to each hypothesis, ignoring all with weight 0

- cJ2:

  factor used for deciding if the hypothesis should be rejected

- correlation:

  matrix describing a potential known correlation structure between some
  hypotheses. Use NA for unkown correlations

- t:

  information time fraction at which the interim test is performed

## Value

a single number greater than 0, the CER

## Details

Note that if the correlation between some values is unkown, the result
may be greater than 1, see also the examples

## Examples

``` r
#the CER is high (even >1) if the p_values of the first stage are already low
get_cer(
 c(0.01, 0.01, 0.9, 0.9),
 c(1, 1, 0, 0),
 0.05,
 matrix(rep(NA, 16), nrow = 4),
 0.5
)
#> [1] 1.000138

#and lower otherwise
get_cer(
 c(0.01, 0.01, 0.9, 0.9),
 c(0, 0, 1, 1),
 0.05,
 matrix(rep(NA, 16), nrow = 4),
 0.5
)
#> [1] 0.0003088926
```
