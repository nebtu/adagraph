# Calculate bounds for preplanned test

Calculates the bounds and cJ values for the p-values for the interim
test and the planned final test

## Usage

``` r
cer_prep_bounds(correlation, weights, alpha, t)
```

## Arguments

- correlation:

  matrix describing the correlation structure of the hypotheses

- weights:

  list of weights of the given hypotheses, same length as hypotheses

- alpha:

  list of length 2 describing the amount of alpha spent that the test of
  the hypotheses should adhere to in the first and second stage
  respectively

- t:

  information fraction at which the first stage test is planned

## Value

A list with the following elements:

- bounds_1: vector of same length as weights with bounds for the first
  interim test

- cJ1: number that gets multiplied by the weights to get bounds_1

- bounds_1: vector of same lenght as weights with bounds for the planned
  final test

- cJ2: number that gets multiplied by the weights to get bounds_2

## Examples

``` r
#simple non-parametric bonferroni
cer_prep_bounds(
    correlation = rbind(c(1,0), c(0,1)),
    weights = c(0.5,0.5),
    alpha = c(0.001525323, 0.025),
    t = 0.5)
#> $bounds_1
#> [1] 0.000762953 0.000762953
#> 
#> $bounds_2
#> [1] 0.0122729 0.0122729
#> 
#> $cJ1
#> [1] 0.001525906
#> 
#> $cJ2
#> [1] 0.0245458
#> 

#weighted bonferroni with correlation 0.5
cer_prep_bounds(
    correlation = rbind(c(1,0.5), c(0.5,1)),
    weights = c(2/3,1/3),
    alpha = c(0.001525323, 0.025),
    t = 0.5)
#> $bounds_1
#> [1] 0.0010404644 0.0005202322
#> 
#> $bounds_2
#> [1] 0.017430042 0.008715021
#> 
#> $cJ1
#> [1] 0.001560697
#> 
#> $cJ2
#> [1] 0.02614506
#> 
```
