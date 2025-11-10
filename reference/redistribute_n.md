# Get the amount of people for each hypotheses in the 2. stage of a trial assuming a fixed population

Get the amount of people for each hypotheses in the 2. stage of a trial
assuming a fixed population

## Usage

``` r
redistribute_n(
  n,
  t = NA,
  n1 = NA,
  which_drop = NA,
  simple_redistribute = FALSE
)
```

## Arguments

- n:

  vector of number of people for each arm of the trial

- t:

  time fraction at which the second stage begins

- n1:

  number of people used in the first stage, instead of t

- which_drop:

  list of arms to drop, by their index in n

- simple_redistribute:

  if the distribution of the dropped arms to the rest has remainders,
  wether those remainders are distributed to the largest ramainders or
  simply to the first in the list

  Either t or n1 needs to be specified If \$t \* n\$ is not an integer,
  the number of people in the first stage is rounded down

## Value

list of number of people in the second stage for all the arms (including
dropped ones)

## Examples

``` r
redistribute_n(c(100, 100), t = 1/2, which_drop = 1)
#> [1]   0 100
```
