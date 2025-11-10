# Get the time fractions for given sample sizes

Get the information time fractions for the planned interim test for
given trial arms, given the sample sizes in treatment and control groups

## Usage

``` r
get_t(n_cont_1, n_cont_2, n_treat_1, n_treat_2)
```

## Arguments

- n_cont_1, n_cont_2:

  vector containing sample sizes for control groups in the first and
  second stage of the trial

- n_treat_1, n_treat_2:

  vector containing sample sizes for treatment groups in the first and
  second stage of the trial

## Examples

``` r
#trivial case with equal ratios throughout
get_t(50, 50, 50, 50)
#> [1] 0.5

#different sample sizes in the second trial stage, but constant allocation ratio
get_t(
  n_cont_1 = c(50, 50, 50, 50),
  n_cont_2 = c(50, 75, 100, 150),
  n_treat_1 = c(50, 50, 50, 50),
  n_treat_2 = c(50, 75, 100, 150)
)
#> [1] 0.5000000 0.4000000 0.3333333 0.2500000

#changing allocation ratio in the second stage:
# the rate and number of control group patients stays the same, but treatment
# group allocation is different
get_t(
  n_cont_1 = c(50, 50, 50, 50),
  n_cont_2 = c(50, 50, 50, 50),
  n_treat_1 = c(50, 50, 50, 50),
  n_treat_2 = c(25, 50, 100, 150)
)
#> [1] 0.6000000 0.5000000 0.4285714 0.4000000
```
