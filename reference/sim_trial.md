# Run a series of simulated trials according to a specified design

Run a series of simulated trials according to a specified design

## Usage

``` r
sim_trial(
  design,
  runs1,
  runs2,
  adapt_rule,
  data_gen_1,
  data_gen_2,
  include_designs = FALSE
)
```

## Arguments

- design:

  cer_design or multiarm_design object to be used for the simulation

- runs1:

  Number of trials to run in the first stage

- runs2:

  Number of second stage to run for every first stage trial

- adapt_rule:

  function that takes a cer_design object after the first interim test
  and returns the same object with appropiate adaptions

- data_gen_1:

  function for generating first stage data, see details

- data_gen_2:

  function for generating second stage data, see details

- include_designs:

  boolean indicating whether to include the designs objects in the
  output

## Value

A dataframe containing the various results

## Details

data_gen_1 and data_gen_2 should take two arguments, the number of
trials to simulate and the adapted design object, and return a matrix or
dataframe with each row being the p-values for a single trial.

See also
[`get_data_gen()`](https://nebtu.github.io/adagraph/reference/get_data_gen.md)
and
[`get_data_gen_2()`](https://nebtu.github.io/adagraph/reference/get_data_gen_2.md)
for further information on how the input and output of data generating
functions should look.

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

adaption <- function(design) {
  design |> multiarm_drop_arms(1)
}

data_gen <- get_data_gen(
  matrix(1),
  rbind(
   c(1, 0.5),
   c(0.5, 1)
 ),
  c(0, 0.3),
  100,
  100
)

data_gen_2 <- get_data_gen_2(
  matrix(1),
  rbind(
   c(1, 0.5),
   c(0.5, 1)
  ),
  c(0, 0)
)

sim_trial(
  design,
  5,
  3,
  adaption,
  data_gen,
  data_gen_2
)
#>    run1 rej_1 rej_2 rej_any p_1        p_2 interim_rej_1 interim_rej_2
#> 1     1 FALSE  TRUE    TRUE   1 0.01448593         FALSE          TRUE
#> 2     1 FALSE  TRUE    TRUE   1 0.02949321         FALSE          TRUE
#> 3     1 FALSE  TRUE    TRUE   1 0.01318780         FALSE          TRUE
#> 4     2 FALSE FALSE   FALSE   1 0.61156774         FALSE         FALSE
#> 5     2 FALSE FALSE   FALSE   1 0.52810949         FALSE         FALSE
#> 6     2 FALSE FALSE   FALSE   1 0.83927697         FALSE         FALSE
#> 7     3 FALSE FALSE   FALSE   1 0.07885208         FALSE         FALSE
#> 8     3 FALSE FALSE   FALSE   1 0.06541350         FALSE         FALSE
#> 9     3 FALSE FALSE   FALSE   1 0.02679034         FALSE         FALSE
#> 10    4 FALSE  TRUE    TRUE   1 0.01389317         FALSE         FALSE
#> 11    4 FALSE FALSE   FALSE   1 0.09854194         FALSE         FALSE
#> 12    4 FALSE  TRUE    TRUE   1 0.02575248         FALSE         FALSE
#> 13    5 FALSE FALSE   FALSE   1 0.55841748         FALSE         FALSE
#> 14    5 FALSE FALSE   FALSE   1 0.35244674         FALSE         FALSE
#> 15    5 FALSE FALSE   FALSE   1 0.25244952         FALSE         FALSE
#>    interim_p_1 interim_p_2
#> 1    0.3815457 0.001889621
#> 2    0.3815457 0.001889621
#> 3    0.3815457 0.001889621
#> 4    0.9800523 0.515780672
#> 5    0.9800523 0.515780672
#> 6    0.9800523 0.515780672
#> 7    0.7844791 0.008659191
#> 8    0.7844791 0.008659191
#> 9    0.7844791 0.008659191
#> 10   0.6278657 0.045058499
#> 11   0.6278657 0.045058499
#> 12   0.6278657 0.045058499
#> 13   0.8622252 0.445776016
#> 14   0.8622252 0.445776016
#> 15   0.8622252 0.445776016
```
