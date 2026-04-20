# Make a new trial design with multiple arms, endpoints or subgroups

Returns an object of class `trial_design`, representing a two-stage
clinical trial design, allowing adaptation using the CER method

## Usage

``` r
trial_design(
  arms = 1,
  endpoints = 1,
  subgroups = 0,
  n_control = NULL,
  n_arms = NULL,
  n_table = NULL,
  weights = double(),
  t = double(),
  alpha = double(),
  test_m = matrix(),
  alpha_spending_f = function() {
 },
  seq_bonf = TRUE,
  names_arms = NULL,
  names_endpoints = NULL,
  names_subgroups = NULL,
  names = NULL
)
```

## Arguments

- arms:

  Number of arms

- endpoints:

  Number of endpoints

- subgroups:

  Number of subgroups in addition to the whole collective (can be 0)

- n_control:

  Integer determining the number of patients in the control group, if
  there are no subgroups

- n_arms:

  Integer (or vector of integers) determining the number of patients in
  each arm, if there are no subgroups

- n_table:

  A data.frame specifying the structure of the subgroups, see details
  for more information

- weights:

  List of weights, measuring how important each hypothesis is. See
  details for numbering of hypotheses

- t:

  information fraction, at which fraction of assigned people will the
  interim analysis happen

- alpha:

  Single number, measuring what total alpha should be spent on the FWER

- test_m:

  Transition matrix describing the graph for the closed test procedure
  to test the hypotheses

- alpha_spending_f:

  alpha spending function, taking parameters alpha (for overall spent
  alpha) and t (information fraction at interim test)

- seq_bonf:

  automatically reject hypotheses at the second stage if the sum of
  their PCER is greater 1

- names_arms:

  names for the different arms. If not provided, they will be
  automatically generated to A1, A2, etc.

- names_endpoints:

  names for the different endpoints If not provided, they will be
  automatically generated to E1, E2, etc.

- names_subgroups:

  names for the different subgroups. If not provided, they will be
  automatically generated to G1, G2, etc.

- names:

  optional names for the hypotheses. If not provided, they are generated
  from the names of the endpoints, arms and subgroups

## Value

An object of class `trial_design`

## Details

The ordering of of the hypothesis is automatically determined in the
following way: All hypotheses associated with on endpoint are grouped
together, so first all arms with their first endpoint, than all arms
with the second endpoint, etc. If there are multiple subgroups, these
subgroups follow (with the same structure) after the full population.

If no names for some structure are provided, they are determined
automatically. For endpoints, the default names are E1, E2, ..., for
arms, they are A1, A2, ... and for subgroups G1, G2, .. are used.

To calculate the subgroup and arm correlation structure, subgroups can
be provided either as proportions or patient numbers. Note that when
using the n_control and n_arm, this is translated to the following
structure internally as well. The structure (in argument n_table) should
be given as a dataframe, where each row specifies a specific subgroup in
a specific arm (or the control). Therefore the first column should be
named arm, and have values of "control" and the names of the arms. Then
should be columns for each of the subgroups (using the subgroup name as
a column name), with logical values, specifying the exact combination of
subgroups that are being specified. The last column should be either
names 'n' (for case numbers) or 'prop' (for proportions) and give the
given value for this exact intersection of subgroups. Those are the
numbers/proportions for the first stage. Assuming that the proprortions
do not stay exactly the same in the second stage, adapting for the new
proportions is necessary. The pre-planned test without adaptations
assumes that the group and arm patient numbers stay the same relative to
each other. The size of the second trial in comparison to the first is
determined by t.

## Examples

``` r
as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))

m <- rbind(
  H1 = c(0, 1 / 2, 1 / 2, 0),
 H2 = c(1 / 2, 0, 0, 1 / 2),
  H3 = c(0, 1, 0, 0),
  H4 = c(1, 0, 0, 0)
)
design <- trial_design(
 arms = 2,
 endpoints = 2,
 n_control = 50,
 n_arms = c(50, 50),
 weights = c(0.5, 0.5, 0, 0), #all weight is at first on the first endpoint,
                               #on both arms equally
 alpha = 0.05,
 test_m = m,
 alpha_spending_f = as,
 t = 0.5
)

design
#> A trial Design object, for testing the 4 hypotheses E1_A1, E1_A2, E2_A1, and E2_A2 at FWER 0.05.
#> 
#> There are 2 arms (A1 and A2), 2 endpoints (E1 and E2) and no subgroups.
#> The first stage sample size per arm/group is:
#>      arm  n
#>  control 50
#>       A1 50
#>       A2 50
#> 
#> ── No interim test has been performed yet. ─────────────────────────────────────
#> ── No adaptations have been performed yet ──────────────────────────────────────
#> ── No final test has been performed yet ────────────────────────────────────────
```
