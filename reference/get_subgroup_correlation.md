# Get correlation between different subgroups and arms

Using the exact proportions/case numbers, calculate the correlations
between different subgroups and study arms

## Usage

``` r
get_subgroup_correlation(subgroups, arms, n_table, names_arms, names_subgroups)
```

## Arguments

- subgroups:

  Number of subgroups in addition to the whole collective (can be 0)

- arms:

  Number of arms

- n_table:

  A data.frame specifying the structure of the subgroups, see details
  for more information

- names_arms:

  names for the different arms.

- names_subgroups:

  names for the different subgroups

## Details

To calculate the subgroup and arm correlation structure, subgroups can
be provided either as proportions or patient numbers. Note that when
using the n_control and n_arm, this is translated to the following
structure internally as well. The structure (in argument n_table) should
be given as a dataframe, where each row specifies a specific subgroup in
a specific arm (or the control). Therefore the first column should be
named arm, and have values of "control" and the names of the arms. Then
should be columns for each of the subgroups (using the subgroup name as
a column name), with logical values, specifying the exact combination of
subgroups that are being specified. The last column should have name 'n'
and give the patient number for this exact intersection of subgroups.
(Or the proportion of all patients that are in this intersection, or the
same values multiplied by any other constant) Rows with no cases don't
have to be passed
