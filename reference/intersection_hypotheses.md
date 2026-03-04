# Get information about the intersection hypothesis of a design

For given design, this returns for each intersection hypothesis the
weights and boundaries for rejecting. After adaptions, it also includes
the updated weights and boundaries.

## Usage

``` r
intersection_hypotheses(design, ...)

# S3 method for class 'cer_design'
intersection_hypotheses(design, ...)
```

## Arguments

- design:

  A cer_design object

- ...:

  additional parameters, not used

## Value

A dataframe of class `intersection_hypotheses`, where each row
corresponds to an intersection hypothesis. This class has a custom
[`print()`](https://rdrr.io/r/base/print.html) function, but works like
a data frame in any other aspect. Use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to see
the dataframe structure directly.
