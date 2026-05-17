# Test a cer design for early rejection of hypotheses and calculate the CER for adaptations

Test a cer design for early rejection of hypotheses and calculate the
CER for adaptations

## Usage

``` r
cer_interim_test(design, p_values)
```

## Arguments

- design:

  A cer_design object

- p_values:

  A vector of p-values for the hypotheses. If named, the values are
  automatically reordered to match the hypothesis order in the design.
  Names must match the hypothesis names exactly.

## Value

a `cer_design` object, which now also includes the CER for each
hypothesis and the rejection status of the hypotheses.

## Examples

``` r
as <- function(x,t) 2-2*pnorm(qnorm(1-x/2)/sqrt(t))
design <- cer_design(
 correlation=rbind(H1=c(1, NA),
                   H2=c(NA, 1)),
 weights=c(2/3, 1/3),
 alpha=0.05,
 test_m=rbind(c(0, 1),
              c(1, 0)),
 alpha_spending_f=as,
 t=0.5)

design <- cer_interim_test(design, c(0.001, 0.02))
design
#> 
#> ── CER Design ──────────────────────────────────────────────────────────────────
#> Testing the 2 hypotheses H1 and H2 at FWER 0.05.
#> 
#> ── Interim test ──
#> 
#> Hypotheses rejected: H1
#> 
#> ── No adaptations performed ──
#> 
#> ── No final test performed ──
#> 
```
