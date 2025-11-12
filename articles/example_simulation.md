# Example for simulating a multiarm trial with adagraph

This vignette describes how to run a simulation of a two-stage clinical
trial with 4 endpoints and 2 different dose regiments.

``` r
library(adagraph)
```

First, we need to define our design object. Here, we use the simulation
design provided by the package, defined by using `get_sim_design()`. It
is taken from the description in section 4 of Mehta 2025
([arXiv:2501.03197](https://arxiv.org/abs/2501.03197)). We assume here a
sample size of 100 people for each of the 2 dose regiments and the
control group. Also, we assume that after half of the results are in, an
interim analysis is performed, which results in a time fraction of
$0.5$.

``` r
ws <- .75 # (weight given to secondary endpoint)
wp <- (1 - ws) / 3 # weight given to the other arms primary endpoint
m <- rbind(
  H1 = c(0, wp, wp, wp, ws, 0, 0, 0),
  H2 = c(wp, 0, wp, wp, 0, ws, 0, 0),
  H3 = c(wp, wp, 0, wp, 0, 0, ws, 0),
  H4 = c(wp, wp, wp, 0, 0, 0, 0, ws),
  H5 = c(0, 1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0),
  H6 = c(1 / 3, 0, 1 / 3, 1 / 3, 0, 0, 0, 0),
  H7 = c(1 / 3, 1 / 3, 0, 1 / 3, 0, 0, 0, 0),
  H8 = c(1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0, 0)
)
```

``` r
#alpha spending function
as <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))

design <- multiarm_cer_design(
  controls = 2,
  treatment_assoc = c(1, 1, 1, 1, 2, 2, 2, 2),
  n_controls = 100,
  n_treatments = 100,
  weights = c(1 / 4, 1 / 4, 1 / 4, 1 / 4, 0, 0, 0, 0),
  t = 1 / 2,
  alpha = 0.025,
  test_m = m,
  alpha_spending_f = as
)

design
#> A Multi-arm Design object, for testing 8 hypotheses at FWER 0.025.
#> 
#> ── Inital design specification ─────────────────────────────────────────────────
#> 
#> Hypotheses weights
#> [1] 0.25 0.25 0.25 0.25 0.00 0.00 0.00 0.00
#> 
#> Graph Transition Matrix
#>          [,1]       [,2]       [,3]       [,4] [,5] [,6] [,7] [,8]
#> H1 0.00000000 0.08333333 0.08333333 0.08333333 0.75 0.00 0.00 0.00
#> H2 0.08333333 0.00000000 0.08333333 0.08333333 0.00 0.75 0.00 0.00
#> H3 0.08333333 0.08333333 0.00000000 0.08333333 0.00 0.00 0.75 0.00
#> H4 0.08333333 0.08333333 0.08333333 0.00000000 0.00 0.00 0.00 0.75
#> H5 0.00000000 0.33333333 0.33333333 0.33333333 0.00 0.00 0.00 0.00
#> H6 0.33333333 0.00000000 0.33333333 0.33333333 0.00 0.00 0.00 0.00
#> H7 0.33333333 0.33333333 0.00000000 0.33333333 0.00 0.00 0.00 0.00
#> H8 0.33333333 0.33333333 0.33333333 0.00000000 0.00 0.00 0.00 0.00
#> 
#> Correlation for parametric test
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]  1.0  0.5  0.5  0.5   NA   NA   NA   NA
#> [2,]  0.5  1.0  0.5  0.5   NA   NA   NA   NA
#> [3,]  0.5  0.5  1.0  0.5   NA   NA   NA   NA
#> [4,]  0.5  0.5  0.5  1.0   NA   NA   NA   NA
#> [5,]   NA   NA   NA   NA  1.0  0.5  0.5  0.5
#> [6,]   NA   NA   NA   NA  0.5  1.0  0.5  0.5
#> [7,]   NA   NA   NA   NA  0.5  0.5  1.0  0.5
#> [8,]   NA   NA   NA   NA  0.5  0.5  0.5  1.0
#> 
#> Number of control groups:
#> [1] 2
#> 
#> Treatment-to-control assignments (per treatment arm):
#> [1] 1 1 1 1 2 2 2 2
#> 
#> Planned sample sizes per control group:
#> [1] 100 100
#> 
#> Planned sample sizes per treatment group:
#> [1] 100 100 100 100 100 100 100 100
#> 
#> Interim test is planned at time fraction 0.5
```

Next, we need to specify how we want the data used in the simulation to
be specified. For this, we need a function that takes a number of trials
to simulate and the design object and returns the p-values for the
simulation. While it is possible to write such a function by hand, for
this case we can use the provided
[`get_data_gen()`](https://nebtu.github.io/adagraph/reference/get_data_gen.md)
and
[`get_data_gen_2()`](https://nebtu.github.io/adagraph/reference/get_data_gen_2.md)
functions. We first define the correlation between the different
endpoints of the same group and the effectiveness of the treatments.
Here, we want assume that none of the treatments have an effect, so we
can estimate the familywise error rate (FWER).

``` r
eff <- c(0, 0, 0, 0)

mu <- c(eff, eff)

corr <- 0.8
corr_control <- rbind(
  c(1, corr),
  c(corr, 1)
)

corr_treatment <- rbind(
  c(1, 0, 0, 0, corr, 0, 0, 0),
  c(0, 1, 0, 0, 0, corr, 0, 0),
  c(0, 0, 1, 0, 0, 0, corr, 0),
  c(0, 0, 0, 1, 0, 0, 0, corr),
  c(corr, 0, 0, 0, 1, 0, 0, 0),
  c(0, corr, 0, 0, 0, 1, 0, 0),
  c(0, 0, corr, 0, 0, 0, 1, 0),
  c(0, 0, 0, corr, 0, 0, 0, 1)
)

data_gen_1 <- get_data_gen(
  corr_control,
  corr_treatment,
  mu,
  50,
  50
)
data_gen_2 <- get_data_gen_2(
  corr_control,
  corr_treatment,
  mu
)
```

Next we need to define how we want to adapt our trial once the interim
results are generated and tested for. Here, we define a simple adaption
were any arms where the p-value for the primary enpoint is higher than
0.75 is dropped. (Addtionally, we also dont continue any arms that are
already rejected in both primary and secondary endpoints.) We need to
take care when updating the number of people participating in the second
stage. We assume that the total amount of people that can be recruted
stays constant. This means that when we drop an arm, those people can be
distributed to the remaining arms. But the trial design object has no
knowledge saved about which arms correspond to the same people and only
measure different endpoints. This means we must calculate the new sample
sizes with only the arms, not the enpoints, using the
[`redistribute_n()`](https://nebtu.github.io/adagraph/reference/redistribute_n.md)
function.

``` r
futility <- 0.75

adapt_func <- function(design) {
  p <- design$p_values_interim[1:4]

  #treatments, were both primary and secndary enpoints are rejected
  trt_rej <- design$rej_interim[1:4] & design$rej_interim[5:8]

  drop_hyp <- (p > futility) | trt_rej

  drop_arms <- which(drop_hyp)

  new_n <- redistribute_n(
    n = c(design$n_treatments[1:4], design$n_controls[1]),
    t = design$t,
    which_drop = drop_arms
  )

  design |>
    multiarm_drop_arms(
      drop_arms,
      n_cont_2 = rep(new_n[5], 2),
      n_treat_2 = rep(new_n[1:4], 2),
      alt_adj = FALSE
    )
}
```

Now we have all the pieces to simulate our trials. Since the most
computationally intensive step here is to calculate the adjusted bound
for rejecting hypotheses after the adaption, we simulate for every first
stage multiple second stages, in this case 10.

``` r
data <- sim_trial(design, 10, 10, adapt_func, data_gen_1, data_gen_2)
print(dim(data))
#> [1] 100  34
print(names(data))
#>  [1] "run1"          "rej_1"         "rej_2"         "rej_3"        
#>  [5] "rej_4"         "rej_5"         "rej_6"         "rej_7"        
#>  [9] "rej_8"         "rej_any"       "p_1"           "p_2"          
#> [13] "p_3"           "p_4"           "p_5"           "p_6"          
#> [17] "p_7"           "p_8"           "interim_rej_1" "interim_rej_2"
#> [21] "interim_rej_3" "interim_rej_4" "interim_rej_5" "interim_rej_6"
#> [25] "interim_rej_7" "interim_rej_8" "interim_p_1"   "interim_p_2"  
#> [29] "interim_p_3"   "interim_p_4"   "interim_p_5"   "interim_p_6"  
#> [33] "interim_p_7"   "interim_p_8"
```

As a result, we get a dataframe with a row for every complete trial we
simulated, noting which hypotheses were rejected at each stage and what
the resulting p-values were. If we need any addtional information on the
trials that is not already included, we could also return the used
multiarm_cer_design objects with the `include_designs` option.

Now we can see the resulting FWER by simply calculating it from the
results.

``` r
print(mean(data$rej_any))
#> [1] 0.06
```
