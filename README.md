
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ife

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ife)](https://CRAN.R-project.org/package=ife)
<!-- badges: end -->

S7 class for influence function estimands with forward mode automatic
differentiation for variance estimation.

## Installation

You can install the development version of *ife* from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nt-williams/ife")
```

## Example

Consider estimating the population mean outcome under treatment
$E[E[Y \mid A=1,W]]$ and control $E[E[Y \mid A=0,W]]$ using augmented
inverse probability weighting (AIPW).

``` r
library(ife)

set.seed(7654)
# Generate simulated data
n <- 500
w <- runif(n)                                   # confounder
a <- rbinom(n, 1, 0.5)                          # treatment (randomized)
y <- rbinom(n, 1, plogis(-0.75 + a + w))        # outcome

# Create data-frames for counterfactual predictions
foo <- data.frame(w, a, y)
foo1 <- foo0 <- foo
foo1$a <- 1  # everyone treated
foo0$a <- 0  # everyone untreated

# Fit outcome model and generate predictions
pi <- 0.5  # known propensity score
m <- glm(y ~ a + w, data = foo, family = binomial())
Qa <- predict(m, type = "response")                    # predicted outcomes
Q1 <- predict(m, newdata = foo1, type = "response")    # under treatment
Q0 <- predict(m, newdata = foo0, type = "response")    # under control

# Calculate un-centered influence functions
if1 <- a / pi * (y - Qa) + Q1              
if0 <- (1 - a) / (1 - pi) * (y - Qa) + Q0  
```

Create *ife* objects for these estimates using
`influence_func_estimate()` or `ife()`:

``` r
ife1 <- influence_func_estimate(mean(if1), if1)
ife0 <- ife(mean(if0), if0)
```

*ife* then allows you to estimate contrasts between estimates, with
variance estimated using automatic differentiation. The additive effect
(risk difference) can be calculated as:

``` r
ife1 - ife0
#>       Estimate: 0.257
#>     Std. error: 0.042
#> 95% Conf. int.: 0.174, 0.339
```

The multiplicative effect (risk ratio) can be estimated as:

``` r
ife1 / ife0
#>       Estimate: 1.547
#>     Std. error: 0.12
#> 95% Conf. int.: 1.312, 1.783
```

For the risk ratio, which is strictly positive, you can estimate the
effect on the log scale and exponentiate the confidence intervals to
ensure the lower bound is always positive:

``` r
exp(log(ife1 / ife0)@conf_int)
#> [1] 1.33 1.80
```
