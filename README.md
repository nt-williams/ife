
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ife

<!-- badges: start -->
<!-- badges: end -->

S7 class (with Ops) for influence function based estimands

## Installation

You can install the development version of ife from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nt-williams/ife")
```

``` r
library(ife)

n <- 500
w <- runif(n)
a <- rbinom(n, 1, 0.5)
y <- rbinom(n, 1, plogis(-0.75 + a + w))

foo <- data.frame(w, a, y)
foo1 <- foo0 <- foo
foo1$a <- 1
foo0$a <- 0

pi <- 0.5
m <- glm(y ~ a + w, data = foo, family = binomial())

Qa <- predict(m, type = "response")
Q1 <- predict(m, newdata = foo1, type = "response")
Q0 <- predict(m, newdata = foo0, type = "response")

if1 <- a / pi * (y - Qa) + Q1
if0 <- (1 - a) / pi * (y - Qa) + Q0

ife1 <- influence_func_estimate(mean(if1), if1)
ife0 <- ife(mean(if0), if0)

ife1 - ife0
```

    #> Estimate: 0.15

    #> Std. error: 0.04
    #> 95% Conf. int.: 0.07, 0.24
    ife1 / ife0

    #> Estimate: 1.32

    #> Std. error: 0.11
    #> 95% Conf. int.: 1.11, 1.52
