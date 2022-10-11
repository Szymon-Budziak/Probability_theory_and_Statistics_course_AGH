Exercises 4 tasks
================

Napisz funkcję w R obliczającą liczbę losowań z ostatniego zadania
matematycznego dla podanej wartości ilorazu \|S\|/\|D\| i podangeo δ.
Przy jej pomocy wyznacz liczbę losowań dla δ = 0.9 oraz:

1.  \|S\|/\|D\| = 0.001,
2.  \|S\|/\|D\| = 10^(-6).

``` r
library("SciViews")
```

``` r
number_of_draws <- function(quotient, delta){
  result <- as.integer(ln(1-delta)/ ln(1-quotient))
  return(result)
}
```

``` r
number_of_draws(0.001, 0.9)
```

    ## [1] 2301

``` r
number_of_draws(10**(-6), 0.9)
```

    ## [1] 2302583
