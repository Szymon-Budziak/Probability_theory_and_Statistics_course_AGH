Exercises 9 tasks
================

Plik `gamma.dat` zawiera dane pochodzące z rozkładu gamma. Znajdź
estymaty parametrów tego rozkładu korzystając:

-   metody momentów,
-   metody największej wiarygodności \[można skorzystać albo z funkcji
    `optim()` albo z funkcji `fitdistr()` z pakietu MASS.\].

**Konkretnie należy napisać skrypt, który:**

-   wczyta dane z podanego pliku i umieści je w wektorze numerycznym,
-   znajdzie estymatę parametrów rozkładu gamma metodą momentów (patrz
    wykład),
-   znajdzie estymatę parametrów rozkładu gamma metodą największej
    wiarygodności przy pomocy standardowej funkcji `optim()`,
-   porówna otrzymane estymaty.

``` r
data <- scan("gamma.dat")
data
```

    ##   [1]  3.502322  2.500280  3.678433  4.053910  2.698610  7.542481  4.429116
    ##   [8]  2.985344  2.578780  1.411543  4.737875  1.626216  3.321868  1.677070
    ##  [15]  2.413140  3.525853  2.735439  1.126352  4.876427  2.355204  3.800414
    ##  [22]  3.510784  5.990068  9.235953  3.126464  2.140103  2.924816  2.718761
    ##  [29]  2.196384  3.415909  7.333773  4.600217  5.783648  5.690908  4.324631
    ##  [36]  4.958897  2.740347  1.437904  1.816612  3.197590  4.897638  2.150594
    ##  [43]  3.102225  3.384592  2.667655  7.706870  2.181126  3.888901  3.580631
    ##  [50]  2.032009  6.268151  4.991951  4.798039  1.767390 11.423160  2.360697
    ##  [57]  6.036450  5.504894  4.862732  4.755630  5.631037  3.287973  1.457715
    ##  [64]  2.521209  2.104051  1.615685  2.629255  3.124216  2.077397  2.343817
    ##  [71]  4.036211  2.436721  2.217144  4.377847  1.144496  5.588296  3.714614
    ##  [78]  4.886407  8.032639  3.829219  2.982871  4.406316  1.230259  2.804169
    ##  [85]  4.251439  3.823562  1.757443  2.984926  2.302527  4.665190  4.115175
    ##  [92]  1.933074  4.099816  6.606709  4.468476  5.520856  9.339452  5.781904
    ##  [99]  4.187966  3.030179

## Method of moments

``` r
data_mean <- sum(data)/length(data)
data_mean
```

    ## [1] 3.8043

``` r
data_variance <- var(data)
data_variance
```

    ## [1] 3.644839

``` r
first_moment <- data_mean^2/data_variance
first_moment
```

    ## [1] 3.970738

``` r
second_moment <- data_variance/data_mean
second_moment
```

    ## [1] 0.958084

## Maximum likelihood

``` r
gamma_log_likelihood <- function(data, par){
  a <- par[1]
  b <- par[2]
  n <- length(data)
  sum_log <- sum(log(data))
  gll <- n*a*log(b) + n*lgamma(a) + sum(data)/b - (a-1)*sum_log
  return(gll)
}
```

``` r
maximum_likelihood <- optim(par=c(first_moment, 1/second_moment), data=data, fn=gamma_log_likelihood)

maximum_likelihood
```

    ## $par
    ## [1] 4.4699969 0.8510284
    ## 
    ## $value
    ## [1] 192.7472
    ## 
    ## $counts
    ## function gradient 
    ##       49       NA 
    ## 
    ## $convergence
    ## [1] 0
    ## 
    ## $message
    ## NULL

``` r
likely_first_moment <- maximum_likelihood$par[1]
likely_first_moment
```

    ## [1] 4.469997

``` r
likely_second_moment <- maximum_likelihood$par[2]
likely_second_moment
```

    ## [1] 0.8510284

## Results comparison

``` r
first_moment_comparison <- first_moment/likely_first_moment
first_moment_comparison
```

    ## [1] 0.8883088

``` r
second_moment_comparison <- second_moment/likely_second_moment
second_moment_comparison
```

    ## [1] 1.125796
