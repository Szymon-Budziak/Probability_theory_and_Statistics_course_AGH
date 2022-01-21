Exercises 8 tasks
================

1.  Napisz skrypt w R implementujący rozkład dyskretny dwuwymiarowy.
    Założenia są następujące:

-   zbiór wartości prawdopodobnych S ma postać {1, …, m} × {1, …, n} dla
    pewnych m, n ∈ N,
-   rozkład (pełny) jest zrealizowany w postaci macierzy prób o wyrazach
    nieujemnych sumujących się do 1, tzn. jeśli X i Y są składowymi
    zmiennej o implementowanym rozkładzie, to:
    **`prob[i, j] = P (X = i, Y = j)`**,
-   macierze z poprzedniego punktu są tworzone przez funkcję `prob(m)`,
    której argumentem jest macierz o wyrazach nieujemnych niekoniecznie
    sumujących się do 1 - funkcja ma dokonywać stosownej normalizacji
    (tzn. dzielić wszystkie wyrazy przez ich sumę),
-   funkcja `boundary(prob, i)` zwraca wektor rozkładu brzegowego
    względem zmiennej numer `i` (tzn. `boundary(prob, 1)` zwraca wektor
    rozkładu brzegowego względem X),
-   funkcja `conditional(prob, i, v)` zwraca wektor rozkładu warunkowego
    względem wartości `v` zmiennej numer `i`, np.
    **`conditional(prob, 1, 3)[2] = P (Y = 2|X = 3)`**,
-   funkcja `mean(prob, i)` zwraca wartość oczekiwaną składowej numer
    `i`, jeśli `i` ma wartość `NA` (która ma być domyślna) powinien
    zostać zwrócony wektor wartości oczekiwanej,
-   funkcja `covariance(prob)` zwraca macierz kowariancji,
-   funkcja `independent(prob)` zwraca `TRUE` jeśli składowe są
    niezależne, `FALSE` w przeciwnym przypadku.

**Create a matrix:**

``` r
m = matrix(c(7, 1, 9, 12, 4, 3, 3, 8, 2), nrow = 3, ncol = 3)
m
```

    ##      [,1] [,2] [,3]
    ## [1,]    7   12    3
    ## [2,]    1    4    8
    ## [3,]    9    3    2

``` r
m_ex = matrix(c(1, 1, 2, 2), nrow=2, ncol=2)
m_ex
```

    ##      [,1] [,2]
    ## [1,]    1    2
    ## [2,]    1    2

**Prob function:**

``` r
prob <- function(m){
  return(m/sum(m))
}
```

``` r
new_m <- prob(m)
new_m
```

    ##            [,1]       [,2]       [,3]
    ## [1,] 0.14285714 0.24489796 0.06122449
    ## [2,] 0.02040816 0.08163265 0.16326531
    ## [3,] 0.18367347 0.06122449 0.04081633

``` r
new_m_ex <- prob(m_ex)
new_m_ex
```

    ##           [,1]      [,2]
    ## [1,] 0.1666667 0.3333333
    ## [2,] 0.1666667 0.3333333

**Boundary function:**

``` r
boundary <- function(prob, i){
  vec <- vector()
  if (i == 1){
    vec <- rowSums(prob)
  }
  else if (i == 2){
    vec <- colSums(prob)
  }
  return(vec)
}
```

``` r
boundary(new_m, 1)
```

    ## [1] 0.4489796 0.2653061 0.2857143

``` r
boundary(new_m, 2)
```

    ## [1] 0.3469388 0.3877551 0.2653061

``` r
boundary(new_m_ex, 1)
```

    ## [1] 0.5 0.5

``` r
boundary(new_m_ex, 2)
```

    ## [1] 0.3333333 0.6666667

**Conditional function:**

``` r
conditional <- function(prob, i, v){
  vec <- vector()
  if(i == 1){
    vec <- prob[v,]/boundary(prob, i)[v]
  }
  else if(i == 2){
    vec <- prob[,v]/boundary(prob, i)[v]
  }
  return(vec)
}
```

``` r
conditional(new_m, 1, 2)
```

    ## [1] 0.07692308 0.30769231 0.61538462

``` r
conditional(new_m, 2, 2)
```

    ## [1] 0.6315789 0.2105263 0.1578947

``` r
conditional(new_m_ex, 1, 2)
```

    ## [1] 0.3333333 0.6666667

``` r
conditional(new_m_ex, 2, 2)
```

    ## [1] 0.5 0.5

**Mean function:**

``` r
mean <- function(prob, i=NA){
  vec_x <- c(0)
  for (j in 1:nrow(prob)) {
    vec_x = vec_x +  j * sum(prob[j,])
  }
  
  vec_y <- c(0)
  for (j in 1:ncol(prob)){
    vec_y = vec_y + j * sum(prob[,j])
  }
  
  vec <- c(vec_x, vec_y)
  if(!is.na(i) && (i == 1 || i == 2)){
    return(vec[i])
  }
  return(vec)
}
```

``` r
mean(new_m, NA)
```

    ## [1] 1.836735 1.918367

``` r
mean(new_m, 1)
```

    ## [1] 1.836735

``` r
mean(new_m, 2)
```

    ## [1] 1.918367

``` r
mean(new_m_ex, NA)
```

    ## [1] 1.500000 1.666667

``` r
mean(new_m_ex, 1)
```

    ## [1] 1.5

``` r
mean(new_m_ex, 2)
```

    ## [1] 1.666667

**Covariance function:**

``` r
covariance <- function(prob){
  return(cov(prob))
}
```

``` r
covariance(new_m)
```

    ##              [,1]         [,2]         [,3]
    ## [1,]  0.007219214  0.001527141 -0.005553242
    ## [2,]  0.001527141  0.010134666 -0.001735388
    ## [3,] -0.005553242 -0.001735388  0.004303762

``` r
covariance(new_m_ex)
```

    ##      [,1] [,2]
    ## [1,]    0    0
    ## [2,]    0    0

**Independent function:**

``` r
independent <- function(prob){
  prob_sum <- c(0)
  for(i in 1:nrow(prob)){
    for(j in 1:ncol(prob)){
      prob_sum = prob_sum + i * j * prob[i,j]
    }
  }
  
  x_y_result <- mean(prob, 1) + mean(prob, 2)
  
  print(c(prob_sum, x_y_result))
  if(prob_sum == x_y_result){
    return(TRUE)
  }
  return(FALSE)
}
```

``` r
independent(new_m)
```

    ## [1] 3.448980 3.755102

    ## [1] FALSE

``` r
independent(new_m_ex)
```

    ## [1] 2.500000 3.166667

    ## [1] FALSE
