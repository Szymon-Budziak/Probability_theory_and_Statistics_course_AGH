Exercises 3 tasks
================

### Zaimplementuj obsługę skończonej przestrzeni probabilistycznej Ω = {1, …, N }. Założenia są następujące:

-   Przestrzeń jest reprezentowana przez wektor długości N o składowych
    nieujemnych, których suma wynosi 1; składowa numer ‘i’ tego wektora
    oznacza prawdopodobieństwo zdarzenia {i}.
-   Należy skonstruować funkcję `prob_space(x)`, która tworzy takie
    wektory na podstawie dowolnych wektorów numerycznych `x` o
    składowych dodatnich. W wektorze wynikowym współrzędne mają być
    zmniejszone proporcjonalnie tak, by ich suma była równa 1, np.
    przestrzeń prawdopodobieństwa klasycznego dla `N = 6` powinna
    powstać po wywołaniu `ps6 <- prob_space(rep(1, 6))`.
-   Zdarzenia reprezentowane są przez wektory logiczne o długości N tak,
    że np. zdarzenie `{1, 3}` dla `N = 6` jest reprezentowane przez
    wektor `c(TRUE, FALSE, TRUE, FALSE,FALSE, FALSE)`.
-   Należy skonstruować funkcję `event(x, prob_space)`, która będzie
    tworzyć takie wektory na podstawie odpowiednich wektorów
    numerycznych, np. wektor z poprzedniego punktu powstałby po
    wywołaniu `event(c(1, 3), ps6)`; “nadmiarowe” zdarzenia elementarne
    mają być ignorowane, czyli np. `event(7, ps6)` powinno zwracać
    zdarzenie niemożliwe (czyli to samo co `rep(FALSE, 6)`).
-   Należy skonstruować funkcje `union(e1, e2)`, `intersect(e1, e2)`,
    `complement(e)` wyznaczające, odpowiednio, sumę, część wspólną i
    dopełnienie podanych zdarzeń.
-   Należy zaimplementować funkcję `prob(event, prob_space)` liczącą
    prawdopodobieństwo zdarzenia `event` w przestrzeni reprezentowanej
    przez wektor `prob_space`.

``` r
prob_space <- function(x){
  vectors <- c(x/ sum(x))
  return(vectors)
}

ps6 <- prob_space(rep(1, 6))
print(ps6)
```

    ## [1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667

``` r
event <- function(x, prob_space){
  vectors <- rep(FALSE, length(prob_space))
  for (v in x){
    if (v > 0 && v < length(prob_space)){
      vectors[v] = TRUE
    }
  }
  return(vectors)
}

event_1_3 <- event(c(1, 3), ps6)
evenet_7 <- event(7, ps6)
print(event_1_3)
```

    ## [1]  TRUE FALSE  TRUE FALSE FALSE FALSE

``` r
print(evenet_7)
```

    ## [1] FALSE FALSE FALSE FALSE FALSE FALSE

``` r
union <- function(e1, e2){
  result <- c()
  if(length(e1) != length(e2)){
    return(result)
  }
  range <- max(e1, e2)
  result <- rep(FALSE, range)
  for(i in e1){
    if(result[i] == FALSE){
      result[i] = TRUE
    }
  }
  for(i in e2){
    if(result[i] == FALSE){
      result[i] = TRUE
    }
  }
  return(result)
}

e1 <- c(1, 3, 5)
e2 <- c(2, 3, 5)
union(e1, e2)
```

    ## [1]  TRUE  TRUE  TRUE FALSE  TRUE

``` r
intersect <- function(e1, e2){
  result <- c()
  if(length(e1) != length(e2)){
    return(result)
  }
  range <- max(e1, e2)
  result <- rep(FALSE, range)
  for(i in e1){
    for(j in e2){
      if(i == j){
        result[i] = TRUE
      }
    }
  }
  return(result)
}

e1 <- c(1, 3, 5)
e2 <- c(2, 3, 5)
intersect(e1, e2)
```

    ## [1] FALSE FALSE  TRUE FALSE  TRUE

``` r
complement <- function(e){
  range <- max(e)
  result <- rep(TRUE, range)
  for(i in e){
    if(result[i] == TRUE){
      result[i] = FALSE
    }
  }
  return(result)
}

e <- c(1, 4, 5)
complement(e)
```

    ## [1] FALSE  TRUE  TRUE FALSE FALSE

``` r
prob <- function(event, prob_space){
  probability <- 0
  for(i in 1:length(prob_space)){
    if(event[i] == TRUE){
      probability <- probability + prob_space[i]
    }
  }
  return(probability)
}
p_s <- prob_space(rep(1, 7))
new_event <- event(c(2:5), p_s)
print(p_s)
```

    ## [1] 0.1428571 0.1428571 0.1428571 0.1428571 0.1428571 0.1428571 0.1428571

``` r
print(new_event)
```

    ## [1] FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE

``` r
prob(new_event, p_s)
```

    ## [1] 0.5714286
