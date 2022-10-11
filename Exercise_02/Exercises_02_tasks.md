Exercises 2 tasks
================

# Wprowadzenie do programowania w R — cz. 2

Semestr zimowy 2021/22

## Zadania

Zadania oparte są na zbiorze danych Auto. Należy napisać skrypt w R,
który znajduje następujące informacje.

``` r
auto <- read.csv("Auto.csv", header = TRUE, na.strings = "?")
head(auto)
```

    ##   mpg cylinders displacement horsepower weight acceleration year origin
    ## 1  18         8          307        130   3504         12.0   70      1
    ## 2  15         8          350        165   3693         11.5   70      1
    ## 3  18         8          318        150   3436         11.0   70      1
    ## 4  16         8          304        150   3433         12.0   70      1
    ## 5  17         8          302        140   3449         10.5   70      1
    ## 6  15         8          429        198   4341         10.0   70      1
    ##                        name
    ## 1 chevrolet chevelle malibu
    ## 2         buick skylark 320
    ## 3        plymouth satellite
    ## 4             amc rebel sst
    ## 5               ford torino
    ## 6          ford galaxie 500

#### 1. Jakie jest średnie zużycie paliwa (mpg) wszystkich samochodów?

``` r
mean(auto[, "mpg"])
```

    ## [1] 23.51587

#### 2. Jakie jest średnie zużycie paliwa samochodów, które mają 4 cylindry?

``` r
mean(auto$mpg[auto$cylinders == 4])
```

    ## [1] 29.31773

#### 3. Jaka jest mediana wagi (weight) wszystkich samochodów?

``` r
median(auto[,"weight"])
```

    ## [1] 2800

#### 4. Jakie jest średnie zużycie paliwa samochodów wyprodukowanych w roku 72?

``` r
mean(auto$mpg[auto$year == 72])
```

    ## [1] 18.71429

#### 5. Jaka jest wariancja przyspieszenia (acceleration) wszystkich samochodów?

``` r
var(auto[, "acceleration"])
```

    ## [1] 7.562474

#### 6. Jaka jest wariancja przyspieszenia samochodów japońskich (origin == 3)?

``` r
var(auto$acceleration[auto$origin == 3])
```

    ## [1] 3.821779

#### 7. Ile jest samochodów, których moc (horsepower) jest powyżej średniej?

``` r
sum(auto$horsepower > mean(auto$horsepower, na.rm=TRUE), na.rm=TRUE)
```

    ## [1] 148

#### 8. Jaka jest maksymalna moc samochodów, których waga jest poniżej średniej?

``` r
max(auto$horsepower[auto$weight < mean(auto$weight, na.rm = TRUE)], na.rm = TRUE)
```

    ## [1] 132

#### 9. Ile jest samochodów, których zużycie paliwa jest poniżej średniej (czyli mpg jest powyżej średniej)?

``` r
sum(auto$mpg > mean(auto$mpg))
```

    ## [1] 189

#### 10. Jaka jest minimalna liczba cylindrów samochodów, których zużycie paliwa jest poniżej średniej?

``` r
min(auto$cylinders[auto$mpg < mean(auto$mpg)])
```

    ## [1] 3

#### 11. Ile jest samochodów o maksymalnej pojemności silnika (displacement)?

``` r
sum(auto$displacement == max(auto$displacement))
```

    ## [1] 3

#### 12. Jakie jest maksymalna waga (weight) samochodów, których pojemność silnika jest mniejsza od jej mediany?

``` r
max(auto$weight[auto$displacement < median(auto$displacement)])
```

    ## [1] 3270
