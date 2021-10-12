
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EndoTools

<!-- badges: start -->

[![R-CMD-check](https://github.com/TalhoukLab/EndoTools/workflows/R-CMD-check/badge.svg)](https://github.com/TalhoukLab/EndoTools/actions)
[![Codecov test
coverage](https://codecov.io/gh/TalhoukLab/EndoTools/branch/master/graph/badge.svg)](https://codecov.io/gh/TalhoukLab/EndoTools?branch=master)
<!-- badges: end -->

The goal of EndoTools is to provide helper tools for calculating
commonly used variables in endometrial cancer projects such as:

-   ProMisE
-   ESMO

There are different versions for each molecular variable.

## Installation

You can install EndoTools from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("TalhoukLab/EndoTools")
```

## Example

This is a basic example which shows you how to assign and compare
different ESMO risk groups:

``` r
library(EndoTools)
library(dplyr)

df <- emdb %>%
  mutate(
    esmo2013 = assign_esmo2013(stage_full, grade_rev, hist_rev_gr),
    esmo2016 = assign_esmo2016(stage_full, grade_rev, hist_rev_gr, myo, lvi),
    esmo2020 = assign_esmo2020(stage_full, grade_rev, hist_rev_gr, myo, lvi)
  )

df %>% 
  count(esmo2013)
#> # A tibble: 3 × 2
#>   esmo2013         n
#>   <chr>        <int>
#> 1 high           445
#> 2 intermediate   187
#> 3 low            168

df %>% 
  count(esmo2016)
#> # A tibble: 6 × 2
#>   esmo2016              n
#>   <chr>             <int>
#> 1 advanced             12
#> 2 high                464
#> 3 high-intermediate   174
#> 4 intermediate         56
#> 5 low                  88
#> 6 metastatic            6

df %>% 
  count(esmo2020)
#> # A tibble: 6 × 2
#>   esmo2020              n
#>   <chr>             <int>
#> 1 high                345
#> 2 high-intermediate   181
#> 3 intermediate        135
#> 4 low                 110
#> 5 metastatic            6
#> 6 <NA>                 23

df %>% 
  count(esmo2013, esmo2016, esmo2020)
#> # A tibble: 22 × 4
#>    esmo2013     esmo2016          esmo2020              n
#>    <chr>        <chr>             <chr>             <int>
#>  1 high         advanced          high                 12
#>  2 high         high              high                333
#>  3 high         high              high-intermediate    59
#>  4 high         high              intermediate         15
#>  5 high         high              <NA>                  5
#>  6 high         high-intermediate high-intermediate    14
#>  7 high         high-intermediate <NA>                  1
#>  8 high         metastatic        metastatic            6
#>  9 intermediate high              high-intermediate    15
#> 10 intermediate high              intermediate         35
#> # … with 12 more rows
```
