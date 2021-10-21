
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
    eclass2 = assign_promise2019(mmr_ihc_2, pole_mut, p53),
    esmo2013 = assign_esmo2013(stage_full, grade_rev, hist_rev_gr),
    esmo2016 = assign_esmo2016(stage_full, grade_rev, hist_rev_gr, myo, lvi),
    esmo2020 = assign_esmo2020(stage_full, grade_rev, hist_rev_gr, myo, lvi, eclass2,
                               residual)
  )

df %>% 
  count(esmo2013)
#> # A tibble: 3 x 2
#>   esmo2013         n
#>   <fct>        <int>
#> 1 low            168
#> 2 intermediate   187
#> 3 high           445

df %>% 
  count(esmo2016)
#> # A tibble: 6 x 2
#>   esmo2016              n
#>   <fct>             <int>
#> 1 low                  88
#> 2 intermediate         56
#> 3 high-intermediate   174
#> 4 high                464
#> 5 advanced             12
#> 6 metastatic            6

df %>% 
  count(esmo2020)
#> # A tibble: 7 x 2
#>   esmo2020              n
#>   <fct>             <int>
#> 1 low                 132
#> 2 intermediate        105
#> 3 high-intermediate   133
#> 4 high                303
#> 5 advanced             20
#> 6 metastatic            6
#> 7 <NA>                101

df %>% 
  count(esmo2013, esmo2016, esmo2020)
#> # A tibble: 41 x 4
#>    esmo2013 esmo2016          esmo2020              n
#>    <fct>    <fct>             <fct>             <int>
#>  1 low      low               low                  53
#>  2 low      low               intermediate          2
#>  3 low      low               high                  6
#>  4 low      low               <NA>                  4
#>  5 low      intermediate      low                  39
#>  6 low      intermediate      high                  7
#>  7 low      intermediate      <NA>                  2
#>  8 low      high-intermediate low                   6
#>  9 low      high-intermediate intermediate          3
#> 10 low      high-intermediate high-intermediate    37
#> # ... with 31 more rows
```
