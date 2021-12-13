
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EndoTools

<!-- badges: start -->

[![R-CMD-check](https://github.com/TalhoukLab/EndoTools/workflows/R-CMD-check/badge.svg)](https://github.com/TalhoukLab/EndoTools/actions)
[![Codecov test
coverage](https://codecov.io/gh/TalhoukLab/EndoTools/branch/master/graph/badge.svg)](https://codecov.io/gh/TalhoukLab/EndoTools?branch=master)
<!-- badges: end -->

The goal of EndoTools is to provide helper tools for calculating
commonly used variables in endometrial cancer projects such as:

-   MMR
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
#> # A tibble: 4 × 2
#>   esmo2013         n
#>   <fct>        <int>
#> 1 low             89
#> 2 intermediate   117
#> 3 high           425
#> 4 <NA>           169

df %>% 
  count(esmo2016)
#> # A tibble: 7 × 2
#>   esmo2016              n
#>   <fct>             <int>
#> 1 low                  36
#> 2 intermediate         21
#> 3 high-intermediate    99
#> 4 high                421
#> 5 advanced             12
#> 6 metastatic            6
#> 7 <NA>                205

df %>% 
  count(esmo2020)
#> # A tibble: 7 × 2
#>   esmo2020              n
#>   <fct>             <int>
#> 1 low                  75
#> 2 intermediate         51
#> 3 high-intermediate    81
#> 4 high                223
#> 5 advanced             32
#> 6 metastatic            6
#> 7 <NA>                332

df %>% 
  count(esmo2013, esmo2016, esmo2020)
#> # A tibble: 54 × 4
#>    esmo2013 esmo2016          esmo2020              n
#>    <fct>    <fct>             <fct>             <int>
#>  1 low      low               low                  19
#>  2 low      low               intermediate          1
#>  3 low      low               <NA>                  6
#>  4 low      intermediate      low                  14
#>  5 low      intermediate      <NA>                  2
#>  6 low      high-intermediate low                   3
#>  7 low      high-intermediate intermediate          2
#>  8 low      high-intermediate high-intermediate    18
#>  9 low      high-intermediate high                  1
#> 10 low      high-intermediate <NA>                  6
#> # … with 44 more rows
```
