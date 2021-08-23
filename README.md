
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
    esmo2020 = assign_esmo2020(stage_full, grade_rev, hist_rev_gr, hist, myo, lvi)
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
#> # A tibble: 7 × 2
#>   esmo2020              n
#>   <chr>             <int>
#> 1 advanced             45
#> 2 high                156
#> 3 high-intermediate   181
#> 4 intermediate        124
#> 5 low                 110
#> 6 metastatic            6
#> 7 <NA>                178

df %>% 
  count(esmo2013, esmo2016, esmo2020)
#> # A tibble: 24 × 4
#>    esmo2013 esmo2016          esmo2020              n
#>    <chr>    <chr>             <chr>             <int>
#>  1 high     advanced          advanced              2
#>  2 high     advanced          high                 10
#>  3 high     high              advanced             43
#>  4 high     high              high                146
#>  5 high     high              high-intermediate    59
#>  6 high     high              intermediate          4
#>  7 high     high              <NA>                160
#>  8 high     high-intermediate high-intermediate    14
#>  9 high     high-intermediate <NA>                  1
#> 10 high     metastatic        metastatic            6
#> # … with 14 more rows
```
