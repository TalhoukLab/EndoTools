
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

This is a basic example which shows you to assign different ESMO risk
groups:

``` r
library(EndoTools)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(knitr)
data("emdb")
df <- emdb %>%
  mutate(
    esmo2013 = assign_esmo2013(stage_full, grade_rev, hist_rev_gr),
    esmo2016 = assign_esmo2016(stage_full, grade_rev, hist_rev_gr, myo, lvi),
    esmo2020 = assign_esmo2020(stage_full, grade_rev, hist_rev_gr, hist, myo, lvi)
  )

df %>% 
  count(esmo2013, esmo2016, esmo2020)
#> # A tibble: 24 x 4
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
#> # ... with 14 more rows
```

``` r
df %>% 
  count(esmo2013) %>% 
  kable(caption = "ESMO 2013 risk gruops")
```

| esmo2013     |   n |
|:-------------|----:|
| high         | 445 |
| intermediate | 187 |
| low          | 168 |

ESMO 2013 risk gruops

``` r
df %>% 
  count(esmo2016) %>% 
  kable(caption = "ESMO 2016 risk gruops")
```

| esmo2016          |   n |
|:------------------|----:|
| advanced          |  12 |
| high              | 464 |
| high-intermediate | 174 |
| intermediate      |  56 |
| low               |  88 |
| metastatic        |   6 |

ESMO 2016 risk gruops

``` r
df %>% 
  count(esmo2020) %>% 
  kable(caption = "ESMO 2020 risk gruops")
```

| esmo2020          |   n |
|:------------------|----:|
| advanced          |  45 |
| high              | 156 |
| high-intermediate | 181 |
| intermediate      | 124 |
| low               | 110 |
| metastatic        |   6 |
| NA                | 178 |

ESMO 2020 risk gruops
