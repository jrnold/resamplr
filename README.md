resamplr
========

[![Travis-CI Build Status](https://travis-ci.org/jrnold/resamplr.svg?branch=master)](https://travis-ci.org/jrnold/resamplr) [![codecov](https://codecov.io/gh/jrnold/resamplr/branch/master/graph/badge.svg)](https://codecov.io/gh/jrnold/resamplr)

The **resamplr** package provides functions that implement resampling methods including the bootstrap, jackknife, random test/train sets, k-fold cross-validation, leave-one-out and leave-p-out cross-validation, time-series cross validation, time-series k-fold cross validation, permutations, rolling windows. These functions generate data frames with `resample` objects that work with the modelling pipeline of [modelr](https://github.com/hadley/modelr) and the [tidyverse](http://tidyverse.org/).

Installation
------------

**resamplr** is not on CRAN. You can install the development version with

``` r
# install.packages("devtools")
devtools::install_github("jrnold/resamplr")
```

Main Features
-------------

The **resamplr** package includes functions to generate data frames of lazy resample objects, as introduced in the [tidyverse](http://tidyverse.org/) [modelr](https://github.com/hadley/modelr) package. The `resample` class stores the a "pointer" to the original dataset and a vector of row indices. The object can be coerced to a dataframe with `as.data.frame` and the row indices with `as.integer`.

``` r
library("modelr")
library("resamplr")
rs <- resample(mtcars, 1:10)
as.data.frame(rs)
#>                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
as.integer(rs)
#>  [1]  1  2  3  4  5  6  7  8  9 10
```

While the **modelr** package contains a few functions with resampling methods (`crossv_kfold`, `crossv_mc`, and `bootstrap`), the **resamplr** package implements many more resampling methods including the following:

-   boostrap

    -   bootstrap (weighted, Bayesian): `bootstrap`
    -   balanced bootstrap: `balanced_bootstrap`
    -   time-series bootstrap: `tsbootstrap`

-   cross-validation

    -   test-training pairs: `holdout_n`, `holdout_frac`
    -   k-fold cross-validation: `crossv_kfold`
    -   time-series cross-validation: `crossv_ts`
    -   leave-one-out and leave-p-out cross-validation: `crossv_lpo`, `crossv_loo`
    -   time-series k-fold cross-validation: `crossv_tskfold`

-   jackknife: `jackknife`
-   permutations: `permute`
-   rolling windows: `roll`

All resampling functions are implemented as generic functions with methods for data frames (`data.frame`) and grouped data frames (`grouped_df`). When used with grouped data frames, these functions allow either resampling groups instead of rows, or resample rows within each group (stratification), or both, depending on what is appropriate for the method.
