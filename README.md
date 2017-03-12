# resamplr

[![Travis-CI Build Status](https://travis-ci.org/jrnold/resamplr.svg?branch=master)](https://travis-ci.org/jrnold/resamplr)
[![codecov](https://codecov.io/gh/jrnold/resamplr/branch/master/graph/badge.svg)](https://codecov.io/gh/jrnold/resamplr)

**WORK IN PROGRESS**

**resamplr** adds additional resampling functions for the **modelr** lazy resamping object `resample`.
These functions are grouped data frame aware, with `data.frame` methods workin on rows, and `grouped_df` methods allowing for group-wise or stratified versions of the resampling/CV algorithms.

- Bootstrap

  - Ordinary bootstrap: `bootstrap.data.frame(data, ...)`
  - Bayesian bootstrap: `bootstrap(data, bayes = TRUE)`
  - Weighted bootstrap: `bootstrap(data, weights = "wt")`
  - Cluster bootstrap: `bootstrap.grouped_df(data, groups = TRUE)`
  - Stratified bootstrap: `bootstrap.grouped_df(data, groups = FALSE, stratify = TRUE)`
  - Balanced bootstrap: `balanced_bootstrap(data, ...)`
  
- Cross Validation

  - Test/train sets by number of observations: `holdout_n()`. Grouped data frame method support by group holdout, or stratified holdout.
  - Test/train sets by fraction of observations: `holdout_frac()` Grouped data frame support by group holdout, or stratified holdout.
  - k-Fold: `crossv_kfold()`: Grouped data frame method supports stratified k-fold or groupwise k-fold.
  - Leave-one-Out: `crossv_loo()`. Grouped data frame method leaves groups out.
  - Leave-p-Out: `crossv_lpo()`. Grouped data frame method leaves $p$ groups out.
  - Time-series k-fold: `crossv_tskfold`. Grouped data frame method supports groupwise k-folds.
  




## Installation

You can install resamplr from github with:

``` r
# install.packages("devtools")
devtools::install_github("jrnold/resamplr")
```
