# rctsimple

[![Travis-CI Build Status](https://travis-ci.org/jimvine/rctsimple.svg?branch=master)](https://travis-ci.org/jimvine/rctsimple)

The goal of rctsimple is to facilitate simple analysis of outcome data from
Randomised Controlled Trials (RCTs). The package provides functions for 
analysing data from RCTs, for both aggregate and raw (participant-level) data.
This package focuses on calculating some simple statistics and presenting them
in a simple fashion.

The main statistics produced by the current version of the package are:

* **Prevalence difference**, an absolute measure suitable for binary / 
  dichotomous outcomes. `prevalence_difference()`
* **Prevalence ratio**, a relative measure suitable for binary / 
  dichotomous outcomes. `prevalence_ratio()`

`confint` methods are also provided for calculating confidence intervals around
the estimates of prevalence differences and prevalence ratios.

## Installation

You can install rctsimple from github with:

```R
# install.packages("devtools")
devtools::install_github("rctsimple/jimvine")
```

## Example

This is a basic example which shows you how to solve a common problem:

```R
# Set up some data for analysis
dummy_rct_data_list   <- list(group1_t = 48, group1_f = 52,
                              group2_t = 64, group2_f = 36)

# Calculate the prevalence difference
p_d <- prevalence_difference(outcome_data = dummy_rct_data_list,
                             groups = c("Control group", "Intervention group"),
                             outcomes = c("Outcome occurred", 
                                          "Outcome did not occur"))
p_d$estimate

# Calculate the prevalence ratio
p_r <- prevalence_ratio(outcome_data = dummy_rct_data_list,
                        groups = c("Control group", "Intervention group"),
                        outcomes = c("Outcome occurred", 
                                     "Outcome did not occur"))
p_r$estimate

# Find the confidence intervals of those estimates
confint(p_d)
confint(p_r)
```
