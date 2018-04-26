
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyacademic <img src="man/figures/logo.png" width="160px" align="right" />
===========================================================================

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

🎓 Tidy tools for academics

Installation
------------

<!-- You can install the released version of tidyacademic from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidyacademic")
```
-->
Install the development version from [Github](https://github.com/mkearney/tidyacademic) with:

``` r
## install devtools if not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install tidyacademic from Github
devtools::install_github("mkearney/tidyacademic")
```

Data sets
---------

Comes with two data sets.

### polcom\_survey

Consists of survey responses to demographic, background, and likert-type attitudinal items about political communication.

``` r
polcom_survey
```

twitter\_data
-------------

Consists of tweet-level observations.

``` r
twitter_data
```

Descriptive statistics
----------------------

Return summary statistics in the form of a data frame.

``` r
## summary stats for social media use (numeric) variables
summarize_numeric(polcom_survey, smuse1:smuse3)

## summary stats for respondent sex and race (categorical) variables
summarize_categorical(polcom_survey, sex, race)
```

Estimate Cronbach's alpha for a set of variables.

``` r
## reliability of social media use items
cronbachs_alpha(polcom_survey, smuse1:smuse3)
```
