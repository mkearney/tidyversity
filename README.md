
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyacademic <img src="man/figures/logo.png" width="160px" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

🎓 Tidy tools for
academics

## \*\*\* This package is in very early development. Feedback is encouraged\!\!\! \*\*\*

## Installation

<!-- You can install the released version of tidyacademic from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidyacademic")
```
-->

Install the development version from
[Github](https://github.com/mkearney/tidyacademic) with:

``` r
## install devtools if not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install tidyacademic from Github
devtools::install_github("mkearney/tidyacademic")
```

## Data sets

Comes with one data set.

### `polcom`

Consists of survey responses to demographic, background, and likert-type
attitudinal items about political communication.

``` r
tibble::as_tibble(polcom)
#> # A tibble: 244 x 63
#>    follow_trump news_1 news_2 news_3 news_4 news_5 news_6 ambiv_sexism_1
#>  * <lgl>         <int>  <int>  <int>  <int>  <int>  <int>          <int>
#>  1 TRUE              8      1      1      1      1      6              3
#>  2 TRUE              1      1      1      1      1      1              5
#>  3 TRUE              8      1      1      1      8      1              5
#>  4 TRUE              8      1      1      1      1      6              2
#>  5 TRUE              6      1      2      1      1      3              4
#>  6 TRUE              8      1      1      1      6      8              1
#>  7 TRUE              8      1      1      1      1      1              4
#>  8 TRUE              5      1      1      3      2      1              1
#>  9 TRUE              8      1      1      1      1      1              1
#> 10 TRUE              5      1      6      7      5      6              3
#> # ... with 234 more rows, and 55 more variables: ambiv_sexism_2 <int>,
#> #   ambiv_sexism_3 <int>, ambiv_sexism_4 <int>, ambiv_sexism_5 <int>,
#> #   ambiv_sexism_6 <int>, img1_hrc_1 <int>, img1_hrc_2 <dbl>,
#> #   img1_hrc_3 <int>, img1_hrc_4 <dbl>, img1_hrc_5 <int>,
#> #   img1_hrc_6 <int>, img1_hrc_7 <int>, img1_hrc_8 <int>,
#> #   img1_hrc_9 <int>, img2_hrc_10 <int>, img2_hrc_11 <int>,
#> #   img2_hrc_12 <dbl>, img2_hrc_13 <int>, img2_hrc_14 <int>,
#> #   img2_hrc_15 <dbl>, img1_djt_1 <int>, img1_djt_2 <dbl>,
#> #   img1_djt_3 <int>, img1_djt_4 <dbl>, img1_djt_5 <int>,
#> #   img1_djt_6 <int>, img1_djt_7 <int>, img1_djt_8 <int>,
#> #   img1_djt_9 <int>, img2_djt_10 <int>, img2_djt_11 <int>,
#> #   img2_djt_12 <dbl>, img2_djt_13 <int>, img2_djt_14 <int>,
#> #   img2_djt_15 <dbl>, pie_1 <int>, pie_2 <int>, pie_3 <int>, pie_4 <int>,
#> #   vote_2016 <int>, vote_2016_choice <int>, pp_ideology <int>,
#> #   pp_party <int>, pp_party_lean <int>, therm_1 <int>, therm_2 <int>,
#> #   therm_3 <int>, therm_4 <int>, therm_5 <int>, age <int>, sex <int>,
#> #   gender <int>, race <int>, edu <int>, hhinc <int>
```

## Descriptive statistics

Return summary statistics in the form of a data frame.

``` r
## summary stats for social media use (numeric) variables
summarize_numeric(polcom_survey, smuse1:smuse3)

## summary stats for respondent sex and race (categorical) variables
summarize_categorical(polcom_survey, sex, race)
```

Estimate Cronbach’s alpha for a set of variables.

``` r
## reliability of social media use items
cronbachs_alpha(polcom, ambiv_sexism_1:ambiv_sexism_6)
#>                           items    alpha alpha.std
#> 1 ambiv_sexism_1:ambiv_sexism_6 0.904609  0.904600
#> 2               -ambiv_sexism_1 0.882322  0.882225
#> 3               -ambiv_sexism_2 0.884272  0.884121
#> 4               -ambiv_sexism_3 0.896061  0.896218
#> 5               -ambiv_sexism_4 0.897127  0.897411
#> 6               -ambiv_sexism_5 0.883554  0.883420
#> 7               -ambiv_sexism_6 0.881595  0.881855
```

## Modeling

### OLS regression

Conduct an Ordinary Least Squares (OLS) regression analysis.

``` r
ols_regression(polcom, pp_ideology ~ age + sex + edu + hhinc)
#> $fit
#>   statistic  estimate  p.value
#> 1 F(4, 236) 1.8185711 0.125953
#> 2       R^2 0.0299016       NA
#> 3   Adj R^2 0.0134592       NA
#> 
#> $coef
#>          term    estimate  std.error statistic     p.value stars
#> 1 (Intercept)  5.51358306 0.87758587  6.282671 1.58892e-09   ***
#> 2         age -0.00876768 0.00948465 -0.924407 3.56219e-01      
#> 3         sex -0.44631307 0.24012913 -1.858638 6.43235e-02     +
#> 4         edu -0.24076929 0.12740870 -1.889740 6.00184e-02     +
#> 5       hhinc  0.01828919 0.04285920  0.426727 6.69967e-01
```

### Logistic regression

Conduct a logistic regression
analysis.

``` r
logistic_regression(polcom, follow_trump ~ pp_ideology + age + sex + edu + hhinc)
#> $fit
#>   statistic estimate  p.value
#> 1     χ2(5)   13.261 0.978948
#> 2       AIC  255.166       NA
#> 
#> $coef
#>          term   estimate std.error statistic     p.value stars
#> 1 (Intercept)  4.2634703 1.3383725  3.185563 0.001444725    **
#> 2 pp_ideology -0.2879746 0.0867717 -3.318761 0.000904179   ***
#> 3         age -0.0169813 0.0132603 -1.280607 0.200331744      
#> 4         sex -0.3158309 0.3240300 -0.974697 0.329710732      
#> 5         edu -0.0803047 0.1747529 -0.459533 0.645851703      
#> 6       hhinc -0.0231718 0.0565281 -0.409917 0.681866718
```
