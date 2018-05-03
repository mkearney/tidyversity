
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyversity <img src="man/figures/logo.png" width="160px" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

🎓 Tidy tools for
academics

## \*\*\* This package is in very early development. Feedback is encouraged\!\!\! \*\*\*

## Installation

<!-- You can install the released version of tidyversity from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidyversity")
```
-->

Install the development version from
[Github](https://github.com/mkearney/tidyversity) with:

``` r
## install devtools if not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install tidyversity from Github
devtools::install_github("mkearney/tidyversity")
```

## Regression

### Ordinary Least Squares (OLS)

Conduct an Ordinary Least Squares (OLS) regression analysis.

``` r
polcom %>%
  tidy_regression(pp_ideology ~ news_1 + ambiv_sexism_1 + pie_1) %>%
  tidy_summary()
#> $data
#> # A tibble: 243 x 12
#>   .rownames pp_ideology news_1 ambiv_sexism_1 pie_1 .fitted .se.fit .resid
#>   <chr>           <int>  <int>          <int> <int>   <dbl>   <dbl>  <dbl>
#> 1 1                   1      8              3     3    3.75   0.169 -2.75 
#> 2 2                   5      1              5     2    5.17   0.334 -0.175
#> # ... with 241 more rows, and 4 more variables: .hat <dbl>, .sigma <dbl>,
#> #   .cooksd <dbl>, .std.resid <dbl>
#> 
#> $fit
#> # A tibble: 7 x 6
#>   fit_statistic     n    df estimate p.value stars
#>   <chr>         <int> <int>    <dbl>   <dbl> <chr>
#> 1 F               243     3   17.2        0. ***  
#> 2 R^2             243    NA    0.178     NA  ""   
#> 3 Adj R^2         243    NA    0.168     NA  ""   
#> 4 RMSE            243    NA    1.70      NA  ""   
#> 5 -2*LL           243     5  945.        NA  ""   
#> 6 AIC             243    NA  955.        NA  ""   
#> 7 BIC             243    NA  972.        NA  ""   
#> 
#> $coef
#> # A tibble: 4 x 6
#>   term           estimate std.error statistic  p.value stars
#>   <chr>             <dbl>     <dbl>     <dbl>    <dbl> <chr>
#> 1 (Intercept)      2.40      0.475      5.04  9.18e- 7 ***  
#> 2 news_1          -0.0219    0.0524    -0.417 6.77e- 1 ""   
#> 3 ambiv_sexism_1   0.594     0.0859     6.92  4.23e-11 ***  
#> 4 pie_1           -0.0843    0.0960    -0.878 3.81e- 1 ""
```

### Logistic (binary data)

Conduct a logistic regression analysis for binary (dichotomous) data.

``` r
polcom %>%
  tidy_regression(follow_trump ~ news_1 + ambiv_sexism_1 + pie_1, 
    type = "logistic") %>%
  tidy_summary()
#> $data
#> # A tibble: 243 x 12
#>   .rownames follow_trump news_1 ambiv_sexism_1 pie_1 .fitted .se.fit
#>   <chr>     <lgl>         <int>          <int> <int>   <dbl>   <dbl>
#> 1 1         TRUE              8              3     3   1.61    0.260
#> 2 2         TRUE              1              5     2   0.237   0.453
#> # ... with 241 more rows, and 5 more variables: .resid <dbl>, .hat <dbl>,
#> #   .sigma <dbl>, .cooksd <dbl>, .std.resid <dbl>
#> 
#> $fit
#> # A tibble: 6 x 6
#>   fit_statistic      n    df estimate p.value stars
#>   <chr>          <int> <int>    <dbl>   <dbl> <chr>
#> 1 χ2               243   239 245.      0.383  ""   
#> 2 Δχ2              243     3   9.99    0.0186 *    
#> 3 Nagelkerke R^2   243    NA   0.0403 NA      ""   
#> 4 McFadden R^2     243    NA   0.0392 NA      ""   
#> 5 AIC              243    NA 253.     NA      ""   
#> 6 BIC              243    NA 267.     NA      ""   
#> 
#> $coef
#> # A tibble: 4 x 6
#>   term           estimate std.error statistic p.value stars
#>   <chr>             <dbl>     <dbl>     <dbl>   <dbl> <chr>
#> 1 (Intercept)       1.72     0.672       2.56  0.0106 *    
#> 2 news_1            0.160    0.0739      2.17  0.0300 *    
#> 3 ambiv_sexism_1   -0.238    0.122      -1.96  0.0498 *    
#> 4 pie_1            -0.224    0.143      -1.57  0.117  ""
```

## Poisson (count data)

Conduct a poisson regression analysis for count data.

``` r
polcom %>%
  tidy_regression(news_1 ~ pp_ideology + ambiv_sexism_1 + pie_1, 
    type = "poisson") %>%
  tidy_summary()
#> $data
#> # A tibble: 243 x 12
#>   .rownames news_1 pp_ideology ambiv_sexism_1 pie_1 .fitted .se.fit .resid
#>   <chr>      <int>       <int>          <int> <int>   <dbl>   <dbl>  <dbl>
#> 1 1              8           1              3     3    1.76  0.0525  0.861
#> 2 2              1           5              5     2    1.61  0.0650 -2.19 
#> # ... with 241 more rows, and 4 more variables: .hat <dbl>, .sigma <dbl>,
#> #   .cooksd <dbl>, .std.resid <dbl>
#> 
#> $fit
#> # A tibble: 6 x 6
#>   fit_statistic      n    df  estimate  p.value stars
#>   <chr>          <int> <int>     <dbl>    <dbl> <chr>
#> 1 χ2               243   239  209.      0.919   ""   
#> 2 Δχ2              243     3   15.3     0.00161 **   
#> 3 Nagelkerke R^2   243    NA    0.0608 NA       ""   
#> 4 McFadden R^2     243    NA    0.0680 NA       ""   
#> 5 AIC              243    NA 1082.     NA       ""   
#> 6 BIC              243    NA 1096.     NA       ""   
#> 
#> $coef
#> # A tibble: 4 x 6
#>   term           estimate std.error statistic  p.value stars
#>   <chr>             <dbl>     <dbl>     <dbl>    <dbl> <chr>
#> 1 (Intercept)     1.59       0.105     15.1   1.14e-51 ***  
#> 2 pp_ideology    -0.00584    0.0155    -0.377 7.06e- 1 ""   
#> 3 ambiv_sexism_1 -0.0224     0.0225    -0.996 3.19e- 1 ""   
#> 4 pie_1           0.0795     0.0228     3.49  4.82e- 4 ***
```

### ANOVA

Conduct an analysis of variance.

``` r
polcom %>%
  dplyr::mutate(sex = factor(sex, labels = c("Male", "Female")), 
    vote_2016_choice = factor(vote_2016_choice, labels = c("Clinton", "Trump", "Stein", "Johnson", "Other"))) %>%
  tidy_anova(pp_ideology ~ sex * vote_2016_choice)
#> $fit
#> # A tibble: 7 x 6
#>   fit_statistic     n    df estimate p.value stars
#>   <chr>         <int> <int>    <dbl>   <dbl> <chr>
#> 1 F               206     9   21.2        0. ***  
#> 2 R^2             206    NA    0.494     NA  ""   
#> 3 Adj R^2         206    NA    0.470     NA  ""   
#> 4 RMSE            206    NA    1.39      NA  ""   
#> 5 -2*LL           206    11  709.        NA  ""   
#> 6 AIC             206    NA  731.        NA  ""   
#> 7 BIC             206    NA  768.        NA  ""   
#> 
#> $coef
#>                   term  df    sumsq    meansq statistic     p.value stars
#> 1                  sex   1  10.4255 10.425468  5.423969 2.08813e-02     *
#> 2     vote_2016_choice   4 354.4163 88.604079 46.097281 2.91422e-27   ***
#> 3 sex:vote_2016_choice   4   2.3517  0.587925  0.305875 8.73816e-01      
#> 4            Residuals 196 376.7337  1.922111        NA          NA
```

## Data sets

Comes with one data set.

### `polcom`

Consists of survey responses to demographic, background, and likert-type
attitudinal items about political communication.

``` r
print(tibble::as_tibble(polcom), n = 5)
#> # A tibble: 244 x 63
#>   follow_trump news_1 news_2 news_3 news_4 news_5 news_6 ambiv_sexism_1
#> * <lgl>         <int>  <int>  <int>  <int>  <int>  <int>          <int>
#> 1 TRUE              8      1      1      1      1      6              3
#> 2 TRUE              1      1      1      1      1      1              5
#> 3 TRUE              8      1      1      1      8      1              5
#> 4 TRUE              8      1      1      1      1      6              2
#> 5 TRUE              6      1      2      1      1      3              4
#> # ... with 239 more rows, and 55 more variables: ambiv_sexism_2 <int>,
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
