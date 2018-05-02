
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
#> $data
#> # A tibble: 241 x 13
#>   .rownames pp_ideology   age   sex   edu hhinc .fitted .se.fit .resid
#>   <chr>           <int> <int> <int> <int> <int>   <dbl>   <dbl>  <dbl>
#> 1 1                   1    69     1     5     2    3.30   0.284  -2.30
#> 2 2                   5    76     1     3     1    3.70   0.253   1.30
#> # ... with 239 more rows, and 4 more variables: .hat <dbl>, .sigma <dbl>,
#> #   .cooksd <dbl>, .std.resid <dbl>
#> 
#> $fit
#> # A tibble: 7 x 6
#>   fit_statistic     n    df  estimate p.value stars
#>   <chr>         <int> <int>     <dbl>   <dbl> <chr>
#> 1 F               241     4    1.82     0.126 ""   
#> 2 R^2             241    NA    0.0299  NA     ""   
#> 3 Adj R^2         241    NA    0.0135  NA     ""   
#> 4 RMSE            241    NA    1.85    NA     ""   
#> 5 -2*LL           241     6  975.      NA     ""   
#> 6 AIC             241    NA  987.      NA     ""   
#> 7 BIC             241    NA 1008.      NA     ""   
#> 
#> $coef
#> # A tibble: 5 x 5
#>   term        estimate std.error statistic       p.value
#>   <chr>          <dbl>     <dbl>     <dbl>         <dbl>
#> 1 (Intercept)  5.51      0.878       6.28  0.00000000159
#> 2 age         -0.00877   0.00948    -0.924 0.356        
#> 3 sex         -0.446     0.240      -1.86  0.0643       
#> 4 edu         -0.241     0.127      -1.89  0.0600       
#> 5 hhinc        0.0183    0.0429      0.427 0.670
```

### Logistic regression

Conduct a logistic regression
analysis.

``` r
logistic_regression(polcom, follow_trump ~ pp_ideology + age + sex + edu + hhinc)
#> $data
#> # A tibble: 241 x 14
#>   .rownames follow_trump pp_ideology   age   sex   edu hhinc .fitted
#>   <chr>     <lgl>              <int> <int> <int> <int> <int>   <dbl>
#> 1 1         TRUE                   1    69     1     5     2   2.04 
#> 2 2         TRUE                   5    76     1     3     1   0.953
#> # ... with 239 more rows, and 6 more variables: .se.fit <dbl>,
#> #   .resid <dbl>, .hat <dbl>, .sigma <dbl>, .cooksd <dbl>,
#> #   .std.resid <dbl>
#> 
#> $fit
#> # A tibble: 8 x 6
#>   fit_statistic       n    df    estimate p.value stars
#>   <chr>           <int> <int>       <dbl>   <dbl> <chr>
#> 1 χ2.full           241   235  243.        0.343  ""   
#> 2 χ2.null           241   240  256.        0.223  ""   
#> 3 Δχ2               241     5   13.3       0.0211 *    
#> 4 Nagelkerke R^2    241    NA    0.0535   NA      ""   
#> 5 Cox & Snell R^2   241    NA   -0.000441 NA      ""   
#> 6 McFadden R^2      241    NA    0.0517   NA      ""   
#> 7 AIC               241    NA  255.       NA      ""   
#> 8 BIC               241    NA  276.       NA      ""   
#> 
#> $coef
#> # A tibble: 6 x 5
#>   term        estimate std.error statistic  p.value
#>   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)   4.26      1.34       3.19  0.00144 
#> 2 pp_ideology  -0.288     0.0868    -3.32  0.000904
#> 3 age          -0.0170    0.0133    -1.28  0.200   
#> 4 sex          -0.316     0.324     -0.975 0.330   
#> 5 edu          -0.0803    0.175     -0.460 0.646   
#> 6 hhinc        -0.0232    0.0565    -0.410 0.682
```

### ANOVA

Conduct an analysis of variance.

``` r
ANOVA(polcom, pp_ideology ~ as.factor(sex) + age + pie_1)
#> $fit
#> # A tibble: 7 x 6
#>   fit_statistic     n    df  estimate p.value stars
#>   <chr>         <int> <int>     <dbl>   <dbl> <chr>
#> 1 F               242     3    2.33    0.0752 +    
#> 2 R^2             242    NA    0.0285 NA      ""   
#> 3 Adj R^2         242    NA    0.0163 NA      ""   
#> 4 RMSE            242    NA    1.85   NA      ""   
#> 5 -2*LL           242     5  980.     NA      ""   
#> 6 AIC             242    NA  990.     NA      ""   
#> 7 BIC             242    NA 1008.     NA      ""   
#> 
#> $coef
#>             term  df     sumsq   meansq statistic   p.value stars
#> 1 as.factor(sex)   1   9.70595  9.70595  2.836923 0.0934314     +
#> 2            age   1   1.77035  1.77035  0.517451 0.4726369      
#> 3          pie_1   1  12.42092 12.42092  3.630473 0.0579359     +
#> 4      Residuals 238 814.26807  3.42129        NA        NA
```
