
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

Load the package (it, of course, plays nicely with tidyverse).

``` r
## load tidyverse
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
#> ✔ tibble  1.4.2     ✔ dplyr   0.7.4
#> ✔ tidyr   0.8.0     ✔ stringr 1.3.0
#> ✔ readr   1.1.1     ✔ forcats 0.3.0
#> ── Conflicts ────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

## load tidyversity
library(tidyversity)
```

## Regression models

### Ordinary Least Squares (OLS)

Conduct an Ordinary Least Squares (OLS) regression analysis.

``` r
polcom %>%
  tidy_regression(follow_trump ~ news_1 + ambiv_sexism_1) %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : follow_trump ~ news_1 + ambiv_sexism_1
#> Model type     : Ordinary Least Squares (OLS) regression
#> Model pkg::fun : stats::lm()
#> Model data     : 243 (observations) X 3 (variables)
#> $fit
#> fit_stat     n     df    estimate    p.value  stars
#> F          243      2      3.831      0.023   *
#> R^2        243      -      0.031       -         
#> Adj R^2    243      -      0.023       -         
#> RMSE       243      -      0.409       -         
#> AIC        243      -    260.148       -         
#> BIC        243      -    274.121       -         
#> 
#> $coef
#> term               est     s.e.    est.se    p.value  stars   std.est
#> (Intercept)      0.745    0.097     7.692      <.001   ***      <.001
#> news_1           0.022    0.012     1.811      0.071   +        0.048
#> ambiv_sexism_1  -0.038    0.021    -1.870      0.063   +       -0.050
```

### Logistic (dichotomous)

Conduct a logistic regression analysis for binary (dichotomous)
outcomes.

``` r
polcom %>%
  tidy_regression(follow_trump ~ news_1 + ambiv_sexism_1, type = "logistic") %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : follow_trump ~ news_1 + ambiv_sexism_1
#> Model type     : Logistic regression
#> Model pkg::fun : stats::glm()
#> Model data     : 243 (observations) X 3 (variables)
#> $fit
#> fit_stat           n     df    estimate    p.value  stars
#> χ2               243    240    247.442      0.357      
#> Δχ2              243      2      7.466      0.024   *
#> Nagelkerke R^2   243      -      0.030       -         
#> McFadden R^2     243      -      0.029       -         
#> RMSE             243      -      2.540       -         
#> AIC              243      -    253.442       -         
#> BIC              243      -    263.921       -         
#> 
#> $coef
#> term               est     s.e.    est.se    p.value  stars   std.est
#> (Intercept)      1.133    0.553     2.049      0.040   *        <.001
#> news_1           0.127    0.070     1.808      0.071   +        0.195
#> ambiv_sexism_1  -0.229    0.122    -1.872      0.061   +       -0.201
```

### Poisson (count)

Conduct a poisson regression analysis for count data.

``` r
polcom %>%
  mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "poisson") %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : polarize ~ news_1 + ambiv_sexism_1
#> Model type     : Poisson regression
#> Model pkg::fun : stats::glm()
#> Model data     : 242 (observations) X 3 (variables)
#> $fit
#> fit_stat           n     df    estimate    p.value  stars
#> χ2               242    239   6549.419      <.001   ***
#> Δχ2              242      2    399.077      <.001   ***
#> Nagelkerke R^2   242      -      0.808       -         
#> McFadden R^2     242      -      0.057       -         
#> RMSE             242      -      0.760       -         
#> AIC              242      -   7725.222       -         
#> BIC              242      -   7735.689       -         
#> 
#> $coef
#> term               est     s.e.     est.se    p.value  stars   std.est
#> (Intercept)      3.798    0.038     99.448      <.001   ***      <.001
#> news_1           0.045    0.005      9.358      <.001   ***      0.881
#> ambiv_sexism_1  -0.126    0.008    -15.852      <.001   ***     -2.230
```

### Negative binomial (overdispersed)

Conduct a negative binomial regression analysis for overdispersed count
data.

``` r
polcom %>%
  mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "negbinom") %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : polarize ~ news_1 + ambiv_sexism_1
#> Model type     : Negative binomial regression
#> Model pkg::fun : MASS::glm.nb()
#> Model data     : 242 (observations) X 3 (variables)
#> $fit
#> fit_stat           n     df    estimate    p.value  stars
#> χ2               242    239    293.328      0.009   **
#> Δχ2              242      2      8.440      0.015   *
#> Nagelkerke R^2   242      -      0.034       -         
#> McFadden R^2     242      -      0.028       -         
#> RMSE             242      -      0.761       -         
#> AIC              242      -   2312.391       -         
#> BIC              242      -   2326.347       -         
#> 
#> $coef
#> term               est     s.e.    est.se    p.value  stars   std.est
#> (Intercept)      3.741    0.258    14.510      <.001   ***      3.752
#> news_1           0.053    0.032     1.632      0.103            0.113
#> ambiv_sexism_1  -0.123    0.054    -2.273      0.023   *       -0.158
```

### Robust and quasi- models

``` r
polcom %>%
  mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, 
    type = "quasipoisson", robust = TRUE) %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : polarize ~ news_1 + ambiv_sexism_1
#> Model type     : [Robust] Poisson regression
#> Model pkg::fun : robust::glmRob()
#> Model data     : 242 (observations) X 3 (variables)
#> $fit
#> fit_stat           n     df     estimate    p.value  stars
#> χ2               242    239    6989.543      <.001   ***
#> Δχ2              242      2   58782.937      <.001   ***
#> Nagelkerke R^2   242      -       1.000       -         
#> McFadden R^2     242      -       0.894       -         
#> RMSE             242      -      31.865       -         
#> AIC              242      -    2245.147       -         
#> BIC              242      -    2259.103       -         
#> 
#> $coef
#> term               est     s.e.     est.se    p.value  stars   std.est
#> (Intercept)      3.705    0.071     51.968      <.001   ***      <.001
#> news_1           0.079    0.010      8.325      <.001   ***      1.279
#> ambiv_sexism_1  -0.241    0.022    -11.179      <.001   ***     -2.086
```

## Mean comparison models

### ANOVA

Conduct an analysis of variance (ANOVA).

``` r
polcom %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female"),
  vote_choice = case_when(
    vote_2016_choice == 1 ~ "Clinton",
    vote_2016_choice == 2 ~ "Trump",
    TRUE ~ "Other")) %>%
  tidy_anova(pp_party ~ sex * vote_choice) %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : pp_party ~ sex * vote_choice
#> Model type     : Analysis of variance (ANOVA)
#> Model pkg::fun : stats::aov()
#> Model data     : 243 (observations) X 3 (variables)
#> $fit
#> fit_stat     n     df    estimate    p.value  stars
#> F          243      5     53.327      <.001   ***
#> R^2        243      -      0.529       -         
#> Adj R^2    243      -      0.519       -         
#> RMSE       243      -      1.238       -         
#> AIC        243      -    801.115       -         
#> BIC        243      -    825.567       -         
#> 
#> $coef
#> term                 est       s.e.     est.se    statistic    p.value  stars   std.est
#> sex                1.000     19.238     19.238       12.561      <.001   ***      2.000
#> vote_choice        2.000    388.606    194.303      126.867      <.001   ***      2.000
#> sex:vote_choice    2.000      0.519      0.259        0.169      0.844            2.000
#> Residuals        237.000    362.978      1.532         -          -             237.000
```

### t-tests

``` r
polcom %>%
  tidy_ttest(pp_ideology ~ follow_trump) %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : pp_ideology ~ follow_trump
#> Model type     : T-test
#> Model pkg::fun : stats::t.test()
#> Model data     : 244 (observations)
#> $fit
#> group       df     mean      diff     lo.95     hi.05
#> FALSE  76.911    4.185     0.922     0.308     1.536
#> TRUE   76.911    3.263    -0.922    -0.308    -1.536
#> 
#> $coef
#>     est        t    p.value  stars
#>  0.922    2.992      0.004   **
```

## Latent variable models

### Structural equation modeling (SEM)

Conduct latent variable analysis using structural equation modeling.

``` r
## mutate data and then specify and estimate model
sem1 <- polcom %>%
  mutate(therm_2 = therm_2 / 10, 
    therm_1 = 10 - therm_1 / 10) %>%
  tidy_sem_model(news =~ news_1 + news_2 + news_3 + news_4 + news_5 + news_6,
    ambiv_sexism =~ ambiv_sexism_1 + ambiv_sexism_2 + ambiv_sexism_3 + 
      ambiv_sexism_4 + ambiv_sexism_5 + ambiv_sexism_6,
    partisan =~ a*therm_1 + a*therm_2,
    ambiv_sexism ~ age + sex + hhinc + edu + news + partisan) %>%
  tidy_sem()

## print model summary
sem1 %>%
  tidy_summary()
#> # A tidy model
#> Model formula  : news =~ news_1 + news_2 + news_3 + news_4 + news_5 + news_6
#>                  ambiv_sexism =~ ambiv_sexism_1 + ambiv_sexism_2 + ambiv_sexism_3 + ambiv_sexism_4 + 
#>                      ambiv_sexism_5 + ambiv_sexism_6
#>                  partisan =~ a * therm_1 + a * therm_2
#>                  ambiv_sexism ~ age + sex + hhinc + edu + news + partisan
#> Model type     : Structural Equation Model (SEM)
#> Model pkg::fun : lavaan::sem()
#> Model data     : 235 (observations) X 18 (variables)
#> $fit
#> fit_stat             n     df     estimate    p.value  stars
#> chisq              235    127     239.579      <.001   ***
#> aic                235      -       0.907       -         
#> bic                235      -       0.892       -         
#> cfi                235      -   16138.684       -         
#> tli                235      -   16256.310       -         
#> rmsea              235      -       0.061       -         
#> srmr               235      -       0.073       -         
#> R^2:ambiv_sexism   235      -       0.379       -         
#> 
#> $coef
#> term                               est       se    est.se    p.value  stars   std.est
#> news =~ news_1                   1.000    <.001      -          -               0.173
#> news =~ news_2                   1.592    0.722     2.204      0.028   *        0.340
#> news =~ news_3                   5.069    2.095     2.419      0.016   *        0.781
#> news =~ news_4                   5.587    2.312     2.417      0.016   *        0.851
#> news =~ news_5                   3.493    1.485     2.353      0.019   *        0.520
#> news =~ news_6                   1.255    0.683     1.838      0.066   +        0.196
#> ambiv_sexism =~ ambiv_sexism_1   1.000    <.001      -          -               0.825
#> ambiv_sexism =~ ambiv_sexism_2   0.942    0.067    14.043      <.001   ***      0.801
#> ambiv_sexism =~ ambiv_sexism_3   0.795    0.067    11.844      <.001   ***      0.706
#> ambiv_sexism =~ ambiv_sexism_4   0.743    0.064    11.647      <.001   ***      0.697
#> ambiv_sexism =~ ambiv_sexism_5   0.902    0.062    14.644      <.001   ***      0.825
#> ambiv_sexism =~ ambiv_sexism_6   0.904    0.064    14.185      <.001   ***      0.807
#> partisan =~ therm_1              1.000    <.001      -          -               0.577
#> partisan =~ therm_2              1.000    <.001      -          -               0.592
#> ambiv_sexism ~ age              -0.004    0.005    -0.824      0.410           -0.051
#> ambiv_sexism ~ sex              -0.271    0.130    -2.089      0.037   *       -0.130
#> ambiv_sexism ~ hhinc            -0.021    0.023    -0.878      0.380           -0.057
#> ambiv_sexism ~ edu              -0.088    0.069    -1.279      0.201           -0.083
#> ambiv_sexism ~ news              0.130    0.215     0.607      0.544            0.047
#> ambiv_sexism ~ partisan          0.347    0.069     5.032      <.001   ***      0.592
```

### Multilevel modeling (MLM)

Estimate multilevel (mixed effects) models.

``` r
lme4::sleepstudy %>%
  tidy_mlm(Reaction ~ Days + (Days | Subject)) %>%
  summary()
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: Reaction ~ Days + (Days | Subject)
#>    Data: .data
#> 
#> REML criterion at convergence: 1743.6
#> 
#> Scaled residuals: 
#>    Min     1Q Median     3Q    Max 
#> -3.954 -0.463  0.023  0.463  5.179 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev. Corr
#>  Subject  (Intercept) 612.1    24.74        
#>           Days         35.1     5.92    0.07
#>  Residual             654.9    25.59        
#> Number of obs: 180, groups:  Subject, 18
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)   251.41       6.82   36.84
#> Days           10.47       1.55    6.77
#> 
#> Correlation of Fixed Effects:
#>      (Intr)
#> Days -0.138
```

# Data sets

Comes with one data set.

### `polcom`

Consists of survey responses to demographic, background, and likert-type
attitudinal items about political communication.

``` r
print(tibble::as_tibble(polcom), n = 5)
#> # A tibble: 244 x 63
#>   follow_trump news_1 news_2 news_3 news_4 news_5 news_6 ambiv_sexism_1 ambiv_sexism_2
#> * <lgl>         <int>  <int>  <int>  <int>  <int>  <int>          <int>          <int>
#> 1 TRUE              8      1      1      1      1      6              3              3
#> 2 TRUE              1      1      1      1      1      1              5              5
#> 3 TRUE              8      1      1      1      8      1              5              4
#> 4 TRUE              8      1      1      1      1      6              2              2
#> 5 TRUE              6      1      2      1      1      3              4              4
#> # ... with 239 more rows, and 54 more variables: ambiv_sexism_3 <int>, ambiv_sexism_4 <int>,
#> #   ambiv_sexism_5 <int>, ambiv_sexism_6 <int>, img1_hrc_1 <int>, img1_hrc_2 <dbl>,
#> #   img1_hrc_3 <int>, img1_hrc_4 <dbl>, img1_hrc_5 <int>, img1_hrc_6 <int>, img1_hrc_7 <int>,
#> #   img1_hrc_8 <int>, img1_hrc_9 <int>, img2_hrc_10 <int>, img2_hrc_11 <int>, img2_hrc_12 <dbl>,
#> #   img2_hrc_13 <int>, img2_hrc_14 <int>, img2_hrc_15 <dbl>, img1_djt_1 <int>, img1_djt_2 <dbl>,
#> #   img1_djt_3 <int>, img1_djt_4 <dbl>, img1_djt_5 <int>, img1_djt_6 <int>, img1_djt_7 <int>,
#> #   img1_djt_8 <int>, img1_djt_9 <int>, img2_djt_10 <int>, img2_djt_11 <int>, img2_djt_12 <dbl>,
#> #   img2_djt_13 <int>, img2_djt_14 <int>, img2_djt_15 <dbl>, pie_1 <int>, pie_2 <int>, pie_3 <int>,
#> #   pie_4 <int>, vote_2016 <int>, vote_2016_choice <int>, pp_ideology <int>, pp_party <int>,
#> #   pp_party_lean <int>, therm_1 <int>, therm_2 <int>, therm_3 <int>, therm_4 <int>, therm_5 <int>,
#> #   age <int>, sex <int>, gender <int>, race <int>, edu <int>, hhinc <int>
```

## Descriptive statistics

Return summary statistics in the form of a data frame ***(not yet
added)***.

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
