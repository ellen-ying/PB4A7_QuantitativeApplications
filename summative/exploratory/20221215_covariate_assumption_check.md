20221215_covariate_assumption_check
================
Yurun (Ellen) Ying
2022-12-15

We will check the assumptions that observed covariates didn’t have a
jump at the cutoff point.

``` r
# analysis officially given in the do file
# bac1 changed to centered value of bac1
# get the data within the bandwidth
dat_bandwidth_0.05 <- bac_data %>% filter(abs(bac1_ctd) <= 0.05)

# check being white
white_rdd <- dat_bandwidth_0.05 %>% lm(white ~ dui*bac1_ctd, data = .)

# use a customary function to calculate robust SEs
summaryR.lm(white_rdd, type = "hc1") 
```

    ## Loading required package: car

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

    ## 
    ## Call:
    ## lm(formula = white ~ dui * bac1_ctd, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8567  0.1439  0.1455  0.1473  0.1576 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.846275   0.004090 206.926   <2e-16 ***
    ## dui          0.005704   0.005008   1.139    0.255    
    ## bac1_ctd     0.078754   0.214216   0.368    0.713    
    ## dui:bac1_ctd 0.015618   0.233955   0.067    0.947    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3545 on 89963 degrees of freedom
    ## Multiple R-squared:  0.0001299,  Adjusted R-squared:  9.66e-05 
    ## F-statistic: 3.799 on 3 and 89963 DF,  p-value: 0.009762
    ## 
    ## Note: Heteroscedasticity-consistent standard errors using adjustment hc1

``` r
# check being male
male_rdd <- dat_bandwidth_0.05 %>% lm(male ~ dui*bac1_ctd, data = .)
summaryR.lm(male_rdd, type = "hc1") 
```

    ## 
    ## Call:
    ## lm(formula = male ~ dui * bac1_ctd, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.7953  0.2049  0.2067  0.2085  0.2158 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.784236   0.004630 169.391   <2e-16 ***
    ## dui           0.006184   0.005704   1.084    0.278    
    ## bac1_ctd     -0.209970   0.239779  -0.876    0.381    
    ## dui:bac1_ctd  0.307072   0.263239   1.167    0.243    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.406 on 89963 degrees of freedom
    ## Multiple R-squared:  5.066e-05,  Adjusted R-squared:  1.732e-05 
    ## F-statistic: 1.501 on 3 and 89963 DF,  p-value: 0.212
    ## 
    ## Note: Heteroscedasticity-consistent standard errors using adjustment hc1

``` r
# check accident
accident_rdd <- dat_bandwidth_0.05 %>% lm(acc ~ dui*bac1_ctd, data = .)
summaryR.lm(accident_rdd, type = "hc1") 
```

    ## 
    ## Call:
    ## lm(formula = acc ~ dui * bac1_ctd, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13713 -0.10940 -0.09831 -0.08801  0.91913 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.083431   0.003297  25.308  < 2e-16 ***
    ## dui          -0.003350   0.004062  -0.825    0.409    
    ## bac1_ctd     -1.095895   0.186328  -5.882 4.08e-09 ***
    ## dui:bac1_ctd  1.888365   0.203057   9.300  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3014 on 89963 degrees of freedom
    ## Multiple R-squared:  0.001505,   Adjusted R-squared:  0.001472 
    ## F-statistic: 44.02 on 3 and 89963 DF,  p-value: < 2.2e-16
    ## 
    ## Note: Heteroscedasticity-consistent standard errors using adjustment hc1

``` r
# check age
age_rdd <- dat_bandwidth_0.05 %>% lm(aged ~ dui*bac1_ctd, data = .)
summaryR.lm(age_rdd, type = "hc1") 
```

    ## 
    ## Call:
    ## lm(formula = aged ~ dui * bac1_ctd, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.309  -9.896  -2.972   7.665  46.207 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   33.9199     0.1346 251.947   <2e-16 ***
    ## dui           -0.1405     0.1644  -0.855    0.393    
    ## bac1_ctd     -69.1637     7.2203  -9.579   <2e-16 ***
    ## dui:bac1_ctd  76.0493     7.8440   9.695   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.57 on 89963 degrees of freedom
    ## Multiple R-squared:  0.002346,   Adjusted R-squared:  0.002312 
    ## F-statistic: 62.95 on 3 and 89963 DF,  p-value: < 2.2e-16
    ## 
    ## Note: Heteroscedasticity-consistent standard errors using adjustment hc1

There is no evidence that the three demographic characteristics or
accident rate were different between people who were just below and
those just above the cutoff point.

## Notes for future improvement

- Compute confidence intervals
- Incorporate logistic regression to estimate white, male, and accident
