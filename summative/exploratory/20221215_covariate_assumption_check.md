20221215_covariate_assumption_check
================
Yurun (Ellen) Ying
2022-12-15

We will check the assumptions that observed covariates didn’t have a
jump at the cutoff point.

``` r
dat_bandwidth_0.05 <- bac_data %>% filter(abs(bac1_ctd) <= 0.05)

# check being white
white_rdd <- dat_bandwidth_0.05 %>% lm(white ~ dui*bac1, data = .)

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
    ## lm(formula = white ~ dui * bac1, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8567  0.1439  0.1455  0.1473  0.1576 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.839975   0.014252  58.936   <2e-16 ***
    ## dui         0.004454   0.017515   0.254    0.799    
    ## bac1        0.078754   0.214216   0.368    0.713    
    ## dui:bac1    0.015618   0.233955   0.067    0.947    
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
male_rdd <- dat_bandwidth_0.05 %>% lm(male ~ dui*bac1, data = .)
summaryR.lm(male_rdd, type = "hc1") 
```

    ## 
    ## Call:
    ## lm(formula = male ~ dui * bac1, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.7953  0.2049  0.2067  0.2085  0.2158 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.80103    0.01592  50.324   <2e-16 ***
    ## dui         -0.01838    0.01979  -0.929    0.353    
    ## bac1        -0.20997    0.23978  -0.876    0.381    
    ## dui:bac1     0.30707    0.26324   1.167    0.243    
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
accident_rdd <- dat_bandwidth_0.05 %>% lm(acc ~ dui*bac1, data = .)
summaryR.lm(accident_rdd, type = "hc1") 
```

    ## 
    ## Call:
    ## lm(formula = acc ~ dui * bac1, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13713 -0.10940 -0.09831 -0.08801  0.91913 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.17110    0.01260  13.575  < 2e-16 ***
    ## dui         -0.15442    0.01527 -10.113  < 2e-16 ***
    ## bac1        -1.09590    0.18633  -5.882 4.08e-09 ***
    ## dui:bac1     1.88836    0.20306   9.300  < 2e-16 ***
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
age_rdd <- dat_bandwidth_0.05 %>% lm(aged ~ dui*bac1, data = .)
summaryR.lm(age_rdd, type = "hc1") 
```

    ## 
    ## Call:
    ## lm(formula = aged ~ dui * bac1, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.309  -9.896  -2.972   7.665  46.207 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  39.4530     0.4831  81.666   <2e-16 ***
    ## dui          -6.2244     0.5861 -10.619   <2e-16 ***
    ## bac1        -69.1637     7.2203  -9.579   <2e-16 ***
    ## dui:bac1     76.0493     7.8440   9.695   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.57 on 89963 degrees of freedom
    ## Multiple R-squared:  0.002346,   Adjusted R-squared:  0.002312 
    ## F-statistic: 62.95 on 3 and 89963 DF,  p-value: < 2.2e-16
    ## 
    ## Note: Heteroscedasticity-consistent standard errors using adjustment hc1
