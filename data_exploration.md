data\_exploration
================
Weiheng Zhang
2021/11/28

## Data cleaning

``` r
cdi =
  read_csv("./data/cdi.csv") %>% 
  mutate(
    cty = as.factor(cty),
    state = as.factor(state),
    region = factor(region, levels = c("1", "2", "3", "4"),
                    labels = c("Northeast", "North_Central", "South", "West")),
    CRM_1000 = 1000*crimes/pop,
    pop_den = pop/area,
    pdocs = docs/pop,
    pbeds = beds/pop) %>%
  dplyr::select(-id, -area, -beds, -docs) %>%  # pop and crimes will be removed after detecting state outliers.
  relocate(CRM_1000)

cdi
```

    ## # A tibble: 440 × 17
    ##    CRM_1000 cty      state    pop pop18 pop65 crimes hsgrad bagrad poverty unemp
    ##       <dbl> <fct>    <fct>  <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>
    ##  1     77.7 Los_Ange CA    8.86e6  32.1   9.7 688936   70     22.3    11.6   8  
    ##  2     85.6 Cook     IL    5.11e6  29.2  12.4 436936   73.4   22.8    11.1   7.2
    ##  3     90.0 Harris   TX    2.82e6  31.3   7.1 253526   74.9   25.4    12.5   5.7
    ##  4     69.6 San_Dieg CA    2.50e6  33.5  10.9 173821   81.9   25.3     8.1   6.1
    ##  5     60.0 Orange   CA    2.41e6  32.6   9.2 144524   81.2   27.8     5.2   4.8
    ##  6    296.  Kings    NY    2.30e6  28.3  12.4 680966   63.7   16.6    19.5   9.5
    ##  7     83.7 Maricopa AZ    2.12e6  29.2  12.5 177593   81.5   22.1     8.8   4.9
    ##  8     91.9 Wayne    MI    2.11e6  27.4  12.5 193978   70     13.7    16.9  10  
    ##  9    126.  Dade     FL    1.94e6  27.1  13.9 244725   65     18.8    14.2   8.7
    ## 10    116.  Dallas   TX    1.85e6  32.6   8.2 214258   77.1   26.3    10.4   6.1
    ## # … with 430 more rows, and 6 more variables: pcincome <dbl>, totalinc <dbl>,
    ## #   region <fct>, pop_den <dbl>, pdocs <dbl>, pbeds <dbl>

## Descriptive Statistics.

``` r
summary(cdi)
```

    ##     CRM_1000             cty          state          pop         
    ##  Min.   :  4.601   Jefferso:  7   CA     : 34   Min.   : 100043  
    ##  1st Qu.: 38.102   Montgome:  6   FL     : 29   1st Qu.: 139027  
    ##  Median : 52.429   Washingt:  5   PA     : 29   Median : 217280  
    ##  Mean   : 57.286   Cumberla:  4   TX     : 28   Mean   : 393011  
    ##  3rd Qu.: 72.597   Jackson :  4   OH     : 24   3rd Qu.: 436064  
    ##  Max.   :295.987   Lake    :  4   NY     : 22   Max.   :8863164  
    ##                    (Other) :410   (Other):274                    
    ##      pop18           pop65            crimes           hsgrad     
    ##  Min.   :16.40   Min.   : 3.000   Min.   :   563   Min.   :46.60  
    ##  1st Qu.:26.20   1st Qu.: 9.875   1st Qu.:  6220   1st Qu.:73.88  
    ##  Median :28.10   Median :11.750   Median : 11820   Median :77.70  
    ##  Mean   :28.57   Mean   :12.170   Mean   : 27112   Mean   :77.56  
    ##  3rd Qu.:30.02   3rd Qu.:13.625   3rd Qu.: 26280   3rd Qu.:82.40  
    ##  Max.   :49.70   Max.   :33.800   Max.   :688936   Max.   :92.90  
    ##                                                                   
    ##      bagrad         poverty           unemp           pcincome    
    ##  Min.   : 8.10   Min.   : 1.400   Min.   : 2.200   Min.   : 8899  
    ##  1st Qu.:15.28   1st Qu.: 5.300   1st Qu.: 5.100   1st Qu.:16118  
    ##  Median :19.70   Median : 7.900   Median : 6.200   Median :17759  
    ##  Mean   :21.08   Mean   : 8.721   Mean   : 6.597   Mean   :18561  
    ##  3rd Qu.:25.32   3rd Qu.:10.900   3rd Qu.: 7.500   3rd Qu.:20270  
    ##  Max.   :52.30   Max.   :36.300   Max.   :21.300   Max.   :37541  
    ##                                                                   
    ##     totalinc                region       pop_den             pdocs          
    ##  Min.   :  1141   Northeast    :103   Min.   :   13.26   Min.   :0.0003559  
    ##  1st Qu.:  2311   North_Central:108   1st Qu.:  192.34   1st Qu.:0.0012127  
    ##  Median :  3857   South        :152   Median :  335.91   Median :0.0017509  
    ##  Mean   :  7869   West         : 77   Mean   :  888.44   Mean   :0.0021230  
    ##  3rd Qu.:  8654                       3rd Qu.:  756.55   3rd Qu.:0.0024915  
    ##  Max.   :184230                       Max.   :32403.72   Max.   :0.0170377  
    ##                                                                             
    ##      pbeds          
    ##  Min.   :0.0001649  
    ##  1st Qu.:0.0021972  
    ##  Median :0.0033287  
    ##  Mean   :0.0036493  
    ##  3rd Qu.:0.0045649  
    ##  Max.   :0.0196982  
    ## 

``` r
sapply(cdi, function(x) sum(is.na(x)))
```

    ## CRM_1000      cty    state      pop    pop18    pop65   crimes   hsgrad 
    ##        0        0        0        0        0        0        0        0 
    ##   bagrad  poverty    unemp pcincome totalinc   region  pop_den    pdocs 
    ##        0        0        0        0        0        0        0        0 
    ##    pbeds 
    ##        0

``` r
map(cdi, ~sum(is.na(.)))
```

    ## $CRM_1000
    ## [1] 0
    ## 
    ## $cty
    ## [1] 0
    ## 
    ## $state
    ## [1] 0
    ## 
    ## $pop
    ## [1] 0
    ## 
    ## $pop18
    ## [1] 0
    ## 
    ## $pop65
    ## [1] 0
    ## 
    ## $crimes
    ## [1] 0
    ## 
    ## $hsgrad
    ## [1] 0
    ## 
    ## $bagrad
    ## [1] 0
    ## 
    ## $poverty
    ## [1] 0
    ## 
    ## $unemp
    ## [1] 0
    ## 
    ## $pcincome
    ## [1] 0
    ## 
    ## $totalinc
    ## [1] 0
    ## 
    ## $region
    ## [1] 0
    ## 
    ## $pop_den
    ## [1] 0
    ## 
    ## $pdocs
    ## [1] 0
    ## 
    ## $pbeds
    ## [1] 0

No missing values were found.

Boxplot for each variable

``` r
par(mfrow = c(2, 3))
boxplot(cdi$CRM_1000, main = 'Crime Rate per 1000 people') # an obvious outlier around 300
boxplot(cdi$pop_den,main = 'Population Density')
boxplot(cdi$pop18, main = 'Percent of Population Aged 18-34')
boxplot(cdi$pop65, main = 'Percent of Population Aged 65+')
boxplot(cdi$pdocs, main = 'Per Capita Active Physicians')
boxplot(cdi$pbeds, main = 'Per Capita Hospital Beds')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
par(mfrow = c(2,3))
boxplot(cdi$hsgrad, main = 'Percent High School Graduates')
boxplot(cdi$bagrad, main = 'Percent Bachelor’s Degrees')
boxplot(cdi$poverty, main = 'Percent Below Poverty Level')
boxplot(cdi$unemp, main = 'Percent Unemployment')
boxplot(cdi$pcincome, main = 'Per Capita Income')
boxplot(cdi$totalinc, main = 'Total Personal Income')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Marginal correlation with CRM\_1000 of each variable

``` r
cdi %>% ggplot(aes(x = pop_den, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pop18, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red') # positive correlation
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pop65, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pdocs, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pbeds, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = hsgrad, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red') #negative correlation
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = bagrad, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = poverty, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red') # positive correlation
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = unemp, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pcincome, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = totalinc, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = region, y = CRM_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-12.png)<!-- -->

## Identify counties with unusual crime rates.

``` r
upper = quantile(cdi$CRM_1000, 0.75)
lower = quantile(cdi$CRM_1000, 0.25)
IQR = upper - lower

cdi %>% 
  filter(CRM_1000 > upper + 1.5*IQR,
         CRM_1000 > lower - 1.5*IQR) %>% 
  dplyr::select(cty, CRM_1000) %>%
  knitr::kable(digits = 2)
```

| cty       | CRM\_1000 |
|:----------|----------:|
| Kings     |    295.99 |
| Dade      |    126.34 |
| Fulton    |    143.35 |
| St.\_Loui |    161.60 |

## Group by states and check for outliers again.

``` r
cdi_state = 
  cdi %>% 
  group_by(state) %>% 
  summarise(state_pop = sum(pop),
            state_crimes = sum(crimes)) %>% 
  mutate(state_CRM_1000 = 1000*state_crimes/state_pop) %>% 
  relocate(state_CRM_1000)

cdi_state
```

    ## # A tibble: 48 × 4
    ##    state_CRM_1000 state state_pop state_crimes
    ##             <dbl> <fct>     <dbl>        <dbl>
    ##  1           71.6 AL      1844764       132007
    ##  2          105.  AR       463069        48526
    ##  3           80.2 AZ      3119969       250285
    ##  4           69.3 CA     28917875      2002790
    ##  5           65.8 CO      2625950       172695
    ##  6           50.9 CT      3287116       167208
    ##  7          106.  DC       606900        64393
    ##  8           60.7 DE       552939        33563
    ##  9           86.2 FL     11744171      1011989
    ## 10           93.0 GA      2913394       270900
    ## # … with 38 more rows

``` r
upper = quantile(cdi_state$state_CRM_1000, 0.75)
lower = quantile(cdi_state$state_CRM_1000, 0.25)
IQR = upper - lower

cdi_state %>% 
  filter(state_CRM_1000 > upper + 1.5*IQR,
         state_CRM_1000 > lower - 1.5*IQR) %>% 
  dplyr::select(state, state_CRM_1000) %>%
  knitr::kable(digits = 2)
```

| state | state\_CRM\_1000 |
|:------|-----------------:|

``` r
boxplot(cdi_state$state_CRM_1000, main = 'State Crime Rate per 1000 people')
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Surprisingly, if we look at the CRM\_1000 at state level, no outlier was
found.

## Remove Unnecessary Variables

``` r
cdi = 
  cdi %>% 
  dplyr::select(-pop, -crimes, -cty)
```

## Checking to Outliers and Influential Points

``` r
mult.fit1 = lm(CRM_1000 ~ ., data = cdi) 

# residuals vs leverage plot
plot(mult.fit1, which = 4)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# remove influential points
cdiout = cdi[-c(6),]

# plot with and without influential points
plot(cdi$poverty, cdi$CRM_1000)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
plot(cdiout$poverty, cdiout$CRM_1000)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
# fit model with and without influential points
with = lm(CRM_1000 ~ ., data = cdi) 

without = lm(CRM_1000 ~ ., data = cdiout)

summary(with); summary(without) ##compare p values and r squared
```

    ## 
    ## Call:
    ## lm(formula = CRM_1000 ~ ., data = cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -48.599  -9.856  -0.416   9.458  62.448 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -4.720e+01  2.933e+01  -1.609 0.108409    
    ## stateAR              1.966e+01  1.338e+01   1.469 0.142587    
    ## stateAZ              1.026e+01  1.028e+01   0.998 0.318834    
    ## stateCA             -2.404e+00  7.587e+00  -0.317 0.751489    
    ## stateCO              7.856e+00  8.696e+00   0.903 0.366913    
    ## stateCT             -1.706e+01  9.262e+00  -1.842 0.066314 .  
    ## stateDC             -2.371e+01  1.838e+01  -1.289 0.198031    
    ## stateDE              3.238e+00  1.353e+01   0.239 0.810949    
    ## stateFL              1.970e+01  7.733e+00   2.548 0.011227 *  
    ## stateGA              1.595e+01  8.454e+00   1.886 0.060022 .  
    ## stateHI              1.476e+01  1.173e+01   1.258 0.208994    
    ## stateID             -3.649e+00  1.797e+01  -0.203 0.839153    
    ## stateIL             -1.270e+01  7.803e+00  -1.628 0.104432    
    ## stateIN             -1.710e+01  7.832e+00  -2.184 0.029586 *  
    ## stateKS              2.153e+01  1.062e+01   2.028 0.043234 *  
    ## stateKY             -1.338e+01  1.152e+01  -1.161 0.246413    
    ## stateLA             -4.184e+00  8.587e+00  -0.487 0.626366    
    ## stateMA             -2.724e+01  9.160e+00  -2.974 0.003126 ** 
    ## stateMD             -7.562e-01  8.687e+00  -0.087 0.930680    
    ## stateME             -1.604e+01  1.003e+01  -1.599 0.110669    
    ## stateMI             -3.209e+00  8.048e+00  -0.399 0.690277    
    ## stateMN             -1.789e+01  9.408e+00  -1.902 0.057949 .  
    ## stateMO             -7.457e+00  8.882e+00  -0.840 0.401656    
    ## stateMS             -4.061e+00  1.159e+01  -0.350 0.726163    
    ## stateMT             -2.463e+01  1.794e+01  -1.373 0.170490    
    ## stateNC              8.282e+00  7.643e+00   1.084 0.279201    
    ## stateND             -3.169e+01  1.797e+01  -1.764 0.078577 .  
    ## stateNE             -3.729e+00  1.176e+01  -0.317 0.751382    
    ## stateNH             -1.919e+01  1.089e+01  -1.762 0.078859 .  
    ## stateNJ             -1.388e+01  8.080e+00  -1.718 0.086620 .  
    ## stateNM              1.521e+01  1.364e+01   1.116 0.265233    
    ## stateNV              1.440e+01  1.365e+01   1.055 0.291938    
    ## stateNY             -1.578e+01  7.638e+00  -2.066 0.039474 *  
    ## stateOH             -1.741e+01  7.411e+00  -2.350 0.019296 *  
    ## stateOK              9.750e+00  1.061e+01   0.919 0.358781    
    ## stateOR              9.558e+00  9.748e+00   0.981 0.327419    
    ## statePA             -2.736e+01  7.374e+00  -3.710 0.000238 ***
    ## stateRI             -4.727e+00  1.197e+01  -0.395 0.693002    
    ## stateSC              1.691e+01  8.126e+00   2.081 0.038092 *  
    ## stateSD             -1.956e+01  1.798e+01  -1.088 0.277319    
    ## stateTN              1.234e+00  8.632e+00   0.143 0.886353    
    ## stateTX              1.460e+01  7.156e+00   2.040 0.042011 *  
    ## stateUT              6.852e-01  1.111e+01   0.062 0.950839    
    ## stateVA             -1.033e+01  8.843e+00  -1.168 0.243677    
    ## stateVT             -2.075e+01  1.813e+01  -1.145 0.252949    
    ## stateWA              5.525e+00  8.785e+00   0.629 0.529795    
    ## stateWI             -7.470e+00  8.386e+00  -0.891 0.373647    
    ## stateWV             -1.625e+01  1.780e+01  -0.913 0.361875    
    ## pop18                7.742e-01  3.448e-01   2.246 0.025308 *  
    ## pop65               -6.045e-01  3.594e-01  -1.682 0.093377 .  
    ## hsgrad               5.751e-01  2.967e-01   1.938 0.053347 .  
    ## bagrad              -5.174e-01  2.951e-01  -1.753 0.080414 .  
    ## poverty              1.666e+00  4.160e-01   4.003 7.50e-05 ***
    ## unemp                8.905e-01  6.578e-01   1.354 0.176590    
    ## pcincome             1.100e-03  5.319e-04   2.067 0.039384 *  
    ## totalinc             1.906e-04  7.366e-05   2.588 0.010015 *  
    ## regionNorth_Central         NA         NA      NA       NA    
    ## regionSouth                 NA         NA      NA       NA    
    ## regionWest                  NA         NA      NA       NA    
    ## pop_den              5.373e-03  4.450e-04  12.074  < 2e-16 ***
    ## pdocs               -8.185e+02  1.060e+03  -0.773 0.440288    
    ## pbeds                3.948e+03  8.560e+02   4.612 5.45e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.58 on 381 degrees of freedom
    ## Multiple R-squared:  0.6805, Adjusted R-squared:  0.6319 
    ## F-statistic: 13.99 on 58 and 381 DF,  p-value: < 2.2e-16

    ## 
    ## Call:
    ## lm(formula = CRM_1000 ~ ., data = cdiout)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -40.289  -9.093  -0.302   9.194  54.618 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -5.651e+01  2.793e+01  -2.023 0.043788 *  
    ## stateAR              1.909e+01  1.273e+01   1.500 0.134424    
    ## stateAZ              1.239e+01  9.781e+00   1.267 0.205964    
    ## stateCA             -1.184e+00  7.218e+00  -0.164 0.869780    
    ## stateCO              1.075e+01  8.283e+00   1.297 0.195317    
    ## stateCT             -1.832e+01  8.811e+00  -2.079 0.038260 *  
    ## stateDC             -7.126e-01  1.785e+01  -0.040 0.968174    
    ## stateDE              4.141e+00  1.287e+01   0.322 0.747704    
    ## stateFL              2.044e+01  7.355e+00   2.779 0.005719 ** 
    ## stateGA              1.740e+01  8.044e+00   2.163 0.031160 *  
    ## stateHI              1.643e+01  1.116e+01   1.472 0.141737    
    ## stateID             -6.269e-01  1.709e+01  -0.037 0.970762    
    ## stateIL             -1.109e+01  7.426e+00  -1.493 0.136284    
    ## stateIN             -1.602e+01  7.451e+00  -2.151 0.032122 *  
    ## stateKS              2.302e+01  1.010e+01   2.280 0.023180 *  
    ## stateKY             -1.268e+01  1.096e+01  -1.157 0.247969    
    ## stateLA             -2.400e+00  8.172e+00  -0.294 0.769163    
    ## stateMA             -2.469e+01  8.720e+00  -2.832 0.004875 ** 
    ## stateMD              2.447e+00  8.277e+00   0.296 0.767660    
    ## stateME             -1.534e+01  9.540e+00  -1.608 0.108706    
    ## stateMI             -1.559e+00  7.658e+00  -0.204 0.838823    
    ## stateMN             -1.625e+01  8.951e+00  -1.815 0.070264 .  
    ## stateMO             -5.272e+00  8.454e+00  -0.624 0.533229    
    ## stateMS             -2.610e+00  1.102e+01  -0.237 0.812931    
    ## stateMT             -2.240e+01  1.706e+01  -1.312 0.190188    
    ## stateNC              7.860e+00  7.269e+00   1.081 0.280233    
    ## stateND             -3.164e+01  1.709e+01  -1.852 0.064846 .  
    ## stateNE             -4.080e-03  1.120e+01   0.000 0.999710    
    ## stateNH             -1.860e+01  1.036e+01  -1.795 0.073405 .  
    ## stateNJ             -1.083e+01  7.699e+00  -1.407 0.160197    
    ## stateNM              1.734e+01  1.297e+01   1.337 0.182143    
    ## stateNV              1.294e+01  1.298e+01   0.997 0.319451    
    ## stateNY             -1.806e+01  7.273e+00  -2.484 0.013431 *  
    ## stateOH             -1.483e+01  7.060e+00  -2.100 0.036399 *  
    ## stateOK              1.240e+01  1.010e+01   1.228 0.220249    
    ## stateOR              1.279e+01  9.284e+00   1.378 0.169090    
    ## statePA             -2.537e+01  7.020e+00  -3.614 0.000342 ***
    ## stateRI             -3.457e+00  1.138e+01  -0.304 0.761491    
    ## stateSC              1.699e+01  7.728e+00   2.198 0.028555 *  
    ## stateSD             -2.036e+01  1.710e+01  -1.190 0.234693    
    ## stateTN              1.525e+00  8.209e+00   0.186 0.852721    
    ## stateTX              1.592e+01  6.809e+00   2.338 0.019919 *  
    ## stateUT              7.587e+00  1.062e+01   0.715 0.475292    
    ## stateVA             -2.377e+00  8.501e+00  -0.280 0.779942    
    ## stateVT             -2.290e+01  1.724e+01  -1.328 0.184887    
    ## stateWA              8.498e+00  8.367e+00   1.016 0.310447    
    ## stateWI             -5.414e+00  7.982e+00  -0.678 0.498005    
    ## stateWV             -1.648e+01  1.693e+01  -0.973 0.331065    
    ## pop18                1.185e+00  3.341e-01   3.546 0.000440 ***
    ## pop65               -4.236e-01  3.429e-01  -1.235 0.217474    
    ## hsgrad               3.805e-01  2.838e-01   1.341 0.180817    
    ## bagrad              -6.454e-01  2.814e-01  -2.294 0.022359 *  
    ## poverty              1.704e+00  3.957e-01   4.305 2.13e-05 ***
    ## unemp                8.212e-01  6.257e-01   1.313 0.190124    
    ## pcincome             1.746e-03  5.158e-04   3.385 0.000785 ***
    ## totalinc             2.212e-04  7.021e-05   3.150 0.001762 ** 
    ## regionNorth_Central         NA         NA      NA       NA    
    ## regionSouth                 NA         NA      NA       NA    
    ## regionWest                  NA         NA      NA       NA    
    ## pop_den              2.125e-03  6.596e-04   3.222 0.001385 ** 
    ## pdocs               -7.029e+01  1.014e+03  -0.069 0.944792    
    ## pbeds                4.064e+03  8.143e+02   4.990 9.20e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.77 on 380 degrees of freedom
    ## Multiple R-squared:  0.651,  Adjusted R-squared:  0.5977 
    ## F-statistic: 12.22 on 58 and 380 DF,  p-value: < 2.2e-16

## Check for transformation

``` r
# fit multivariate model
mult.fit1 = lm(CRM_1000 ~ ., data = cdiout) 
summary(mult.fit1)
```

    ## 
    ## Call:
    ## lm(formula = CRM_1000 ~ ., data = cdiout)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -40.289  -9.093  -0.302   9.194  54.618 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -5.651e+01  2.793e+01  -2.023 0.043788 *  
    ## stateAR              1.909e+01  1.273e+01   1.500 0.134424    
    ## stateAZ              1.239e+01  9.781e+00   1.267 0.205964    
    ## stateCA             -1.184e+00  7.218e+00  -0.164 0.869780    
    ## stateCO              1.075e+01  8.283e+00   1.297 0.195317    
    ## stateCT             -1.832e+01  8.811e+00  -2.079 0.038260 *  
    ## stateDC             -7.126e-01  1.785e+01  -0.040 0.968174    
    ## stateDE              4.141e+00  1.287e+01   0.322 0.747704    
    ## stateFL              2.044e+01  7.355e+00   2.779 0.005719 ** 
    ## stateGA              1.740e+01  8.044e+00   2.163 0.031160 *  
    ## stateHI              1.643e+01  1.116e+01   1.472 0.141737    
    ## stateID             -6.269e-01  1.709e+01  -0.037 0.970762    
    ## stateIL             -1.109e+01  7.426e+00  -1.493 0.136284    
    ## stateIN             -1.602e+01  7.451e+00  -2.151 0.032122 *  
    ## stateKS              2.302e+01  1.010e+01   2.280 0.023180 *  
    ## stateKY             -1.268e+01  1.096e+01  -1.157 0.247969    
    ## stateLA             -2.400e+00  8.172e+00  -0.294 0.769163    
    ## stateMA             -2.469e+01  8.720e+00  -2.832 0.004875 ** 
    ## stateMD              2.447e+00  8.277e+00   0.296 0.767660    
    ## stateME             -1.534e+01  9.540e+00  -1.608 0.108706    
    ## stateMI             -1.559e+00  7.658e+00  -0.204 0.838823    
    ## stateMN             -1.625e+01  8.951e+00  -1.815 0.070264 .  
    ## stateMO             -5.272e+00  8.454e+00  -0.624 0.533229    
    ## stateMS             -2.610e+00  1.102e+01  -0.237 0.812931    
    ## stateMT             -2.240e+01  1.706e+01  -1.312 0.190188    
    ## stateNC              7.860e+00  7.269e+00   1.081 0.280233    
    ## stateND             -3.164e+01  1.709e+01  -1.852 0.064846 .  
    ## stateNE             -4.080e-03  1.120e+01   0.000 0.999710    
    ## stateNH             -1.860e+01  1.036e+01  -1.795 0.073405 .  
    ## stateNJ             -1.083e+01  7.699e+00  -1.407 0.160197    
    ## stateNM              1.734e+01  1.297e+01   1.337 0.182143    
    ## stateNV              1.294e+01  1.298e+01   0.997 0.319451    
    ## stateNY             -1.806e+01  7.273e+00  -2.484 0.013431 *  
    ## stateOH             -1.483e+01  7.060e+00  -2.100 0.036399 *  
    ## stateOK              1.240e+01  1.010e+01   1.228 0.220249    
    ## stateOR              1.279e+01  9.284e+00   1.378 0.169090    
    ## statePA             -2.537e+01  7.020e+00  -3.614 0.000342 ***
    ## stateRI             -3.457e+00  1.138e+01  -0.304 0.761491    
    ## stateSC              1.699e+01  7.728e+00   2.198 0.028555 *  
    ## stateSD             -2.036e+01  1.710e+01  -1.190 0.234693    
    ## stateTN              1.525e+00  8.209e+00   0.186 0.852721    
    ## stateTX              1.592e+01  6.809e+00   2.338 0.019919 *  
    ## stateUT              7.587e+00  1.062e+01   0.715 0.475292    
    ## stateVA             -2.377e+00  8.501e+00  -0.280 0.779942    
    ## stateVT             -2.290e+01  1.724e+01  -1.328 0.184887    
    ## stateWA              8.498e+00  8.367e+00   1.016 0.310447    
    ## stateWI             -5.414e+00  7.982e+00  -0.678 0.498005    
    ## stateWV             -1.648e+01  1.693e+01  -0.973 0.331065    
    ## pop18                1.185e+00  3.341e-01   3.546 0.000440 ***
    ## pop65               -4.236e-01  3.429e-01  -1.235 0.217474    
    ## hsgrad               3.805e-01  2.838e-01   1.341 0.180817    
    ## bagrad              -6.454e-01  2.814e-01  -2.294 0.022359 *  
    ## poverty              1.704e+00  3.957e-01   4.305 2.13e-05 ***
    ## unemp                8.212e-01  6.257e-01   1.313 0.190124    
    ## pcincome             1.746e-03  5.158e-04   3.385 0.000785 ***
    ## totalinc             2.212e-04  7.021e-05   3.150 0.001762 ** 
    ## regionNorth_Central         NA         NA      NA       NA    
    ## regionSouth                 NA         NA      NA       NA    
    ## regionWest                  NA         NA      NA       NA    
    ## pop_den              2.125e-03  6.596e-04   3.222 0.001385 ** 
    ## pdocs               -7.029e+01  1.014e+03  -0.069 0.944792    
    ## pbeds                4.064e+03  8.143e+02   4.990 9.20e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.77 on 380 degrees of freedom
    ## Multiple R-squared:  0.651,  Adjusted R-squared:  0.5977 
    ## F-statistic: 12.22 on 58 and 380 DF,  p-value: < 2.2e-16

``` r
boxcox(mult.fit1)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

a is close to 1/2. Perform sqrt transformation.

``` r
cdi_sqrt = 
  cdiout %>% 
  mutate(sqrt_CRM_1000 = sqrt(CRM_1000)) %>% 
  dplyr::select(-CRM_1000) %>% 
  relocate(sqrt_CRM_1000)

cdi_sqrt
```

    ## # A tibble: 439 × 14
    ##    sqrt_CRM_1000 state pop18 pop65 hsgrad bagrad poverty unemp pcincome totalinc
    ##            <dbl> <fct> <dbl> <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
    ##  1          8.82 CA     32.1   9.7   70     22.3    11.6   8      20786   184230
    ##  2          9.25 IL     29.2  12.4   73.4   22.8    11.1   7.2    21729   110928
    ##  3          9.48 TX     31.3   7.1   74.9   25.4    12.5   5.7    19517    55003
    ##  4          8.34 CA     33.5  10.9   81.9   25.3     8.1   6.1    19588    48931
    ##  5          7.74 CA     32.6   9.2   81.2   27.8     5.2   4.8    24400    58818
    ##  6          9.15 AZ     29.2  12.5   81.5   22.1     8.8   4.9    18042    38287
    ##  7          9.58 MI     27.4  12.5   70     13.7    16.9  10      17461    36872
    ##  8         11.2  FL     27.1  13.9   65     18.8    14.2   8.7    17823    34525
    ##  9         10.8  TX     32.6   8.2   77.1   26.3    10.4   6.1    21001    38911
    ## 10          8.30 PA     29.1  15.2   64.3   15.2    16.1   8      16721    26512
    ## # … with 429 more rows, and 4 more variables: region <fct>, pop_den <dbl>,
    ## #   pdocs <dbl>, pbeds <dbl>

``` r
mult.fit2 = lm(sqrt_CRM_1000 ~ ., data = cdi_sqrt) 

summary(mult.fit2)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ ., data = cdi_sqrt)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5895 -0.5960  0.0000  0.6825  3.1749 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -7.751e-02  1.901e+00  -0.041 0.967498    
    ## stateAR              1.038e+00  8.660e-01   1.199 0.231273    
    ## stateAZ              8.428e-01  6.657e-01   1.266 0.206247    
    ## stateCA              3.459e-03  4.912e-01   0.007 0.994385    
    ## stateCO              8.345e-01  5.637e-01   1.480 0.139597    
    ## stateCT             -1.524e+00  5.996e-01  -2.541 0.011436 *  
    ## stateDC             -3.045e-01  1.215e+00  -0.251 0.802170    
    ## stateDE              4.005e-01  8.756e-01   0.457 0.647648    
    ## stateFL              1.248e+00  5.006e-01   2.493 0.013081 *  
    ## stateGA              1.036e+00  5.474e-01   1.892 0.059287 .  
    ## stateHI              1.208e+00  7.593e-01   1.591 0.112542    
    ## stateID              7.840e-02  1.163e+00   0.067 0.946303    
    ## stateIL             -7.056e-01  5.054e-01  -1.396 0.163459    
    ## stateIN             -1.210e+00  5.071e-01  -2.387 0.017475 *  
    ## stateKS              1.408e+00  6.874e-01   2.049 0.041179 *  
    ## stateKY             -7.065e-01  7.460e-01  -0.947 0.344187    
    ## stateLA             -1.875e-01  5.561e-01  -0.337 0.736149    
    ## stateMA             -1.877e+00  5.935e-01  -3.163 0.001689 ** 
    ## stateMD              1.509e-01  5.633e-01   0.268 0.788933    
    ## stateME             -1.008e+00  6.493e-01  -1.553 0.121184    
    ## stateMI             -1.406e-02  5.212e-01  -0.027 0.978491    
    ## stateMN             -9.763e-01  6.092e-01  -1.603 0.109864    
    ## stateMO             -5.616e-01  5.753e-01  -0.976 0.329626    
    ## stateMS             -2.257e-01  7.501e-01  -0.301 0.763676    
    ## stateMT             -1.505e+00  1.161e+00  -1.295 0.195943    
    ## stateNC              5.212e-01  4.947e-01   1.054 0.292773    
    ## stateND             -2.134e+00  1.163e+00  -1.835 0.067308 .  
    ## stateNE             -6.975e-03  7.623e-01  -0.009 0.992704    
    ## stateNH             -1.347e+00  7.051e-01  -1.911 0.056802 .  
    ## stateNJ             -7.910e-01  5.240e-01  -1.510 0.131982    
    ## stateNM              1.231e+00  8.828e-01   1.394 0.163999    
    ## stateNV              9.489e-01  8.833e-01   1.074 0.283381    
    ## stateNY             -1.224e+00  4.950e-01  -2.472 0.013856 *  
    ## stateOH             -1.226e+00  4.805e-01  -2.552 0.011102 *  
    ## stateOK              8.731e-01  6.874e-01   1.270 0.204825    
    ## stateOR              8.963e-01  6.318e-01   1.419 0.156861    
    ## statePA             -1.945e+00  4.778e-01  -4.071 5.69e-05 ***
    ## stateRI             -2.106e-01  7.746e-01  -0.272 0.785855    
    ## stateSC              1.180e+00  5.259e-01   2.243 0.025475 *  
    ## stateSD             -1.230e+00  1.164e+00  -1.057 0.291186    
    ## stateTN              1.490e-01  5.587e-01   0.267 0.789819    
    ## stateTX              1.012e+00  4.634e-01   2.183 0.029615 *  
    ## stateUT              5.793e-01  7.226e-01   0.802 0.423209    
    ## stateVA             -8.112e-02  5.785e-01  -0.140 0.888557    
    ## stateVT             -1.476e+00  1.173e+00  -1.258 0.209132    
    ## stateWA              6.679e-01  5.694e-01   1.173 0.241608    
    ## stateWI             -3.132e-01  5.432e-01  -0.576 0.564651    
    ## stateWV             -9.533e-01  1.152e+00  -0.827 0.408623    
    ## pop18                8.178e-02  2.274e-02   3.597 0.000364 ***
    ## pop65               -1.707e-02  2.334e-02  -0.731 0.465033    
    ## hsgrad               2.242e-02  1.932e-02   1.160 0.246579    
    ## bagrad              -3.975e-02  1.915e-02  -2.076 0.038573 *  
    ## poverty              1.077e-01  2.693e-02   4.000 7.62e-05 ***
    ## unemp                5.195e-02  4.258e-02   1.220 0.223221    
    ## pcincome             1.162e-04  3.510e-05   3.311 0.001018 ** 
    ## totalinc             1.485e-05  4.778e-06   3.108 0.002027 ** 
    ## regionNorth_Central         NA         NA      NA       NA    
    ## regionSouth                 NA         NA      NA       NA    
    ## regionWest                  NA         NA      NA       NA    
    ## pop_den              1.351e-04  4.489e-05   3.010 0.002784 ** 
    ## pdocs               -2.809e+01  6.904e+01  -0.407 0.684362    
    ## pbeds                2.671e+02  5.542e+01   4.819 2.09e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.073 on 380 degrees of freedom
    ## Multiple R-squared:  0.6438, Adjusted R-squared:  0.5894 
    ## F-statistic: 11.84 on 58 and 380 DF,  p-value: < 2.2e-16

``` r
boxcox(mult.fit2) 
```

![](data_exploration_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## check for multicollinearity

``` r
cdi_cor = 
  cdi_sqrt %>%
  mutate(
    state = as.numeric(state),
    region = as.numeric(region)
  ) %>% 
  dplyr::select(-sqrt_CRM_1000)

pairs(cdi_cor)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# Correlation matrix for all variables
cor(cdi_cor) %>% 
  knitr::kable()
```

|          |      state |      pop18 |      pop65 |     hsgrad |     bagrad |    poverty |      unemp |   pcincome |   totalinc |     region |   pop\_den |      pdocs |      pbeds |
|:---------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
| state    |  1.0000000 |  0.0458336 | -0.1390971 | -0.0083831 | -0.0477500 |  0.0047524 | -0.1390343 | -0.1103026 | -0.1258578 | -0.2562575 |  0.0089099 | -0.0488717 | -0.0224824 |
| pop18    |  0.0458336 |  1.0000000 | -0.6163064 |  0.2514195 |  0.4561921 |  0.0345260 | -0.2788381 | -0.0317187 |  0.0719820 |  0.0523271 |  0.1753505 |  0.2370282 |  0.0295423 |
| pop65    | -0.1390971 | -0.6163064 |  1.0000000 | -0.2691950 | -0.3392857 |  0.0063125 |  0.2365637 |  0.0186518 | -0.0231996 | -0.1735022 |  0.0375186 |  0.0186097 |  0.2471372 |
| hsgrad   | -0.0083831 |  0.2514195 | -0.2691950 |  1.0000000 |  0.7085868 | -0.6885900 | -0.5916743 |  0.5234917 |  0.0547361 | -0.0165421 | -0.0541694 |  0.1433871 | -0.2115757 |
| bagrad   | -0.0477500 |  0.4561921 | -0.3392857 |  0.7085868 |  1.0000000 | -0.4079919 | -0.5404104 |  0.6952038 |  0.2269968 |  0.0184593 |  0.2403729 |  0.4412101 | -0.0452782 |
| poverty  |  0.0047524 |  0.0345260 |  0.0063125 | -0.6885900 | -0.4079919 |  1.0000000 |  0.4338054 | -0.6032654 | -0.0520255 |  0.2808142 |  0.0700113 |  0.0641332 |  0.3730669 |
| unemp    | -0.1390343 | -0.2788381 |  0.2365637 | -0.5916743 | -0.5404104 |  0.4338054 |  1.0000000 | -0.3215515 | -0.0409915 | -0.0505757 | -0.0247810 | -0.2483054 | -0.0629361 |
| pcincome | -0.1103026 | -0.0317187 |  0.0186518 |  0.5234917 |  0.6952038 | -0.6032654 | -0.3215515 |  1.0000000 |  0.3524250 | -0.2244520 |  0.3401884 |  0.3601164 | -0.0534450 |
| totalinc | -0.1258578 |  0.0719820 | -0.0231996 |  0.0547361 |  0.2269968 | -0.0520255 | -0.0409915 |  0.3524250 |  1.0000000 |  0.0457996 |  0.3291187 |  0.2004509 |  0.0057142 |
| region   | -0.2562575 |  0.0523271 | -0.1735022 | -0.0165421 |  0.0184593 |  0.2808142 | -0.0505757 | -0.2244520 |  0.0457996 |  1.0000000 | -0.0947031 | -0.0400175 | -0.1135005 |
| pop\_den |  0.0089099 |  0.1753505 |  0.0375186 | -0.0541694 |  0.2403729 |  0.0700113 | -0.0247810 |  0.3401884 |  0.3291187 | -0.0947031 |  1.0000000 |  0.4374799 |  0.2784009 |
| pdocs    | -0.0488717 |  0.2370282 |  0.0186097 |  0.1433871 |  0.4412101 |  0.0641332 | -0.2483054 |  0.3601164 |  0.2004509 | -0.0400175 |  0.4374799 |  1.0000000 |  0.6667072 |
| pbeds    | -0.0224824 |  0.0295423 |  0.2471372 | -0.2115757 | -0.0452782 |  0.3730669 | -0.0629361 | -0.0534450 |  0.0057142 | -0.1135005 |  0.2784009 |  0.6667072 |  1.0000000 |

``` r
corrplot(cor(cdi_cor), 
         method = "color", 
         type = "upper",
         addCoef.col = "black", 
         number.cex = 0.6,
         diag = FALSE) 
```

![](data_exploration_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

## Model Selection

Fit a linear model after removing highly correlated variables.

``` r
new_cdi =
  cdi_sqrt %>% 
  mutate(
    state = as.factor(state),
    region = as.factor(region)) %>% 
  dplyr::select(-state, -bagrad, -pdocs, -totalinc)

mult.fit = lm(sqrt_CRM_1000 ~ ., data = new_cdi)
summary(mult.fit)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ ., data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1650 -0.6753  0.0156  0.7786  3.9944 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         2.101e-01  1.417e+00   0.148  0.88224    
    ## pop18               6.158e-02  1.928e-02   3.194  0.00151 ** 
    ## pop65               4.399e-03  2.025e-02   0.217  0.82816    
    ## hsgrad              3.190e-03  1.470e-02   0.217  0.82832    
    ## poverty             1.062e-01  2.324e-02   4.571 6.36e-06 ***
    ## unemp               5.423e-02  3.442e-02   1.576  0.11579    
    ## pcincome            9.206e-05  2.157e-05   4.268 2.43e-05 ***
    ## regionNorth_Central 8.561e-01  1.781e-01   4.807 2.13e-06 ***
    ## regionSouth         1.997e+00  1.751e-01  11.405  < 2e-16 ***
    ## regionWest          1.837e+00  2.022e-01   9.087  < 2e-16 ***
    ## pop_den             1.316e-04  4.338e-05   3.034  0.00256 ** 
    ## pbeds               2.062e+02  3.767e+01   5.473 7.54e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.178 on 427 degrees of freedom
    ## Multiple R-squared:  0.5181, Adjusted R-squared:  0.5057 
    ## F-statistic: 41.73 on 11 and 427 DF,  p-value: < 2.2e-16

Fit a linear model with interactions terms

``` r
mult.fit_inter = lm(sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + unemp + pcincome + region + pop_den + pbeds + pop65 * region + poverty * region + pcincome * region, data = new_cdi)
summary(mult.fit_inter)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + 
    ##     unemp + pcincome + region + pop_den + pbeds + pop65 * region + 
    ##     poverty * region + pcincome * region, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2073 -0.6863  0.0669  0.7199  3.9311 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   5.439e-01  1.755e+00   0.310  0.75676    
    ## pop18                         6.689e-02  1.989e-02   3.364  0.00084 ***
    ## pop65                        -4.562e-02  4.730e-02  -0.964  0.33539    
    ## hsgrad                       -2.708e-03  1.522e-02  -0.178  0.85889    
    ## poverty                       1.555e-01  5.726e-02   2.715  0.00690 ** 
    ## unemp                         4.236e-02  3.564e-02   1.188  0.23535    
    ## pcincome                      1.176e-04  3.619e-05   3.250  0.00125 ** 
    ## regionNorth_Central          -2.569e+00  1.588e+00  -1.618  0.10637    
    ## regionSouth                   2.309e+00  1.281e+00   1.802  0.07226 .  
    ## regionWest                    4.080e+00  1.579e+00   2.584  0.01010 *  
    ## pop_den                       1.073e-04  4.637e-05   2.313  0.02121 *  
    ## pbeds                         1.858e+02  3.819e+01   4.864 1.63e-06 ***
    ## pop65:regionNorth_Central     5.891e-02  7.336e-02   0.803  0.42241    
    ## pop65:regionSouth             5.955e-02  4.772e-02   1.248  0.21277    
    ## pop65:regionWest              5.574e-03  6.742e-02   0.083  0.93415    
    ## poverty:regionNorth_Central   8.235e-02  6.982e-02   1.179  0.23888    
    ## poverty:regionSouth          -6.764e-02  5.769e-02  -1.172  0.24167    
    ## poverty:regionWest           -9.547e-02  7.071e-02  -1.350  0.17773    
    ## pcincome:regionNorth_Central  1.079e-04  5.810e-05   1.857  0.06407 .  
    ## pcincome:regionSouth         -3.148e-05  4.493e-05  -0.700  0.48401    
    ## pcincome:regionWest          -8.996e-05  5.265e-05  -1.709  0.08822 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.164 on 418 degrees of freedom
    ## Multiple R-squared:  0.5389, Adjusted R-squared:  0.5168 
    ## F-statistic: 24.42 on 20 and 418 DF,  p-value: < 2.2e-16

Backward Elimination

``` r
# No pop65 * region
step1 = update(mult.fit_inter, . ~ . -pop65 * region)
summary(step1)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + hsgrad + poverty + unemp + 
    ##     pcincome + pop_den + pbeds + poverty:region + pcincome:region, 
    ##     data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3620 -0.6835  0.0390  0.7613  4.1663 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  1.695e+00  1.265e+00   1.340 0.181079    
    ## pop18                        5.627e-02  1.587e-02   3.545 0.000436 ***
    ## hsgrad                       3.928e-03  1.471e-02   0.267 0.789599    
    ## poverty                      6.420e-02  4.329e-02   1.483 0.138842    
    ## unemp                        3.611e-02  3.587e-02   1.006 0.314781    
    ## pcincome                     5.099e-05  2.181e-05   2.338 0.019870 *  
    ## pop_den                      1.398e-04  4.612e-05   3.031 0.002590 ** 
    ## pbeds                        1.861e+02  3.663e+01   5.079 5.68e-07 ***
    ## poverty:regionNorth_Central  8.556e-02  4.195e-02   2.039 0.042027 *  
    ## poverty:regionSouth          5.599e-02  3.605e-02   1.553 0.121158    
    ## poverty:regionWest           8.130e-02  4.018e-02   2.023 0.043667 *  
    ## pcincome:regionNorth_Central 8.403e-06  1.712e-05   0.491 0.623790    
    ## pcincome:regionSouth         7.982e-05  1.429e-05   5.585 4.17e-08 ***
    ## pcincome:regionWest          5.575e-05  1.726e-05   3.230 0.001335 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.19 on 425 degrees of freedom
    ## Multiple R-squared:  0.5101, Adjusted R-squared:  0.4951 
    ## F-statistic: 34.04 on 13 and 425 DF,  p-value: < 2.2e-16

``` r
# No hsgrad
step2 = update(step1, . ~ . -hsgrad)
summary(step2)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + 
    ##     pop_den + pbeds + poverty:region + pcincome:region, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3787 -0.6897  0.0264  0.7653  4.1425 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  1.964e+00  7.620e-01   2.577 0.010295 *  
    ## pop18                        5.778e-02  1.481e-02   3.901 0.000111 ***
    ## poverty                      6.246e-02  4.275e-02   1.461 0.144775    
    ## unemp                        3.276e-02  3.357e-02   0.976 0.329793    
    ## pcincome                     5.255e-05  2.099e-05   2.503 0.012690 *  
    ## pop_den                      1.370e-04  4.491e-05   3.051 0.002425 ** 
    ## pbeds                        1.860e+02  3.659e+01   5.082 5.59e-07 ***
    ## poverty:regionNorth_Central  8.507e-02  4.187e-02   2.032 0.042781 *  
    ## poverty:regionSouth          5.529e-02  3.592e-02   1.539 0.124451    
    ## poverty:regionWest           8.099e-02  4.012e-02   2.019 0.044147 *  
    ## pcincome:regionNorth_Central 9.062e-06  1.692e-05   0.536 0.592549    
    ## pcincome:regionSouth         8.006e-05  1.425e-05   5.619 3.47e-08 ***
    ## pcincome:regionWest          5.659e-05  1.695e-05   3.338 0.000918 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.189 on 426 degrees of freedom
    ## Multiple R-squared:   0.51,  Adjusted R-squared:  0.4962 
    ## F-statistic: 36.95 on 12 and 426 DF,  p-value: < 2.2e-16

``` r
# No pcincome * region
step3 = update(step2, . ~ . -pcincome * region)
summary(step3)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + poverty + unemp + pop_den + 
    ##     pbeds + poverty:region, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2460 -0.7844 -0.0327  0.7950  4.2647 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  4.384e+00  5.350e-01   8.196 2.89e-15 ***
    ## pop18                        4.767e-02  1.534e-02   3.107 0.002013 ** 
    ## poverty                     -1.060e-01  3.155e-02  -3.361 0.000846 ***
    ## unemp                        1.882e-02  3.412e-02   0.552 0.581527    
    ## pop_den                      2.538e-04  4.030e-05   6.298 7.47e-10 ***
    ## pbeds                        2.223e+02  3.747e+01   5.933 6.11e-09 ***
    ## poverty:regionNorth_Central  1.221e-01  2.333e-02   5.232 2.63e-07 ***
    ## poverty:regionSouth          2.148e-01  2.291e-02   9.374  < 2e-16 ***
    ## poverty:regionWest           2.208e-01  2.438e-02   9.058  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.246 on 430 degrees of freedom
    ## Multiple R-squared:  0.4569, Adjusted R-squared:  0.4468 
    ## F-statistic: 45.22 on 8 and 430 DF,  p-value: < 2.2e-16

``` r
# No unemp
step4 = update(step3, . ~ . -unemp)
summary(step4)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + poverty + pop_den + pbeds + 
    ##     poverty:region, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2567 -0.8083 -0.0335  0.7952  4.2987 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  4.548e+00  4.449e-01  10.222  < 2e-16 ***
    ## pop18                        4.503e-02  1.457e-02   3.092 0.002119 ** 
    ## poverty                     -9.634e-02  2.619e-02  -3.679 0.000264 ***
    ## pop_den                      2.539e-04  4.026e-05   6.306 7.11e-10 ***
    ## pbeds                        2.163e+02  3.578e+01   6.044 3.26e-09 ***
    ## poverty:regionNorth_Central  1.195e-01  2.286e-02   5.229 2.67e-07 ***
    ## poverty:regionSouth          2.100e-01  2.122e-02   9.900  < 2e-16 ***
    ## poverty:regionWest           2.183e-01  2.394e-02   9.120  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.245 on 431 degrees of freedom
    ## Multiple R-squared:  0.4565, Adjusted R-squared:  0.4477 
    ## F-statistic: 51.72 on 7 and 431 DF,  p-value: < 2.2e-16

``` r
multi_fit_back1 = lm(sqrt_CRM_1000 ~ pop18 + poverty + pop_den + pbeds + poverty * region, data = new_cdi)
summary(multi_fit_back1)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + poverty + pop_den + pbeds + 
    ##     poverty * region, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4945 -0.7278 -0.0008  0.7728  4.4456 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  3.696e+00  4.875e-01   7.583 2.12e-13 ***
    ## pop18                        4.710e-02  1.403e-02   3.357 0.000858 ***
    ## poverty                      1.757e-02  3.957e-02   0.444 0.657293    
    ## pop_den                      2.235e-04  3.910e-05   5.716 2.05e-08 ***
    ## pbeds                        1.999e+02  3.466e+01   5.766 1.55e-08 ***
    ## regionNorth_Central         -2.028e-01  4.044e-01  -0.502 0.616186    
    ## regionSouth                  1.605e+00  3.439e-01   4.667 4.09e-06 ***
    ## regionWest                   1.420e+00  4.362e-01   3.255 0.001222 ** 
    ## poverty:regionNorth_Central  1.238e-01  5.214e-02   2.374 0.018013 *  
    ## poverty:regionSouth          4.317e-02  4.281e-02   1.009 0.313771    
    ## poverty:regionWest           5.259e-02  5.209e-02   1.010 0.313280    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.198 on 428 degrees of freedom
    ## Multiple R-squared:  0.5002, Adjusted R-squared:  0.4886 
    ## F-statistic: 42.84 on 10 and 428 DF,  p-value: < 2.2e-16

``` r
# multi_fit_back1: adjusted r squared is 0.4477 

# just use one function for backward 
step(mult.fit_inter, direction = 'backward')
```

    ## Start:  AIC=153.99
    ## sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + pop65 * region + poverty * region + 
    ##     pcincome * region
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - pop65:region     3     3.169 569.74 150.44
    ## - hsgrad           1     0.043 566.61 152.02
    ## - unemp            1     1.914 568.48 153.47
    ## <none>                         566.57 153.99
    ## - pop_den          1     7.251 573.82 157.57
    ## - pcincome:region  3    13.710 580.28 158.49
    ## - poverty:region   3    13.823 580.39 158.57
    ## - pop18            1    15.335 581.90 163.71
    ## - pbeds            1    32.074 598.64 176.16
    ## 
    ## Step:  AIC=150.44
    ## sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + poverty:region + pcincome:region
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - pop65            1     0.045 569.78 148.47
    ## - hsgrad           1     0.107 569.84 148.52
    ## - unemp            1     2.345 572.08 150.24
    ## <none>                         569.74 150.44
    ## - pop_den          1     7.363 577.10 154.07
    ## - pcincome:region  3    14.316 584.05 155.33
    ## - poverty:region   3    18.180 587.92 158.23
    ## - pop18            1    17.207 586.95 161.50
    ## - pbeds            1    31.881 601.62 172.34
    ## 
    ## Step:  AIC=148.47
    ## sqrt_CRM_1000 ~ pop18 + hsgrad + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + poverty:region + pcincome:region
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - hsgrad           1     0.115 569.90 146.56
    ## - unemp            1     2.428 572.21 148.34
    ## <none>                         569.78 148.47
    ## - pop_den          1     7.490 577.27 152.21
    ## - pcincome:region  3    14.280 584.06 153.34
    ## - poverty:region   3    18.329 588.11 156.37
    ## - pop18            1    24.342 594.13 164.84
    ## - pbeds            1    36.617 606.40 173.81
    ## 
    ## Step:  AIC=146.56
    ## sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + region + 
    ##     pop_den + pbeds + poverty:region + pcincome:region
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## <none>                         569.90 146.56
    ## - unemp            1     3.073 572.97 146.92
    ## - pop_den          1     8.337 578.23 150.94
    ## - pcincome:region  3    14.220 584.12 151.38
    ## - poverty:region   3    18.214 588.11 154.37
    ## - pop18            1    27.317 597.22 165.12
    ## - pbeds            1    36.658 606.56 171.93

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + poverty:region + pcincome:region, 
    ##     data = new_cdi)
    ## 
    ## Coefficients:
    ##                  (Intercept)                         pop18  
    ##                   -1.959e-01                     6.579e-02  
    ##                      poverty                         unemp  
    ##                    1.395e-01                     5.046e-02  
    ##                     pcincome           regionNorth_Central  
    ##                    1.163e-04                    -1.907e+00  
    ##                  regionSouth                    regionWest  
    ##                    3.016e+00                     4.231e+00  
    ##                      pop_den                         pbeds  
    ##                    1.112e-04                     1.870e+02  
    ##  poverty:regionNorth_Central           poverty:regionSouth  
    ##                    1.027e-01                    -5.264e-02  
    ##           poverty:regionWest  pcincome:regionNorth_Central  
    ##                   -8.552e-02                     1.067e-04  
    ##         pcincome:regionSouth           pcincome:regionWest  
    ##                   -3.133e-05                    -9.168e-05

``` r
multi_fit_back2 = lm(sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + region + pop_den + pbeds + poverty * region, data = new_cdi)
summary(multi_fit_back2)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + poverty * region, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3667 -0.6859  0.0422  0.7399  3.9752 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  3.953e-01  8.684e-01   0.455  0.64915    
    ## pop18                        6.166e-02  1.465e-02   4.209 3.13e-05 ***
    ## poverty                      1.252e-01  4.871e-02   2.570  0.01050 *  
    ## unemp                        4.287e-02  3.351e-02   1.279  0.20154    
    ## pcincome                     9.994e-05  2.202e-05   4.539 7.36e-06 ***
    ## regionNorth_Central          3.237e-01  4.115e-01   0.787  0.43186    
    ## regionSouth                  2.246e+00  3.633e-01   6.182 1.48e-09 ***
    ## regionWest                   1.930e+00  4.509e-01   4.280 2.31e-05 ***
    ## pop_den                      1.162e-04  4.483e-05   2.592  0.00987 ** 
    ## pbeds                        1.876e+02  3.604e+01   5.204 3.04e-07 ***
    ## poverty:regionNorth_Central  6.520e-02  5.253e-02   1.241  0.21523    
    ## poverty:regionSouth         -3.091e-02  4.563e-02  -0.677  0.49859    
    ## poverty:regionWest          -1.718e-02  5.327e-02  -0.323  0.74721    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.171 on 426 degrees of freedom
    ## Multiple R-squared:  0.5246, Adjusted R-squared:  0.5112 
    ## F-statistic: 39.17 on 12 and 426 DF,  p-value: < 2.2e-16

``` r
anova(multi_fit_back2)
```

    ## Analysis of Variance Table
    ## 
    ## Response: sqrt_CRM_1000
    ##                 Df Sum Sq Mean Sq  F value    Pr(>F)    
    ## pop18            1  52.14  52.138  38.0244 1.624e-09 ***
    ## poverty          1 245.17 245.174 178.8064 < 2.2e-16 ***
    ## unemp            1  27.17  27.168  19.8137 1.092e-05 ***
    ## pcincome         1  69.39  69.393  50.6086 4.803e-12 ***
    ## region           3 167.49  55.829  40.7165 < 2.2e-16 ***
    ## pop_den          1  25.74  25.739  18.7713 1.840e-05 ***
    ## pbeds            1  49.30  49.297  35.9522 4.317e-09 ***
    ## poverty:region   3   8.13   2.709   1.9756    0.1169    
    ## Residuals      426 584.12   1.371                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
broom::glance(multi_fit_back2)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.525         0.511  1.17      39.2 2.62e-61    12  -686. 1399. 1456.
    ## # … with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
broom::tidy(multi_fit_back2)
```

    ## # A tibble: 13 × 5
    ##    term                           estimate  std.error statistic       p.value
    ##    <chr>                             <dbl>      <dbl>     <dbl>         <dbl>
    ##  1 (Intercept)                   0.395      0.868         0.455 0.649        
    ##  2 pop18                         0.0617     0.0146        4.21  0.0000313    
    ##  3 poverty                       0.125      0.0487        2.57  0.0105       
    ##  4 unemp                         0.0429     0.0335        1.28  0.202        
    ##  5 pcincome                      0.0000999  0.0000220     4.54  0.00000736   
    ##  6 regionNorth_Central           0.324      0.411         0.787 0.432        
    ##  7 regionSouth                   2.25       0.363         6.18  0.00000000148
    ##  8 regionWest                    1.93       0.451         4.28  0.0000231    
    ##  9 pop_den                       0.000116   0.0000448     2.59  0.00987      
    ## 10 pbeds                       188.        36.0           5.20  0.000000304  
    ## 11 poverty:regionNorth_Central   0.0652     0.0525        1.24  0.215        
    ## 12 poverty:regionSouth          -0.0309     0.0456       -0.677 0.499        
    ## 13 poverty:regionWest           -0.0172     0.0533       -0.323 0.747

``` r
# multi_fit_back2: Adjusted R-squared is 0.5112 and AIC=146.56
```

Forward Selection

``` r
step(mult.fit_inter, direction = 'forward')
```

    ## Start:  AIC=153.99
    ## sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + pop65 * region + poverty * region + 
    ##     pcincome * region

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + 
    ##     unemp + pcincome + region + pop_den + pbeds + pop65 * region + 
    ##     poverty * region + pcincome * region, data = new_cdi)
    ## 
    ## Coefficients:
    ##                  (Intercept)                         pop18  
    ##                    5.439e-01                     6.689e-02  
    ##                        pop65                        hsgrad  
    ##                   -4.562e-02                    -2.708e-03  
    ##                      poverty                         unemp  
    ##                    1.555e-01                     4.236e-02  
    ##                     pcincome           regionNorth_Central  
    ##                    1.176e-04                    -2.569e+00  
    ##                  regionSouth                    regionWest  
    ##                    2.309e+00                     4.080e+00  
    ##                      pop_den                         pbeds  
    ##                    1.073e-04                     1.858e+02  
    ##    pop65:regionNorth_Central             pop65:regionSouth  
    ##                    5.891e-02                     5.955e-02  
    ##             pop65:regionWest   poverty:regionNorth_Central  
    ##                    5.574e-03                     8.235e-02  
    ##          poverty:regionSouth            poverty:regionWest  
    ##                   -6.764e-02                    -9.547e-02  
    ## pcincome:regionNorth_Central          pcincome:regionSouth  
    ##                    1.079e-04                    -3.148e-05  
    ##          pcincome:regionWest  
    ##                   -8.996e-05

``` r
multi_fit_forward = lm(sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + unemp + pcincome + region + pop_den + pbeds + pop65 * region + poverty * region + pcincome * region, data = new_cdi)
summary(multi_fit_forward)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + 
    ##     unemp + pcincome + region + pop_den + pbeds + pop65 * region + 
    ##     poverty * region + pcincome * region, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2073 -0.6863  0.0669  0.7199  3.9311 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   5.439e-01  1.755e+00   0.310  0.75676    
    ## pop18                         6.689e-02  1.989e-02   3.364  0.00084 ***
    ## pop65                        -4.562e-02  4.730e-02  -0.964  0.33539    
    ## hsgrad                       -2.708e-03  1.522e-02  -0.178  0.85889    
    ## poverty                       1.555e-01  5.726e-02   2.715  0.00690 ** 
    ## unemp                         4.236e-02  3.564e-02   1.188  0.23535    
    ## pcincome                      1.176e-04  3.619e-05   3.250  0.00125 ** 
    ## regionNorth_Central          -2.569e+00  1.588e+00  -1.618  0.10637    
    ## regionSouth                   2.309e+00  1.281e+00   1.802  0.07226 .  
    ## regionWest                    4.080e+00  1.579e+00   2.584  0.01010 *  
    ## pop_den                       1.073e-04  4.637e-05   2.313  0.02121 *  
    ## pbeds                         1.858e+02  3.819e+01   4.864 1.63e-06 ***
    ## pop65:regionNorth_Central     5.891e-02  7.336e-02   0.803  0.42241    
    ## pop65:regionSouth             5.955e-02  4.772e-02   1.248  0.21277    
    ## pop65:regionWest              5.574e-03  6.742e-02   0.083  0.93415    
    ## poverty:regionNorth_Central   8.235e-02  6.982e-02   1.179  0.23888    
    ## poverty:regionSouth          -6.764e-02  5.769e-02  -1.172  0.24167    
    ## poverty:regionWest           -9.547e-02  7.071e-02  -1.350  0.17773    
    ## pcincome:regionNorth_Central  1.079e-04  5.810e-05   1.857  0.06407 .  
    ## pcincome:regionSouth         -3.148e-05  4.493e-05  -0.700  0.48401    
    ## pcincome:regionWest          -8.996e-05  5.265e-05  -1.709  0.08822 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.164 on 418 degrees of freedom
    ## Multiple R-squared:  0.5389, Adjusted R-squared:  0.5168 
    ## F-statistic: 24.42 on 20 and 418 DF,  p-value: < 2.2e-16

``` r
# our Adjusted R-squared is 0.5168 and AIC=153.99
```

Use both selection

``` r
step(mult.fit_inter, direction = 'both')
```

    ## Start:  AIC=153.99
    ## sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + pop65 * region + poverty * region + 
    ##     pcincome * region
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - pop65:region     3     3.169 569.74 150.44
    ## - hsgrad           1     0.043 566.61 152.02
    ## - unemp            1     1.914 568.48 153.47
    ## <none>                         566.57 153.99
    ## - pop_den          1     7.251 573.82 157.57
    ## - pcincome:region  3    13.710 580.28 158.49
    ## - poverty:region   3    13.823 580.39 158.57
    ## - pop18            1    15.335 581.90 163.71
    ## - pbeds            1    32.074 598.64 176.16
    ## 
    ## Step:  AIC=150.44
    ## sqrt_CRM_1000 ~ pop18 + pop65 + hsgrad + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + poverty:region + pcincome:region
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - pop65            1     0.045 569.78 148.47
    ## - hsgrad           1     0.107 569.84 148.52
    ## - unemp            1     2.345 572.08 150.24
    ## <none>                         569.74 150.44
    ## + pop65:region     3     3.169 566.57 153.99
    ## - pop_den          1     7.363 577.10 154.07
    ## - pcincome:region  3    14.316 584.05 155.33
    ## - poverty:region   3    18.180 587.92 158.23
    ## - pop18            1    17.207 586.95 161.50
    ## - pbeds            1    31.881 601.62 172.34
    ## 
    ## Step:  AIC=148.47
    ## sqrt_CRM_1000 ~ pop18 + hsgrad + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + poverty:region + pcincome:region
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## - hsgrad           1     0.115 569.90 146.56
    ## - unemp            1     2.428 572.21 148.34
    ## <none>                         569.78 148.47
    ## + pop65            1     0.045 569.74 150.44
    ## - pop_den          1     7.490 577.27 152.21
    ## - pcincome:region  3    14.280 584.06 153.34
    ## - poverty:region   3    18.329 588.11 156.37
    ## - pop18            1    24.342 594.13 164.84
    ## - pbeds            1    36.617 606.40 173.81
    ## 
    ## Step:  AIC=146.56
    ## sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + region + 
    ##     pop_den + pbeds + poverty:region + pcincome:region
    ## 
    ##                   Df Sum of Sq    RSS    AIC
    ## <none>                         569.90 146.56
    ## - unemp            1     3.073 572.97 146.92
    ## + hsgrad           1     0.115 569.78 148.47
    ## + pop65            1     0.053 569.84 148.52
    ## - pop_den          1     8.337 578.23 150.94
    ## - pcincome:region  3    14.220 584.12 151.38
    ## - poverty:region   3    18.214 588.11 154.37
    ## - pop18            1    27.317 597.22 165.12
    ## - pbeds            1    36.658 606.56 171.93

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + poverty:region + pcincome:region, 
    ##     data = new_cdi)
    ## 
    ## Coefficients:
    ##                  (Intercept)                         pop18  
    ##                   -1.959e-01                     6.579e-02  
    ##                      poverty                         unemp  
    ##                    1.395e-01                     5.046e-02  
    ##                     pcincome           regionNorth_Central  
    ##                    1.163e-04                    -1.907e+00  
    ##                  regionSouth                    regionWest  
    ##                    3.016e+00                     4.231e+00  
    ##                      pop_den                         pbeds  
    ##                    1.112e-04                     1.870e+02  
    ##  poverty:regionNorth_Central           poverty:regionSouth  
    ##                    1.027e-01                    -5.264e-02  
    ##           poverty:regionWest  pcincome:regionNorth_Central  
    ##                   -8.552e-02                     1.067e-04  
    ##         pcincome:regionSouth           pcincome:regionWest  
    ##                   -3.133e-05                    -9.168e-05

``` r
multi_fit_both = lm(sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + region + pop_den + pbeds + poverty * region + pcincome * region, data = new_cdi)
summary(multi_fit_both)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + poverty + unemp + pcincome + 
    ##     region + pop_den + pbeds + poverty * region + pcincome * 
    ##     region, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1887 -0.7003  0.0553  0.7049  3.9034 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -1.959e-01  1.164e+00  -0.168  0.86646    
    ## pop18                         6.579e-02  1.461e-02   4.503 8.68e-06 ***
    ## poverty                       1.395e-01  5.467e-02   2.551  0.01109 *  
    ## unemp                         5.046e-02  3.341e-02   1.510  0.13171    
    ## pcincome                      1.163e-04  3.527e-05   3.297  0.00106 ** 
    ## regionNorth_Central          -1.907e+00  1.411e+00  -1.352  0.17718    
    ## regionSouth                   3.016e+00  1.152e+00   2.619  0.00913 ** 
    ## regionWest                    4.231e+00  1.393e+00   3.037  0.00253 ** 
    ## pop_den                       1.112e-04  4.471e-05   2.488  0.01325 *  
    ## pbeds                         1.870e+02  3.586e+01   5.216 2.87e-07 ***
    ## poverty:regionNorth_Central   1.027e-01  6.350e-02   1.617  0.10660    
    ## poverty:regionSouth          -5.264e-02  5.561e-02  -0.946  0.34443    
    ## poverty:regionWest           -8.552e-02  6.722e-02  -1.272  0.20398    
    ## pcincome:regionNorth_Central  1.067e-04  5.777e-05   1.847  0.06540 .  
    ## pcincome:regionSouth         -3.133e-05  4.455e-05  -0.703  0.48228    
    ## pcincome:regionWest          -9.168e-05  5.156e-05  -1.778  0.07609 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.161 on 423 degrees of freedom
    ## Multiple R-squared:  0.5362, Adjusted R-squared:  0.5197 
    ## F-statistic:  32.6 on 15 and 423 DF,  p-value: < 2.2e-16

``` r
# our Adjusted R-squared is 0.5197 and AIC=146.56
```

Test based procedure

``` r
cdi_test = 
  new_cdi %>% 
  mutate(region = as.numeric(region))

mat = as.matrix(cdi_test)
# Printing the 2 best models of each size, using the Cp criterion:
leaps(x = mat[,2:10], y = mat[,1], nbest = 2, method = "Cp")
```

    ## $which
    ##       1     2     3     4     5     6     7     8     9
    ## 1 FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
    ## 1 FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## 2 FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE
    ## 2 FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE
    ## 3 FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 3 FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE
    ## 4 FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE
    ## 4 FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
    ## 5  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
    ## 5  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE
    ## 6  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 6  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
    ## 7  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 7  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 8  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 8  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 9  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 
    ## $label
    ##  [1] "(Intercept)" "1"           "2"           "3"           "4"          
    ##  [6] "5"           "6"           "7"           "8"           "9"          
    ## 
    ## $size
    ##  [1]  2  2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10
    ## 
    ## $Cp
    ##  [1] 232.299940 243.143342  96.114852 145.670858  60.501572  70.129178
    ##  [7]  33.471233  35.165152  12.622162  21.051674   5.917987  10.879598
    ## [13]   6.127540   7.602038   8.001656   8.127540  10.000000

``` r
# Printing the 2 best models of each size, using the adjusted R^2 criterion:
leaps(x = mat[,2:10], y = mat[,1], nbest = 2, method = "adjr2")
```

    ## $which
    ##       1     2     3     4     5     6     7     8     9
    ## 1 FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
    ## 1 FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## 2 FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE
    ## 2 FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE
    ## 3 FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 3 FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE
    ## 4 FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE
    ## 4 FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
    ## 5  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
    ## 5  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE
    ## 6  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 6  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
    ## 7  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 7  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 8  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 8  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 9  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 
    ## $label
    ##  [1] "(Intercept)" "1"           "2"           "3"           "4"          
    ##  [6] "5"           "6"           "7"           "8"           "9"          
    ## 
    ## $size
    ##  [1]  2  2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10
    ## 
    ## $adjr2
    ##  [1] 0.2038944 0.1909580 0.3673049 0.3080477 0.4109303 0.3993915 0.4444463
    ##  [8] 0.4424115 0.4706747 0.4605251 0.4799538 0.4739660 0.4809130 0.4791294
    ## [15] 0.4798585 0.4797058 0.4786480

``` r
# Function regsubsets() performs a subset selection by identifying the "best" model that contains
# a certain number of predictors. By default "best" is chosen using SSE/RSS (smaller is better)
b = regsubsets(sqrt_CRM_1000 ~ ., data = cdi_test)
rs = summary(b)

# plot of Cp and Adj-R2 as functions of parameters
par(mfrow = c(1,2))

plot(2:9, rs$cp, xlab = "No of parameters", ylab = "Cp Statistic")
abline(0,1)

plot(2:9, rs$adjr2, xlab = "No of parameters", ylab = "Adj R2")
```

![](data_exploration_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
multi_fit_test = lm(sqrt_CRM_1000 ~ pop18 + hsgrad + poverty + pcincome + region + pop_den + pbeds + poverty * region, data = cdi_test)
summary(multi_fit_test)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + hsgrad + poverty + pcincome + 
    ##     region + pop_den + pbeds + poverty * region, data = cdi_test)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4336 -0.7237 -0.0206  0.7634  3.7380 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     8.450e-01  1.095e+00   0.772 0.440730    
    ## pop18           7.182e-02  1.565e-02   4.588 5.87e-06 ***
    ## hsgrad         -2.120e-02  1.355e-02  -1.565 0.118419    
    ## poverty         1.924e-01  5.439e-02   3.537 0.000449 ***
    ## pcincome        1.060e-04  2.268e-05   4.673 3.98e-06 ***
    ## region          9.126e-01  1.393e-01   6.551 1.64e-10 ***
    ## pop_den         9.193e-05  4.597e-05   2.000 0.046174 *  
    ## pbeds           2.090e+02  3.437e+01   6.081 2.63e-09 ***
    ## poverty:region -2.786e-02  1.672e-02  -1.667 0.096335 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.204 on 430 degrees of freedom
    ## Multiple R-squared:  0.4925, Adjusted R-squared:  0.483 
    ## F-statistic: 52.16 on 8 and 430 DF,  p-value: < 2.2e-16

``` r
#adjusted r squared = 0.483
```

## Model diagnostics

Diagnostic Plots for Backward Elimination 1

``` r
par(mfrow = c(2,2))
plot(multi_fit_back1)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

Diagnostic Plots for Backward Elimination 2

``` r
par(mfrow = c(2,2))
plot(multi_fit_back2)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

Diagnostic Plots for Forward Selection

``` r
par(mfrow = c(2,2))
plot(multi_fit_forward)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

Diagnostic Plots for Both Selection

``` r
par(mfrow = c(2,2))
plot(multi_fit_both)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

Diagnostic Plots for Test based procedure

``` r
par(mfrow = c(2,2))
plot(multi_fit_test)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
