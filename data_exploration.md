data\_exploration
================
Weiheng Zhang
2021/11/28

``` r
library(tidyverse)
library(lubridate)
library(dplyr)
library(p8105.datasets)
library(leaflet)
library(MASS)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Data cleaning

``` r
cdi =
  read_csv("./data/cdi.csv") %>% 
  mutate(
    cty = as.factor(cty),
    state = as.factor(state),
    region = factor(region, levels = c("1", "2", "3", "4"),
                    labels = c("Northeast", "North_Central", "South", "West")),
    CRM_1000 = 1000*crimes/pop #每1000人中的犯罪数量(county level)
  ) %>% 
  relocate(CRM_1000)

cdi
```

    ## # A tibble: 440 x 18
    ##    CRM_1000    id cty   state  area    pop pop18 pop65  docs  beds crimes hsgrad
    ##       <dbl> <dbl> <fct> <fct> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
    ##  1     77.7     1 Los_~ CA     4060 8.86e6  32.1   9.7 23677 27700 688936   70  
    ##  2     85.6     2 Cook  IL      946 5.11e6  29.2  12.4 15153 21550 436936   73.4
    ##  3     90.0     3 Harr~ TX     1729 2.82e6  31.3   7.1  7553 12449 253526   74.9
    ##  4     69.6     4 San_~ CA     4205 2.50e6  33.5  10.9  5905  6179 173821   81.9
    ##  5     60.0     5 Oran~ CA      790 2.41e6  32.6   9.2  6062  6369 144524   81.2
    ##  6    296.      6 Kings NY       71 2.30e6  28.3  12.4  4861  8942 680966   63.7
    ##  7     83.7     7 Mari~ AZ     9204 2.12e6  29.2  12.5  4320  6104 177593   81.5
    ##  8     91.9     8 Wayne MI      614 2.11e6  27.4  12.5  3823  9490 193978   70  
    ##  9    126.      9 Dade  FL     1945 1.94e6  27.1  13.9  6274  8840 244725   65  
    ## 10    116.     10 Dall~ TX      880 1.85e6  32.6   8.2  4718  6934 214258   77.1
    ## # ... with 430 more rows, and 6 more variables: bagrad <dbl>, poverty <dbl>,
    ## #   unemp <dbl>, pcincome <dbl>, totalinc <dbl>, region <fct>

## Descriptive Statistics.

``` r
summary(cdi)
```

    ##     CRM_1000             id              cty          state    
    ##  Min.   :  4.601   Min.   :  1.0   Jefferso:  7   CA     : 34  
    ##  1st Qu.: 38.102   1st Qu.:110.8   Montgome:  6   FL     : 29  
    ##  Median : 52.429   Median :220.5   Washingt:  5   PA     : 29  
    ##  Mean   : 57.286   Mean   :220.5   Cumberla:  4   TX     : 28  
    ##  3rd Qu.: 72.597   3rd Qu.:330.2   Jackson :  4   OH     : 24  
    ##  Max.   :295.987   Max.   :440.0   Lake    :  4   NY     : 22  
    ##                                    (Other) :410   (Other):274  
    ##       area              pop              pop18           pop65       
    ##  Min.   :   15.0   Min.   : 100043   Min.   :16.40   Min.   : 3.000  
    ##  1st Qu.:  451.2   1st Qu.: 139027   1st Qu.:26.20   1st Qu.: 9.875  
    ##  Median :  656.5   Median : 217280   Median :28.10   Median :11.750  
    ##  Mean   : 1041.4   Mean   : 393011   Mean   :28.57   Mean   :12.170  
    ##  3rd Qu.:  946.8   3rd Qu.: 436064   3rd Qu.:30.02   3rd Qu.:13.625  
    ##  Max.   :20062.0   Max.   :8863164   Max.   :49.70   Max.   :33.800  
    ##                                                                      
    ##       docs              beds             crimes           hsgrad     
    ##  Min.   :   39.0   Min.   :   92.0   Min.   :   563   Min.   :46.60  
    ##  1st Qu.:  182.8   1st Qu.:  390.8   1st Qu.:  6220   1st Qu.:73.88  
    ##  Median :  401.0   Median :  755.0   Median : 11820   Median :77.70  
    ##  Mean   :  988.0   Mean   : 1458.6   Mean   : 27112   Mean   :77.56  
    ##  3rd Qu.: 1036.0   3rd Qu.: 1575.8   3rd Qu.: 26280   3rd Qu.:82.40  
    ##  Max.   :23677.0   Max.   :27700.0   Max.   :688936   Max.   :92.90  
    ##                                                                      
    ##      bagrad         poverty           unemp           pcincome    
    ##  Min.   : 8.10   Min.   : 1.400   Min.   : 2.200   Min.   : 8899  
    ##  1st Qu.:15.28   1st Qu.: 5.300   1st Qu.: 5.100   1st Qu.:16118  
    ##  Median :19.70   Median : 7.900   Median : 6.200   Median :17759  
    ##  Mean   :21.08   Mean   : 8.721   Mean   : 6.597   Mean   :18561  
    ##  3rd Qu.:25.32   3rd Qu.:10.900   3rd Qu.: 7.500   3rd Qu.:20270  
    ##  Max.   :52.30   Max.   :36.300   Max.   :21.300   Max.   :37541  
    ##                                                                   
    ##     totalinc                region   
    ##  Min.   :  1141   Northeast    :103  
    ##  1st Qu.:  2311   North_Central:108  
    ##  Median :  3857   South        :152  
    ##  Mean   :  7869   West         : 77  
    ##  3rd Qu.:  8654                      
    ##  Max.   :184230                      
    ## 

``` r
sapply(cdi, function(x) sum(is.na(x)))
```

    ## CRM_1000       id      cty    state     area      pop    pop18    pop65 
    ##        0        0        0        0        0        0        0        0 
    ##     docs     beds   crimes   hsgrad   bagrad  poverty    unemp pcincome 
    ##        0        0        0        0        0        0        0        0 
    ## totalinc   region 
    ##        0        0

No missing values were found.

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

``` r
cdi %>% 
  ggplot(aes(x = "", y = CRM_1000))+
  geom_boxplot() + 
  labs(title = "Counties' CRM_1000 Values",)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

    ## # A tibble: 48 x 4
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
    ## # ... with 38 more rows

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
cdi_state %>% 
  ggplot(aes(x = "", y = state_CRM_1000))+
  geom_boxplot() + 
  labs(title = "States' CRM_1000 Values",)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Surprisingly, if we look at the CRM\_1000 at state level, no outlier was
found.

## Check for transformation

``` r
# fit multivariate model
mult.fit1 = lm(CRM_1000 ~ poverty + region + state + area + pop + pop18 + pop65 + hsgrad + bagrad + unemp + pcincome + totalinc, data = cdi) 

# check diagnostics
plot(mult.fit1)
```

    ## Warning: not plotting observations with leverage one:
    ##   73, 232, 233, 339, 356, 388, 429

![](data_exploration_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](data_exploration_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](data_exploration_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](data_exploration_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
boxcox(mult.fit1)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

## a = 1/2. Perform sqrt transformation.

``` r
cdi = 
  cdi %>% 
  mutate(sqrt_CRM_1000 = sqrt(CRM_1000))

mult.fit2 = lm(sqrt_CRM_1000 ~ poverty + region + state + area + pop + pop18 + pop65 + hsgrad + bagrad + unemp + pcincome + totalinc, data = cdi) 

# check diagnostics
summary(mult.fit2)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ poverty + region + state + area + 
    ##     pop + pop18 + pop65 + hsgrad + bagrad + unemp + pcincome + 
    ##     totalinc, data = cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7879 -0.5955  0.0000  0.6568  5.1472 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -5.089e+00  2.318e+00  -2.196  0.02872 *  
    ## poverty              2.003e-01  2.515e-02   7.964 1.93e-14 ***
    ## regionNorth_Central  9.644e-01  1.193e+00   0.809  0.41930    
    ## regionSouth          1.281e+00  1.221e+00   1.049  0.29502    
    ## regionWest           1.936e+00  1.201e+00   1.612  0.10777    
    ## stateAR              1.722e+00  9.080e-01   1.897  0.05865 .  
    ## stateAZ              2.034e-01  6.875e-01   0.296  0.76749    
    ## stateCA             -8.261e-01  4.383e-01  -1.885  0.06023 .  
    ## stateCO              1.019e-01  5.266e-01   0.194  0.84664    
    ## stateCT             -5.296e-01  1.213e+00  -0.436  0.66273    
    ## stateDC              6.494e-01  1.231e+00   0.527  0.59820    
    ## stateDE              1.546e+00  1.392e+00   1.111  0.26731    
    ## stateFL              8.421e-01  5.195e-01   1.621  0.10584    
    ## stateGA              1.073e+00  5.763e-01   1.862  0.06340 .  
    ## stateHI              3.911e-01  7.695e-01   0.508  0.61154    
    ## stateID             -5.659e-01  1.189e+00  -0.476  0.63428    
    ## stateIL             -1.370e-01  4.447e-01  -0.308  0.75823    
    ## stateIN             -7.794e-01  4.596e-01  -1.696  0.09072 .  
    ## stateKS              1.803e+00  6.657e-01   2.709  0.00705 ** 
    ## stateKY             -4.483e-01  7.832e-01  -0.572  0.56740    
    ## stateLA             -2.425e-01  5.856e-01  -0.414  0.67906    
    ## stateMA             -6.593e-01  1.201e+00  -0.549  0.58333    
    ## stateMD             -8.697e-02  5.783e-01  -0.150  0.88054    
    ## stateME              6.367e-01  1.248e+00   0.510  0.61012    
    ## stateMI              2.620e-01  4.540e-01   0.577  0.56423    
    ## stateMN              6.923e-04  5.550e-01   0.001  0.99901    
    ## stateMO              5.475e-01  5.292e-01   1.035  0.30149    
    ## stateMS              5.294e-02  7.897e-01   0.067  0.94659    
    ## stateMT             -1.424e+00  1.189e+00  -1.197  0.23201    
    ## stateNC              3.934e-01  5.129e-01   0.767  0.44359    
    ## stateND             -7.976e-01  1.194e+00  -0.668  0.50456    
    ## stateNE              6.944e-01  7.492e-01   0.927  0.35461    
    ## stateNH             -7.872e-02  1.272e+00  -0.062  0.95068    
    ## stateNJ              3.601e-01  1.182e+00   0.305  0.76077    
    ## stateNM              2.491e-01  8.947e-01   0.278  0.78084    
    ## stateNV              6.295e-01  9.371e-01   0.672  0.50214    
    ## stateNY              4.634e-01  1.166e+00   0.397  0.69123    
    ## stateOH             -9.957e-01  4.140e-01  -2.405  0.01665 *  
    ## stateOK              9.407e-01  7.215e-01   1.304  0.19306    
    ## stateOR              1.552e-01  5.876e-01   0.264  0.79178    
    ## statePA             -7.065e-01  1.164e+00  -0.607  0.54406    
    ## stateRI              7.092e-01  1.318e+00   0.538  0.59082    
    ## stateSC              1.065e+00  5.506e-01   1.934  0.05389 .  
    ## stateSD              8.745e-02  1.186e+00   0.074  0.94125    
    ## stateTN              2.359e-01  5.877e-01   0.401  0.68836    
    ## stateTX              5.426e-01  4.834e-01   1.122  0.26238    
    ## stateUT              2.055e-01  6.876e-01   0.299  0.76516    
    ## stateVA             -8.078e-02  5.925e-01  -0.136  0.89162    
    ## stateVT                     NA         NA      NA       NA    
    ## stateWA                     NA         NA      NA       NA    
    ## stateWI                     NA         NA      NA       NA    
    ## stateWV             -6.430e-01  1.213e+00  -0.530  0.59628    
    ## area                -1.313e-04  5.024e-05  -2.613  0.00934 ** 
    ## pop                  6.570e-06  8.134e-07   8.078 8.72e-15 ***
    ## pop18                1.266e-01  2.284e-02   5.541 5.62e-08 ***
    ## pop65                5.179e-02  2.266e-02   2.286  0.02281 *  
    ## hsgrad               9.689e-03  1.995e-02   0.486  0.62753    
    ## bagrad              -5.122e-02  1.950e-02  -2.626  0.00898 ** 
    ## unemp               -1.537e-03  4.382e-02  -0.035  0.97204    
    ## pcincome             3.019e-04  3.759e-05   8.032 1.20e-14 ***
    ## totalinc            -2.944e-04  3.923e-05  -7.504 4.40e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.13 on 382 degrees of freedom
    ## Multiple R-squared:  0.6318, Adjusted R-squared:  0.5768 
    ## F-statistic:  11.5 on 57 and 382 DF,  p-value: < 2.2e-16

``` r
plot(mult.fit2)
```

    ## Warning: not plotting observations with leverage one:
    ##   73, 232, 233, 339, 356, 388, 429

![](data_exploration_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](data_exploration_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->![](data_exploration_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->![](data_exploration_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
boxcox(mult.fit2) 
```

![](data_exploration_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->

## Initial Exploration of correlation between counties’ CRM\_100 and all variables, With untransformed outcome (CRM\_1000)

## 好像除了poverty, region和state之外都看不出什么明显关联（捂脸）

``` r
cdi %>% ggplot(aes(x = area, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pop, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pop18, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pop65, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = docs, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = beds, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-6.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = hsgrad, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-7.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = bagrad, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-8.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = poverty, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-9.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = unemp, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-10.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = pcincome, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-11.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = totalinc, y = CRM_1000)) + geom_point()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-12.png)<!-- -->

``` r
cdi %>% ggplot(aes(x = region, y = CRM_1000)) + geom_boxplot()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-13.png)<!-- -->

``` r
cdi %>% ggplot(aes(y = state, x = CRM_1000)) + geom_boxplot()
```

![](data_exploration_files/figure-gfm/unnamed-chunk-10-14.png)<!-- -->
