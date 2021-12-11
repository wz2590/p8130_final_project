data\_exploration
================
Weiheng Zhang
2021/11/28

``` r
library(tidyverse)
library(lubridate)
library(dplyr)
library(leaflet)
library(corrplot)
library(MASS)
library(performance)
library(leaps)


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

## Boxplot for each variable

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

## a is close to 1/2. Perform sqrt transformation.

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

summary(mult.fit2) ##region returned NA, why?
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
corrplot(cor(cdi_cor), type = "upper", diag = FALSE)
```

![](data_exploration_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
corrplot(cor(cdi_cor), 
         method = "color", 
         type = "upper", ## order = "hclust",
         addCoef.col = "black", 
         number.cex = 0.6,
         diag = FALSE) 
```

![](data_exploration_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

## Model Selection

Fit a linear model after removing highly correlated variables.

``` r
new_cdi =
  cdi_sqrt %>% 
  mutate(
    state = as.factor(state),
    region = as.factor(region)) %>% 
  dplyr::select(-hsgrad, -pcincome)

mult.fit = lm(sqrt_CRM_1000 ~ ., data = new_cdi)
summary(mult.fit)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ ., data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4527 -0.6385  0.0000  0.6682  3.5922 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.137e+00  8.987e-01   4.604 5.66e-06 ***
    ## stateAR              9.485e-01  8.765e-01   1.082 0.279868    
    ## stateAZ              6.777e-01  6.592e-01   1.028 0.304594    
    ## stateCA              2.242e-01  4.877e-01   0.460 0.646001    
    ## stateCO              8.740e-01  5.571e-01   1.569 0.117523    
    ## stateCT             -1.196e+00  5.995e-01  -1.995 0.046756 *  
    ## stateDC             -3.096e-01  1.230e+00  -0.252 0.801327    
    ## stateDE              5.506e-01  8.853e-01   0.622 0.534374    
    ## stateFL              1.260e+00  5.006e-01   2.517 0.012241 *  
    ## stateGA              1.197e+00  5.519e-01   2.169 0.030734 *  
    ## stateHI              1.274e+00  7.678e-01   1.659 0.097923 .  
    ## stateID              1.275e-01  1.167e+00   0.109 0.913063    
    ## stateIL             -5.103e-01  5.027e-01  -1.015 0.310690    
    ## stateIN             -1.134e+00  5.113e-01  -2.218 0.027110 *  
    ## stateKS              1.539e+00  6.871e-01   2.240 0.025638 *  
    ## stateKY             -6.556e-01  7.551e-01  -0.868 0.385812    
    ## stateLA             -5.721e-02  5.571e-01  -0.103 0.918264    
    ## stateMA             -1.677e+00  5.914e-01  -2.835 0.004824 ** 
    ## stateMD              2.957e-01  5.684e-01   0.520 0.603276    
    ## stateME             -9.391e-01  6.516e-01  -1.441 0.150328    
    ## stateMI              2.508e-01  5.093e-01   0.492 0.622654    
    ## stateMN             -9.394e-01  6.004e-01  -1.565 0.118500    
    ## stateMO             -4.890e-01  5.793e-01  -0.844 0.399165    
    ## stateMS             -2.364e-01  7.529e-01  -0.314 0.753717    
    ## stateMT             -1.505e+00  1.164e+00  -1.293 0.196830    
    ## stateNC              5.202e-01  5.000e-01   1.040 0.298802    
    ## stateND             -2.194e+00  1.171e+00  -1.874 0.061658 .  
    ## stateNE             -1.835e-02  7.598e-01  -0.024 0.980740    
    ## stateNH             -1.176e+00  7.093e-01  -1.659 0.098033 .  
    ## stateNJ             -4.030e-01  5.175e-01  -0.779 0.436638    
    ## stateNM              1.109e+00  8.867e-01   1.251 0.211872    
    ## stateNV              1.418e+00  8.816e-01   1.608 0.108595    
    ## stateNY             -1.063e+00  4.964e-01  -2.141 0.032908 *  
    ## stateOH             -1.096e+00  4.786e-01  -2.290 0.022590 *  
    ## stateOK              9.119e-01  6.838e-01   1.334 0.183102    
    ## stateOR              8.431e-01  6.199e-01   1.360 0.174622    
    ## statePA             -1.899e+00  4.818e-01  -3.943 9.58e-05 ***
    ## stateRI             -2.728e-01  7.842e-01  -0.348 0.728168    
    ## stateSC              1.121e+00  5.312e-01   2.111 0.035388 *  
    ## stateSD             -1.149e+00  1.177e+00  -0.976 0.329585    
    ## stateTN              1.538e-01  5.658e-01   0.272 0.785930    
    ## stateTX              1.068e+00  4.671e-01   2.287 0.022729 *  
    ## stateUT              2.454e-01  6.914e-01   0.355 0.722806    
    ## stateVA              9.406e-02  5.802e-01   0.162 0.871304    
    ## stateVT             -1.574e+00  1.186e+00  -1.327 0.185315    
    ## stateWA              7.348e-01  5.508e-01   1.334 0.182996    
    ## stateWI             -1.620e-01  5.421e-01  -0.299 0.765233    
    ## stateWV             -9.646e-01  1.166e+00  -0.827 0.408654    
    ## pop18                4.235e-02  1.930e-02   2.194 0.028837 *  
    ## pop65               -1.831e-02  2.355e-02  -0.777 0.437533    
    ## bagrad               8.222e-03  1.251e-02   0.657 0.511548    
    ## poverty              6.033e-02  2.142e-02   2.816 0.005113 ** 
    ## unemp                4.905e-02  4.162e-02   1.178 0.239345    
    ## totalinc             1.864e-05  4.616e-06   4.039 6.50e-05 ***
    ## regionNorth_Central         NA         NA      NA       NA    
    ## regionSouth                 NA         NA      NA       NA    
    ## regionWest                  NA         NA      NA       NA    
    ## pop_den              1.660e-04  4.317e-05   3.846 0.000141 ***
    ## pdocs               -3.028e+00  6.891e+01  -0.044 0.964975    
    ## pbeds                2.748e+02  5.583e+01   4.922 1.27e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.087 on 382 degrees of freedom
    ## Multiple R-squared:  0.6327, Adjusted R-squared:  0.5789 
    ## F-statistic: 11.75 on 56 and 382 DF,  p-value: < 2.2e-16

``` r
#不确定是否加interaction
#mult.fit_inter = lm(sqrt_CRM_1000 ~ state + pop18 + grad + pcincome + hsgrad + pop65 + bagrad + #poverty + unemp + totalinc + region + pop_den + pbeds + hsgrad*bagrad + hsgrad*poverty + #pop18*pop65 + bagrad*pcincome + pbeds*pdocs, data = new_cdi)
#summary(mult.fit_new)
```

Backward Elimination

``` r
# No region, becasue the original mulitiple linear fit, the region has NA
step1 = update(mult.fit, . ~ . -region)
summary(step1)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + 
    ##     poverty + unemp + totalinc + pop_den + pdocs + pbeds, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4527 -0.6385  0.0000  0.6682  3.5922 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.137e+00  8.987e-01   4.604 5.66e-06 ***
    ## stateAR      9.485e-01  8.765e-01   1.082 0.279868    
    ## stateAZ      6.777e-01  6.592e-01   1.028 0.304594    
    ## stateCA      2.242e-01  4.877e-01   0.460 0.646001    
    ## stateCO      8.740e-01  5.571e-01   1.569 0.117523    
    ## stateCT     -1.196e+00  5.995e-01  -1.995 0.046756 *  
    ## stateDC     -3.096e-01  1.230e+00  -0.252 0.801327    
    ## stateDE      5.506e-01  8.853e-01   0.622 0.534374    
    ## stateFL      1.260e+00  5.006e-01   2.517 0.012241 *  
    ## stateGA      1.197e+00  5.519e-01   2.169 0.030734 *  
    ## stateHI      1.274e+00  7.678e-01   1.659 0.097923 .  
    ## stateID      1.275e-01  1.167e+00   0.109 0.913063    
    ## stateIL     -5.103e-01  5.027e-01  -1.015 0.310690    
    ## stateIN     -1.134e+00  5.113e-01  -2.218 0.027110 *  
    ## stateKS      1.539e+00  6.871e-01   2.240 0.025638 *  
    ## stateKY     -6.556e-01  7.551e-01  -0.868 0.385812    
    ## stateLA     -5.721e-02  5.571e-01  -0.103 0.918264    
    ## stateMA     -1.677e+00  5.914e-01  -2.835 0.004824 ** 
    ## stateMD      2.957e-01  5.684e-01   0.520 0.603276    
    ## stateME     -9.391e-01  6.516e-01  -1.441 0.150328    
    ## stateMI      2.508e-01  5.093e-01   0.492 0.622654    
    ## stateMN     -9.394e-01  6.004e-01  -1.565 0.118500    
    ## stateMO     -4.890e-01  5.793e-01  -0.844 0.399165    
    ## stateMS     -2.364e-01  7.529e-01  -0.314 0.753717    
    ## stateMT     -1.505e+00  1.164e+00  -1.293 0.196830    
    ## stateNC      5.202e-01  5.000e-01   1.040 0.298802    
    ## stateND     -2.194e+00  1.171e+00  -1.874 0.061658 .  
    ## stateNE     -1.835e-02  7.598e-01  -0.024 0.980740    
    ## stateNH     -1.176e+00  7.093e-01  -1.659 0.098033 .  
    ## stateNJ     -4.030e-01  5.175e-01  -0.779 0.436638    
    ## stateNM      1.109e+00  8.867e-01   1.251 0.211872    
    ## stateNV      1.418e+00  8.816e-01   1.608 0.108595    
    ## stateNY     -1.063e+00  4.964e-01  -2.141 0.032908 *  
    ## stateOH     -1.096e+00  4.786e-01  -2.290 0.022590 *  
    ## stateOK      9.119e-01  6.838e-01   1.334 0.183102    
    ## stateOR      8.431e-01  6.199e-01   1.360 0.174622    
    ## statePA     -1.899e+00  4.818e-01  -3.943 9.58e-05 ***
    ## stateRI     -2.728e-01  7.842e-01  -0.348 0.728168    
    ## stateSC      1.121e+00  5.312e-01   2.111 0.035388 *  
    ## stateSD     -1.149e+00  1.177e+00  -0.976 0.329585    
    ## stateTN      1.538e-01  5.658e-01   0.272 0.785930    
    ## stateTX      1.068e+00  4.671e-01   2.287 0.022729 *  
    ## stateUT      2.454e-01  6.914e-01   0.355 0.722806    
    ## stateVA      9.406e-02  5.802e-01   0.162 0.871304    
    ## stateVT     -1.574e+00  1.186e+00  -1.327 0.185315    
    ## stateWA      7.348e-01  5.508e-01   1.334 0.182996    
    ## stateWI     -1.620e-01  5.421e-01  -0.299 0.765233    
    ## stateWV     -9.646e-01  1.166e+00  -0.827 0.408654    
    ## pop18        4.235e-02  1.930e-02   2.194 0.028837 *  
    ## pop65       -1.831e-02  2.355e-02  -0.777 0.437533    
    ## bagrad       8.222e-03  1.251e-02   0.657 0.511548    
    ## poverty      6.033e-02  2.142e-02   2.816 0.005113 ** 
    ## unemp        4.905e-02  4.162e-02   1.178 0.239345    
    ## totalinc     1.864e-05  4.616e-06   4.039 6.50e-05 ***
    ## pop_den      1.660e-04  4.317e-05   3.846 0.000141 ***
    ## pdocs       -3.028e+00  6.891e+01  -0.044 0.964975    
    ## pbeds        2.748e+02  5.583e+01   4.922 1.27e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.087 on 382 degrees of freedom
    ## Multiple R-squared:  0.6327, Adjusted R-squared:  0.5789 
    ## F-statistic: 11.75 on 56 and 382 DF,  p-value: < 2.2e-16

``` r
# No state 
step2 = update(step1, . ~ . -state)
summary(step2)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + pop65 + bagrad + poverty + 
    ##     unemp + totalinc + pop_den + pdocs + pbeds, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6159 -0.8039  0.0396  0.9258  3.8838 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.385e+00  8.501e-01   5.158 3.82e-07 ***
    ## pop18        2.605e-02  2.182e-02   1.194 0.233205    
    ## pop65       -7.860e-03  2.264e-02  -0.347 0.728703    
    ## bagrad       2.274e-02  1.385e-02   1.642 0.101258    
    ## poverty      1.730e-01  1.906e-02   9.074  < 2e-16 ***
    ## unemp       -6.362e-02  3.672e-02  -1.732 0.083923 .  
    ## totalinc     1.971e-05  5.487e-06   3.592 0.000366 ***
    ## pop_den      1.044e-04  4.780e-05   2.184 0.029487 *  
    ## pdocs        3.669e+01  7.617e+01   0.482 0.630295    
    ## pbeds        1.178e+02  5.644e+01   2.087 0.037441 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.357 on 429 degrees of freedom
    ## Multiple R-squared:  0.3567, Adjusted R-squared:  0.3432 
    ## F-statistic: 26.43 on 9 and 429 DF,  p-value: < 2.2e-16

``` r
# No pop65
step3 = update(step2, . ~ . -pop65)
summary(step3)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + bagrad + poverty + unemp + 
    ##     totalinc + pop_den + pdocs + pbeds, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6131 -0.7909  0.0520  0.9298  3.8753 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.184e+00  6.220e-01   6.727 5.56e-11 ***
    ## pop18        3.016e-02  1.832e-02   1.646 0.100545    
    ## bagrad       2.311e-02  1.379e-02   1.676 0.094399 .  
    ## poverty      1.743e-01  1.863e-02   9.359  < 2e-16 ***
    ## unemp       -6.548e-02  3.629e-02  -1.804 0.071900 .  
    ## totalinc     1.969e-05  5.481e-06   3.593 0.000365 ***
    ## pop_den      1.026e-04  4.748e-05   2.162 0.031189 *  
    ## pdocs        3.730e+01  7.608e+01   0.490 0.624157    
    ## pbeds        1.125e+02  5.428e+01   2.073 0.038777 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.356 on 430 degrees of freedom
    ## Multiple R-squared:  0.3565, Adjusted R-squared:  0.3445 
    ## F-statistic: 29.78 on 8 and 430 DF,  p-value: < 2.2e-16

``` r
# No pdocs
step4 = update(step3, . ~ . -pdocs)
summary(step4)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ pop18 + bagrad + poverty + unemp + 
    ##     totalinc + pop_den + pbeds, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6326 -0.7917  0.0413  0.9286  3.8593 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.112e+00  6.037e-01   6.811 3.28e-11 ***
    ## pop18        3.024e-02  1.831e-02   1.652 0.099315 .  
    ## bagrad       2.640e-02  1.204e-02   2.192 0.028894 *  
    ## poverty      1.739e-01  1.859e-02   9.354  < 2e-16 ***
    ## unemp       -6.418e-02  3.616e-02  -1.775 0.076674 .  
    ## totalinc     1.993e-05  5.454e-06   3.654 0.000290 ***
    ## pop_den      1.074e-04  4.646e-05   2.311 0.021316 *  
    ## pbeds        1.315e+02  3.792e+01   3.469 0.000575 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.355 on 431 degrees of freedom
    ## Multiple R-squared:  0.3562, Adjusted R-squared:  0.3457 
    ## F-statistic: 34.06 on 7 and 431 DF,  p-value: < 2.2e-16

``` r
# No pop18
step5 = update(step4, . ~ . -pop18)
summary(step5)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ bagrad + poverty + unemp + totalinc + 
    ##     pop_den + pbeds, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7190 -0.8136  0.0372  0.9238  3.9539 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.805e+00  4.346e-01  11.056  < 2e-16 ***
    ## bagrad       3.458e-02  1.100e-02   3.145 0.001777 ** 
    ## poverty      1.832e-01  1.775e-02  10.320  < 2e-16 ***
    ## unemp       -7.320e-02  3.582e-02  -2.043 0.041612 *  
    ## totalinc     1.940e-05  5.456e-06   3.556 0.000418 ***
    ## pop_den      1.134e-04  4.641e-05   2.443 0.014962 *  
    ## pbeds        1.248e+02  3.777e+01   3.304 0.001032 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.357 on 432 degrees of freedom
    ## Multiple R-squared:  0.3521, Adjusted R-squared:  0.3431 
    ## F-statistic: 39.13 on 6 and 432 DF,  p-value: < 2.2e-16

``` r
multi_fit_back1 = lm(sqrt_CRM_1000 ~ bagrad + poverty + unemp + totalinc + 
    pop_den + pbeds, data = new_cdi)
summary(multi_fit_back1)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ bagrad + poverty + unemp + totalinc + 
    ##     pop_den + pbeds, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7190 -0.8136  0.0372  0.9238  3.9539 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.805e+00  4.346e-01  11.056  < 2e-16 ***
    ## bagrad       3.458e-02  1.100e-02   3.145 0.001777 ** 
    ## poverty      1.832e-01  1.775e-02  10.320  < 2e-16 ***
    ## unemp       -7.320e-02  3.582e-02  -2.043 0.041612 *  
    ## totalinc     1.940e-05  5.456e-06   3.556 0.000418 ***
    ## pop_den      1.134e-04  4.641e-05   2.443 0.014962 *  
    ## pbeds        1.248e+02  3.777e+01   3.304 0.001032 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.357 on 432 degrees of freedom
    ## Multiple R-squared:  0.3521, Adjusted R-squared:  0.3431 
    ## F-statistic: 39.13 on 6 and 432 DF,  p-value: < 2.2e-16

``` r
# multi_fit1: adjusted r squared is 0.3431, 

# just use one function for backward 
step(mult.fit, direction = 'backward')
```

    ## Start:  AIC=126.05
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + poverty + unemp + 
    ##     totalinc + region + pop_den + pdocs + pbeds
    ## 
    ## 
    ## Step:  AIC=126.05
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + poverty + unemp + 
    ##     totalinc + pop_den + pdocs + pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - pdocs     1      0.00 451.22 124.06
    ## - bagrad    1      0.51 451.73 124.55
    ## - pop65     1      0.71 451.93 124.75
    ## - unemp     1      1.64 452.86 125.65
    ## <none>                  451.22 126.05
    ## - pop18     1      5.69 456.91 129.55
    ## - poverty   1      9.37 460.59 133.07
    ## - pop_den   1     17.47 468.69 140.73
    ## - totalinc  1     19.27 470.49 142.41
    ## - pbeds     1     28.62 479.84 151.05
    ## - state    47    339.16 790.38 278.14
    ## 
    ## Step:  AIC=124.06
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + poverty + unemp + 
    ##     totalinc + pop_den + pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - bagrad    1      0.60 451.83 122.64
    ## - pop65     1      0.71 451.94 122.75
    ## - unemp     1      1.64 452.86 123.65
    ## <none>                  451.22 124.06
    ## - pop18     1      5.71 456.93 127.58
    ## - poverty   1      9.37 460.59 131.08
    ## - pop_den   1     18.09 469.31 139.31
    ## - totalinc  1     19.32 470.54 140.46
    ## - pbeds     1     59.24 510.46 176.21
    ## - state    47    339.59 790.81 276.38
    ## 
    ## Step:  AIC=122.64
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + poverty + unemp + totalinc + 
    ##     pop_den + pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - pop65     1      0.89 452.71 121.50
    ## - unemp     1      1.13 452.96 121.74
    ## <none>                  451.83 122.64
    ## - pop18     1      7.08 458.91 127.47
    ## - poverty   1      8.77 460.59 129.08
    ## - pop_den   1     19.46 471.28 139.15
    ## - totalinc  1     20.19 472.01 139.83
    ## - pbeds     1     61.06 512.89 176.29
    ## - state    47    347.42 799.25 279.04
    ## 
    ## Step:  AIC=121.5
    ## sqrt_CRM_1000 ~ state + pop18 + poverty + unemp + totalinc + 
    ##     pop_den + pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - unemp     1      1.15 453.86 120.61
    ## <none>                  452.71 121.50
    ## - poverty   1      8.67 461.38 127.83
    ## - pop18     1     16.30 469.02 135.03
    ## - pop_den   1     18.92 471.63 137.48
    ## - totalinc  1     20.65 473.37 139.09
    ## - pbeds     1     63.86 516.57 177.43
    ## - state    47    347.15 799.87 277.38
    ## 
    ## Step:  AIC=120.61
    ## sqrt_CRM_1000 ~ state + pop18 + poverty + totalinc + pop_den + 
    ##     pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  453.86 120.61
    ## - pop18     1     15.28 469.14 133.15
    ## - pop_den   1     18.51 472.37 136.17
    ## - totalinc  1     19.74 473.60 137.30
    ## - poverty   1     24.75 478.61 141.93
    ## - pbeds     1     64.62 518.48 177.05
    ## - state    47    359.28 813.14 282.60

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ state + pop18 + poverty + totalinc + 
    ##     pop_den + pbeds, data = new_cdi)
    ## 
    ## Coefficients:
    ## (Intercept)      stateAR      stateAZ      stateCA      stateCO      stateCT  
    ##   4.141e+00    9.456e-01    6.368e-01    3.493e-01    8.819e-01   -1.115e+00  
    ##     stateDC      stateDE      stateFL      stateGA      stateHI      stateID  
    ##  -1.524e-01    5.944e-01    1.204e+00    1.192e+00    1.146e+00    1.110e-01  
    ##     stateIL      stateIN      stateKS      stateKY      stateLA      stateMA  
    ##  -4.324e-01   -1.161e+00    1.550e+00   -6.464e-01   -5.486e-02   -1.507e+00  
    ##     stateMD      stateME      stateMI      stateMN      stateMO      stateMS  
    ##   3.714e-01   -8.683e-01    3.847e-01   -8.946e-01   -4.071e-01   -2.167e-01  
    ##     stateMT      stateNC      stateND      stateNE      stateNH      stateNJ  
    ##  -1.494e+00    4.671e-01   -2.260e+00   -7.235e-02   -1.042e+00   -3.128e-01  
    ##     stateNM      stateNV      stateNY      stateOH      stateOK      stateOR  
    ##   1.093e+00    1.395e+00   -1.022e+00   -1.102e+00    9.323e-01    8.248e-01  
    ##     statePA      stateRI      stateSC      stateSD      stateTN      stateTX  
    ##  -1.907e+00   -1.874e-01    1.084e+00   -1.210e+00    1.430e-01    1.099e+00  
    ##     stateUT      stateVA      stateVT      stateWA      stateWI      stateWV  
    ##   2.572e-01    1.614e-01   -1.489e+00    7.641e-01   -1.836e-01   -9.234e-01  
    ##       pop18      poverty     totalinc      pop_den        pbeds  
    ##   5.038e-02    6.994e-02    1.856e-05    1.651e-04    2.536e+02

``` r
multi_fit_back2 = lm(sqrt_CRM_1000 ~ state + pop18 + poverty + totalinc + 
    pop_den + pbeds, data = new_cdi)

summary(multi_fit_back2)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ state + pop18 + poverty + totalinc + 
    ##     pop_den + pbeds, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5079 -0.6130  0.0000  0.6907  3.5703 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.141e+00  6.109e-01   6.778 4.56e-11 ***
    ## stateAR      9.456e-01  8.737e-01   1.082 0.279803    
    ## stateAZ      6.368e-01  6.438e-01   0.989 0.323205    
    ## stateCA      3.493e-01  4.613e-01   0.757 0.449424    
    ## stateCO      8.819e-01  5.525e-01   1.596 0.111227    
    ## stateCT     -1.115e+00  5.752e-01  -1.939 0.053206 .  
    ## stateDC     -1.524e-01  1.217e+00  -0.125 0.900439    
    ## stateDE      5.944e-01  8.751e-01   0.679 0.497408    
    ## stateFL      1.204e+00  4.644e-01   2.593 0.009871 ** 
    ## stateGA      1.192e+00  5.486e-01   2.173 0.030374 *  
    ## stateHI      1.146e+00  7.550e-01   1.518 0.129718    
    ## stateID      1.110e-01  1.163e+00   0.095 0.924025    
    ## stateIL     -4.324e-01  4.935e-01  -0.876 0.381442    
    ## stateIN     -1.161e+00  5.067e-01  -2.291 0.022523 *  
    ## stateKS      1.550e+00  6.833e-01   2.268 0.023860 *  
    ## stateKY     -6.464e-01  7.519e-01  -0.860 0.390544    
    ## stateLA     -5.486e-02  5.504e-01  -0.100 0.920661    
    ## stateMA     -1.507e+00  5.358e-01  -2.812 0.005174 ** 
    ## stateMD      3.714e-01  5.539e-01   0.670 0.502968    
    ## stateME     -8.683e-01  6.395e-01  -1.358 0.175320    
    ## stateMI      3.847e-01  4.870e-01   0.790 0.430111    
    ## stateMN     -8.946e-01  5.884e-01  -1.521 0.129199    
    ## stateMO     -4.071e-01  5.707e-01  -0.713 0.476074    
    ## stateMS     -2.167e-01  7.494e-01  -0.289 0.772566    
    ## stateMT     -1.494e+00  1.161e+00  -1.286 0.199077    
    ## stateNC      4.671e-01  4.875e-01   0.958 0.338637    
    ## stateND     -2.260e+00  1.167e+00  -1.937 0.053468 .  
    ## stateNE     -7.235e-02  7.551e-01  -0.096 0.923724    
    ## stateNH     -1.042e+00  6.912e-01  -1.508 0.132295    
    ## stateNJ     -3.128e-01  5.036e-01  -0.621 0.534834    
    ## stateNM      1.093e+00  8.764e-01   1.247 0.212994    
    ## stateNV      1.395e+00  8.733e-01   1.597 0.110974    
    ## stateNY     -1.022e+00  4.817e-01  -2.122 0.034448 *  
    ## stateOH     -1.102e+00  4.717e-01  -2.336 0.020013 *  
    ## stateOK      9.323e-01  6.817e-01   1.368 0.172209    
    ## stateOR      8.248e-01  6.113e-01   1.349 0.178055    
    ## statePA     -1.907e+00  4.653e-01  -4.098 5.08e-05 ***
    ## stateRI     -1.874e-01  7.570e-01  -0.248 0.804640    
    ## stateSC      1.084e+00  5.263e-01   2.060 0.040064 *  
    ## stateSD     -1.210e+00  1.170e+00  -1.034 0.301758    
    ## stateTN      1.430e-01  5.625e-01   0.254 0.799477    
    ## stateTX      1.099e+00  4.641e-01   2.369 0.018336 *  
    ## stateUT      2.572e-01  6.858e-01   0.375 0.707852    
    ## stateVA      1.614e-01  5.748e-01   0.281 0.779049    
    ## stateVT     -1.489e+00  1.168e+00  -1.274 0.203391    
    ## stateWA      7.641e-01  5.428e-01   1.408 0.160060    
    ## stateWI     -1.836e-01  5.330e-01  -0.345 0.730626    
    ## stateWV     -9.234e-01  1.162e+00  -0.794 0.427434    
    ## pop18        5.038e-02  1.397e-02   3.605 0.000353 ***
    ## poverty      6.994e-02  1.524e-02   4.588 6.06e-06 ***
    ## totalinc     1.856e-05  4.530e-06   4.097 5.10e-05 ***
    ## pop_den      1.651e-04  4.161e-05   3.968 8.64e-05 ***
    ## pbeds        2.536e+02  3.421e+01   7.413 7.88e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.084 on 386 degrees of freedom
    ## Multiple R-squared:  0.6306, Adjusted R-squared:  0.5808 
    ## F-statistic: 12.67 on 52 and 386 DF,  p-value: < 2.2e-16

``` r
# multi_fit2: Adjusted R-squared is 0.5808 and AIC=120.61
```

Forward Selection

``` r
step(mult.fit, direction = 'forward')
```

    ## Start:  AIC=126.05
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + poverty + unemp + 
    ##     totalinc + region + pop_den + pdocs + pbeds

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + 
    ##     poverty + unemp + totalinc + region + pop_den + pdocs + pbeds, 
    ##     data = new_cdi)
    ## 
    ## Coefficients:
    ##         (Intercept)              stateAR              stateAZ  
    ##           4.137e+00            9.485e-01            6.777e-01  
    ##             stateCA              stateCO              stateCT  
    ##           2.242e-01            8.740e-01           -1.196e+00  
    ##             stateDC              stateDE              stateFL  
    ##          -3.096e-01            5.506e-01            1.260e+00  
    ##             stateGA              stateHI              stateID  
    ##           1.197e+00            1.274e+00            1.275e-01  
    ##             stateIL              stateIN              stateKS  
    ##          -5.103e-01           -1.134e+00            1.539e+00  
    ##             stateKY              stateLA              stateMA  
    ##          -6.556e-01           -5.721e-02           -1.677e+00  
    ##             stateMD              stateME              stateMI  
    ##           2.957e-01           -9.391e-01            2.508e-01  
    ##             stateMN              stateMO              stateMS  
    ##          -9.394e-01           -4.890e-01           -2.364e-01  
    ##             stateMT              stateNC              stateND  
    ##          -1.505e+00            5.202e-01           -2.194e+00  
    ##             stateNE              stateNH              stateNJ  
    ##          -1.835e-02           -1.176e+00           -4.030e-01  
    ##             stateNM              stateNV              stateNY  
    ##           1.109e+00            1.418e+00           -1.063e+00  
    ##             stateOH              stateOK              stateOR  
    ##          -1.096e+00            9.119e-01            8.431e-01  
    ##             statePA              stateRI              stateSC  
    ##          -1.899e+00           -2.728e-01            1.121e+00  
    ##             stateSD              stateTN              stateTX  
    ##          -1.149e+00            1.538e-01            1.068e+00  
    ##             stateUT              stateVA              stateVT  
    ##           2.454e-01            9.406e-02           -1.574e+00  
    ##             stateWA              stateWI              stateWV  
    ##           7.348e-01           -1.620e-01           -9.646e-01  
    ##               pop18                pop65               bagrad  
    ##           4.235e-02           -1.831e-02            8.222e-03  
    ##             poverty                unemp             totalinc  
    ##           6.033e-02            4.905e-02            1.864e-05  
    ## regionNorth_Central          regionSouth           regionWest  
    ##                  NA                   NA                   NA  
    ##             pop_den                pdocs                pbeds  
    ##           1.660e-04           -3.028e+00            2.748e+02

``` r
multi_fit_forward = lm(sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + poverty + unemp + totalinc + region + pop_den + pdocs + pbeds, data = new_cdi)
summary(multi_fit_forward)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + 
    ##     poverty + unemp + totalinc + region + pop_den + pdocs + pbeds, 
    ##     data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4527 -0.6385  0.0000  0.6682  3.5922 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.137e+00  8.987e-01   4.604 5.66e-06 ***
    ## stateAR              9.485e-01  8.765e-01   1.082 0.279868    
    ## stateAZ              6.777e-01  6.592e-01   1.028 0.304594    
    ## stateCA              2.242e-01  4.877e-01   0.460 0.646001    
    ## stateCO              8.740e-01  5.571e-01   1.569 0.117523    
    ## stateCT             -1.196e+00  5.995e-01  -1.995 0.046756 *  
    ## stateDC             -3.096e-01  1.230e+00  -0.252 0.801327    
    ## stateDE              5.506e-01  8.853e-01   0.622 0.534374    
    ## stateFL              1.260e+00  5.006e-01   2.517 0.012241 *  
    ## stateGA              1.197e+00  5.519e-01   2.169 0.030734 *  
    ## stateHI              1.274e+00  7.678e-01   1.659 0.097923 .  
    ## stateID              1.275e-01  1.167e+00   0.109 0.913063    
    ## stateIL             -5.103e-01  5.027e-01  -1.015 0.310690    
    ## stateIN             -1.134e+00  5.113e-01  -2.218 0.027110 *  
    ## stateKS              1.539e+00  6.871e-01   2.240 0.025638 *  
    ## stateKY             -6.556e-01  7.551e-01  -0.868 0.385812    
    ## stateLA             -5.721e-02  5.571e-01  -0.103 0.918264    
    ## stateMA             -1.677e+00  5.914e-01  -2.835 0.004824 ** 
    ## stateMD              2.957e-01  5.684e-01   0.520 0.603276    
    ## stateME             -9.391e-01  6.516e-01  -1.441 0.150328    
    ## stateMI              2.508e-01  5.093e-01   0.492 0.622654    
    ## stateMN             -9.394e-01  6.004e-01  -1.565 0.118500    
    ## stateMO             -4.890e-01  5.793e-01  -0.844 0.399165    
    ## stateMS             -2.364e-01  7.529e-01  -0.314 0.753717    
    ## stateMT             -1.505e+00  1.164e+00  -1.293 0.196830    
    ## stateNC              5.202e-01  5.000e-01   1.040 0.298802    
    ## stateND             -2.194e+00  1.171e+00  -1.874 0.061658 .  
    ## stateNE             -1.835e-02  7.598e-01  -0.024 0.980740    
    ## stateNH             -1.176e+00  7.093e-01  -1.659 0.098033 .  
    ## stateNJ             -4.030e-01  5.175e-01  -0.779 0.436638    
    ## stateNM              1.109e+00  8.867e-01   1.251 0.211872    
    ## stateNV              1.418e+00  8.816e-01   1.608 0.108595    
    ## stateNY             -1.063e+00  4.964e-01  -2.141 0.032908 *  
    ## stateOH             -1.096e+00  4.786e-01  -2.290 0.022590 *  
    ## stateOK              9.119e-01  6.838e-01   1.334 0.183102    
    ## stateOR              8.431e-01  6.199e-01   1.360 0.174622    
    ## statePA             -1.899e+00  4.818e-01  -3.943 9.58e-05 ***
    ## stateRI             -2.728e-01  7.842e-01  -0.348 0.728168    
    ## stateSC              1.121e+00  5.312e-01   2.111 0.035388 *  
    ## stateSD             -1.149e+00  1.177e+00  -0.976 0.329585    
    ## stateTN              1.538e-01  5.658e-01   0.272 0.785930    
    ## stateTX              1.068e+00  4.671e-01   2.287 0.022729 *  
    ## stateUT              2.454e-01  6.914e-01   0.355 0.722806    
    ## stateVA              9.406e-02  5.802e-01   0.162 0.871304    
    ## stateVT             -1.574e+00  1.186e+00  -1.327 0.185315    
    ## stateWA              7.348e-01  5.508e-01   1.334 0.182996    
    ## stateWI             -1.620e-01  5.421e-01  -0.299 0.765233    
    ## stateWV             -9.646e-01  1.166e+00  -0.827 0.408654    
    ## pop18                4.235e-02  1.930e-02   2.194 0.028837 *  
    ## pop65               -1.831e-02  2.355e-02  -0.777 0.437533    
    ## bagrad               8.222e-03  1.251e-02   0.657 0.511548    
    ## poverty              6.033e-02  2.142e-02   2.816 0.005113 ** 
    ## unemp                4.905e-02  4.162e-02   1.178 0.239345    
    ## totalinc             1.864e-05  4.616e-06   4.039 6.50e-05 ***
    ## regionNorth_Central         NA         NA      NA       NA    
    ## regionSouth                 NA         NA      NA       NA    
    ## regionWest                  NA         NA      NA       NA    
    ## pop_den              1.660e-04  4.317e-05   3.846 0.000141 ***
    ## pdocs               -3.028e+00  6.891e+01  -0.044 0.964975    
    ## pbeds                2.748e+02  5.583e+01   4.922 1.27e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.087 on 382 degrees of freedom
    ## Multiple R-squared:  0.6327, Adjusted R-squared:  0.5789 
    ## F-statistic: 11.75 on 56 and 382 DF,  p-value: < 2.2e-16

``` r
# our Adjusted R-squared is 0.5789 and AIC=126.05
```

use both selection

``` r
step(mult.fit, direction = 'both')
```

    ## Start:  AIC=126.05
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + poverty + unemp + 
    ##     totalinc + region + pop_den + pdocs + pbeds
    ## 
    ## 
    ## Step:  AIC=126.05
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + poverty + unemp + 
    ##     totalinc + pop_den + pdocs + pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - pdocs     1      0.00 451.22 124.06
    ## - bagrad    1      0.51 451.73 124.55
    ## - pop65     1      0.71 451.93 124.75
    ## - unemp     1      1.64 452.86 125.65
    ## <none>                  451.22 126.05
    ## - pop18     1      5.69 456.91 129.55
    ## - poverty   1      9.37 460.59 133.07
    ## - pop_den   1     17.47 468.69 140.73
    ## - totalinc  1     19.27 470.49 142.41
    ## - pbeds     1     28.62 479.84 151.05
    ## - state    47    339.16 790.38 278.14
    ## 
    ## Step:  AIC=124.06
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + bagrad + poverty + unemp + 
    ##     totalinc + pop_den + pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - bagrad    1      0.60 451.83 122.64
    ## - pop65     1      0.71 451.94 122.75
    ## - unemp     1      1.64 452.86 123.65
    ## <none>                  451.22 124.06
    ## + pdocs     1      0.00 451.22 126.05
    ## - pop18     1      5.71 456.93 127.58
    ## - poverty   1      9.37 460.59 131.08
    ## - pop_den   1     18.09 469.31 139.31
    ## - totalinc  1     19.32 470.54 140.46
    ## - pbeds     1     59.24 510.46 176.21
    ## - state    47    339.59 790.81 276.38
    ## 
    ## Step:  AIC=122.64
    ## sqrt_CRM_1000 ~ state + pop18 + pop65 + poverty + unemp + totalinc + 
    ##     pop_den + pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - pop65     1      0.89 452.71 121.50
    ## - unemp     1      1.13 452.96 121.74
    ## <none>                  451.83 122.64
    ## + bagrad    1      0.60 451.22 124.06
    ## + pdocs     1      0.10 451.73 124.55
    ## - pop18     1      7.08 458.91 127.47
    ## - poverty   1      8.77 460.59 129.08
    ## - pop_den   1     19.46 471.28 139.15
    ## - totalinc  1     20.19 472.01 139.83
    ## - pbeds     1     61.06 512.89 176.29
    ## - state    47    347.42 799.25 279.04
    ## 
    ## Step:  AIC=121.5
    ## sqrt_CRM_1000 ~ state + pop18 + poverty + unemp + totalinc + 
    ##     pop_den + pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## - unemp     1      1.15 453.86 120.61
    ## <none>                  452.71 121.50
    ## + pop65     1      0.89 451.83 122.64
    ## + bagrad    1      0.78 451.94 122.75
    ## + pdocs     1      0.19 452.52 123.32
    ## - poverty   1      8.67 461.38 127.83
    ## - pop18     1     16.30 469.02 135.03
    ## - pop_den   1     18.92 471.63 137.48
    ## - totalinc  1     20.65 473.37 139.09
    ## - pbeds     1     63.86 516.57 177.43
    ## - state    47    347.15 799.87 277.38
    ## 
    ## Step:  AIC=120.61
    ## sqrt_CRM_1000 ~ state + pop18 + poverty + totalinc + pop_den + 
    ##     pbeds
    ## 
    ##            Df Sum of Sq    RSS    AIC
    ## <none>                  453.86 120.61
    ## + unemp     1      1.15 452.71 121.50
    ## + pop65     1      0.90 452.96 121.74
    ## + bagrad    1      0.17 453.69 122.45
    ## + pdocs     1      0.06 453.80 122.56
    ## - pop18     1     15.28 469.14 133.15
    ## - pop_den   1     18.51 472.37 136.17
    ## - totalinc  1     19.74 473.60 137.30
    ## - poverty   1     24.75 478.61 141.93
    ## - pbeds     1     64.62 518.48 177.05
    ## - state    47    359.28 813.14 282.60

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ state + pop18 + poverty + totalinc + 
    ##     pop_den + pbeds, data = new_cdi)
    ## 
    ## Coefficients:
    ## (Intercept)      stateAR      stateAZ      stateCA      stateCO      stateCT  
    ##   4.141e+00    9.456e-01    6.368e-01    3.493e-01    8.819e-01   -1.115e+00  
    ##     stateDC      stateDE      stateFL      stateGA      stateHI      stateID  
    ##  -1.524e-01    5.944e-01    1.204e+00    1.192e+00    1.146e+00    1.110e-01  
    ##     stateIL      stateIN      stateKS      stateKY      stateLA      stateMA  
    ##  -4.324e-01   -1.161e+00    1.550e+00   -6.464e-01   -5.486e-02   -1.507e+00  
    ##     stateMD      stateME      stateMI      stateMN      stateMO      stateMS  
    ##   3.714e-01   -8.683e-01    3.847e-01   -8.946e-01   -4.071e-01   -2.167e-01  
    ##     stateMT      stateNC      stateND      stateNE      stateNH      stateNJ  
    ##  -1.494e+00    4.671e-01   -2.260e+00   -7.235e-02   -1.042e+00   -3.128e-01  
    ##     stateNM      stateNV      stateNY      stateOH      stateOK      stateOR  
    ##   1.093e+00    1.395e+00   -1.022e+00   -1.102e+00    9.323e-01    8.248e-01  
    ##     statePA      stateRI      stateSC      stateSD      stateTN      stateTX  
    ##  -1.907e+00   -1.874e-01    1.084e+00   -1.210e+00    1.430e-01    1.099e+00  
    ##     stateUT      stateVA      stateVT      stateWA      stateWI      stateWV  
    ##   2.572e-01    1.614e-01   -1.489e+00    7.641e-01   -1.836e-01   -9.234e-01  
    ##       pop18      poverty     totalinc      pop_den        pbeds  
    ##   5.038e-02    6.994e-02    1.856e-05    1.651e-04    2.536e+02

``` r
multi_fit_both = lm(sqrt_CRM_1000 ~ state + pop18 + poverty + totalinc + 
    pop_den + pbeds, data = new_cdi)
summary(multi_fit_both)
```

    ## 
    ## Call:
    ## lm(formula = sqrt_CRM_1000 ~ state + pop18 + poverty + totalinc + 
    ##     pop_den + pbeds, data = new_cdi)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5079 -0.6130  0.0000  0.6907  3.5703 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.141e+00  6.109e-01   6.778 4.56e-11 ***
    ## stateAR      9.456e-01  8.737e-01   1.082 0.279803    
    ## stateAZ      6.368e-01  6.438e-01   0.989 0.323205    
    ## stateCA      3.493e-01  4.613e-01   0.757 0.449424    
    ## stateCO      8.819e-01  5.525e-01   1.596 0.111227    
    ## stateCT     -1.115e+00  5.752e-01  -1.939 0.053206 .  
    ## stateDC     -1.524e-01  1.217e+00  -0.125 0.900439    
    ## stateDE      5.944e-01  8.751e-01   0.679 0.497408    
    ## stateFL      1.204e+00  4.644e-01   2.593 0.009871 ** 
    ## stateGA      1.192e+00  5.486e-01   2.173 0.030374 *  
    ## stateHI      1.146e+00  7.550e-01   1.518 0.129718    
    ## stateID      1.110e-01  1.163e+00   0.095 0.924025    
    ## stateIL     -4.324e-01  4.935e-01  -0.876 0.381442    
    ## stateIN     -1.161e+00  5.067e-01  -2.291 0.022523 *  
    ## stateKS      1.550e+00  6.833e-01   2.268 0.023860 *  
    ## stateKY     -6.464e-01  7.519e-01  -0.860 0.390544    
    ## stateLA     -5.486e-02  5.504e-01  -0.100 0.920661    
    ## stateMA     -1.507e+00  5.358e-01  -2.812 0.005174 ** 
    ## stateMD      3.714e-01  5.539e-01   0.670 0.502968    
    ## stateME     -8.683e-01  6.395e-01  -1.358 0.175320    
    ## stateMI      3.847e-01  4.870e-01   0.790 0.430111    
    ## stateMN     -8.946e-01  5.884e-01  -1.521 0.129199    
    ## stateMO     -4.071e-01  5.707e-01  -0.713 0.476074    
    ## stateMS     -2.167e-01  7.494e-01  -0.289 0.772566    
    ## stateMT     -1.494e+00  1.161e+00  -1.286 0.199077    
    ## stateNC      4.671e-01  4.875e-01   0.958 0.338637    
    ## stateND     -2.260e+00  1.167e+00  -1.937 0.053468 .  
    ## stateNE     -7.235e-02  7.551e-01  -0.096 0.923724    
    ## stateNH     -1.042e+00  6.912e-01  -1.508 0.132295    
    ## stateNJ     -3.128e-01  5.036e-01  -0.621 0.534834    
    ## stateNM      1.093e+00  8.764e-01   1.247 0.212994    
    ## stateNV      1.395e+00  8.733e-01   1.597 0.110974    
    ## stateNY     -1.022e+00  4.817e-01  -2.122 0.034448 *  
    ## stateOH     -1.102e+00  4.717e-01  -2.336 0.020013 *  
    ## stateOK      9.323e-01  6.817e-01   1.368 0.172209    
    ## stateOR      8.248e-01  6.113e-01   1.349 0.178055    
    ## statePA     -1.907e+00  4.653e-01  -4.098 5.08e-05 ***
    ## stateRI     -1.874e-01  7.570e-01  -0.248 0.804640    
    ## stateSC      1.084e+00  5.263e-01   2.060 0.040064 *  
    ## stateSD     -1.210e+00  1.170e+00  -1.034 0.301758    
    ## stateTN      1.430e-01  5.625e-01   0.254 0.799477    
    ## stateTX      1.099e+00  4.641e-01   2.369 0.018336 *  
    ## stateUT      2.572e-01  6.858e-01   0.375 0.707852    
    ## stateVA      1.614e-01  5.748e-01   0.281 0.779049    
    ## stateVT     -1.489e+00  1.168e+00  -1.274 0.203391    
    ## stateWA      7.641e-01  5.428e-01   1.408 0.160060    
    ## stateWI     -1.836e-01  5.330e-01  -0.345 0.730626    
    ## stateWV     -9.234e-01  1.162e+00  -0.794 0.427434    
    ## pop18        5.038e-02  1.397e-02   3.605 0.000353 ***
    ## poverty      6.994e-02  1.524e-02   4.588 6.06e-06 ***
    ## totalinc     1.856e-05  4.530e-06   4.097 5.10e-05 ***
    ## pop_den      1.651e-04  4.161e-05   3.968 8.64e-05 ***
    ## pbeds        2.536e+02  3.421e+01   7.413 7.88e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.084 on 386 degrees of freedom
    ## Multiple R-squared:  0.6306, Adjusted R-squared:  0.5808 
    ## F-statistic: 12.67 on 52 and 386 DF,  p-value: < 2.2e-16

``` r
# our Adjusted R-squared is 0.5808 and AIC=120.61
```

``` r
#```{r}
##Step 1:  Fit simple linear regressions for all variables,look for the variable with lowest p-value
#fit1 = lm(sqrt_CRM_1000 ~ state, data = new_cdi)
#summary(fit1) 
#fit2 = lm(sqrt_CRM_1000 ~ region, data = new_cdi)
#summary(fit2) 
#fit3 = lm(sqrt_CRM_1000 ~ totalinc, data = new_cdi)
#summary(fit3) 
#fit4 = lm(sqrt_CRM_1000 ~ pop18, data = new_cdi)
#summary(fit4)
#fit5 = lm(sqrt_CRM_1000 ~ pop65, data = new_cdi)
#summary(fit5) 
#fit6 = lm(sqrt_CRM_1000 ~ pdocs, data = new_cdi)
#summary(fit6) 
#fit7 = lm(sqrt_CRM_1000 ~ pbeds, data = new_cdi)
#summary(fit7) 
#fit8 = lm(sqrt_CRM_1000 ~ bagrad, data = new_cdi)
#summary(fit8) 
#fit9 = lm(sqrt_CRM_1000 ~ poverty, data = new_cdi)
#summary(fit9) 
#fit10 = lm(sqrt_CRM_1000 ~ unemp, data = new_cdi)
#summary(fit10) 
#fit11 = lm(sqrt_CRM_1000 ~ pop_den, data = new_cdi)
#summary(fit11) 
#
#
#
## Enter first the one with the lowest p-value: poverty
#forward1 = lm(sqrt_CRM_1000 ~ poverty, data = new_cdi)
#summary(forward1)
#
#### Step 2: Enter the one with the lowest p-value in the rest 
#fit1 = update(forward1, . ~ . +state)
#summary(fit1)
#fit2 = update(forward1, . ~ . +region)
#summary(fit2)
#fit3 = update(forward1, . ~ . +totalinc)
#summary(fit3)
#fit4 = update(forward1, . ~ . +pop18)
#summary(fit4)
#fit5 = update(forward1, . ~ . +pop65)
#summary(fit5)
#fit6 = update(forward1, . ~ . +pdocs)
#summary(fit6)
#fit7 = update(forward1, . ~ . +pbeds)
#summary(fit7)
#fit8 = update(forward1, . ~ . +bagrad)
#summary(fit8)
#fit9 = update(forward1, . ~ . +unemp)
#summary(fit9)
#fit10 = update(forward1, . ~ . +pop_den)
#summary(fit10) 
#
#
#
## Enter the one with the lowest p-value: beds
#forward2 = update(forward1, . ~ . + beds)
#summary(forward2)
#
#### Step 3: Enter the one with the lowest p-value in the rest 
#fit1 = update(forward2, . ~ . +state)
#summary(fit1)
#fit2 = update(forward2, . ~ . +area)
#summary(fit2)
#fit3 = update(forward2, . ~ . +pop)
#summary(fit3)
#fit4 = update(forward2, . ~ . +pop18)
#summary(fit4)
#fit5 = update(forward2, . ~ . +pop65)
#summary(fit5)
#fit6 = update(forward2, . ~ . +docs)
#summary(fit6)
#fit7 = update(forward2, . ~ . +hsgrad)
#summary(fit7)
#fit8 = update(forward2, . ~ . +bagrad)
#summary(fit8)
#fit9 = update(forward2, . ~ . +unemp)
#summary(fit9)
#fit10 = update(forward2, . ~ . +pcincome)
#summary(fit10)
#fit11 = update(forward2, . ~ . +totalinc)
#summary(fit11)
#
#
## Enter the one with the lowest p-value: bagrad
#forward3 = update(forward2, . ~ . + bagrad)
#summary(forward3)
#
#### Step 4: Enter the one with the lowest p-value in the rest 
#fit1 = update(forward2, . ~ . +state)
#summary(fit1)
#fit2 = update(forward2, . ~ . +area)
#summary(fit2)
#fit3 = update(forward2, . ~ . +pop)
#summary(fit3)
#fit4 = update(forward2, . ~ . +pop18)
#summary(fit4)
#fit5 = update(forward2, . ~ . +pop65)
#summary(fit5)
#fit6 = update(forward2, . ~ . +docs)
#summary(fit6)
#fit7 = update(forward2, . ~ . +hsgrad)
#summary(fit7)
#fit8 = update(forward2, . ~ . +unemp)
#summary(fit8)
#fit9 = update(forward2, . ~ . +pcincome)
#summary(fit9)
#fit10 = update(forward2, . ~ . +totalinc)
#summary(fit10)
#
#
## Enter the one with the lowest p-value: unemp
#forward4 = update(forward3, . ~ . + unemp)
#summary(forward4)
#
#
#### Step 5: Enter the one with the lowest p-value in the rest 
#fit1 = update(forward2, . ~ . +state)
#summary(fit1)
#fit2 = update(forward2, . ~ . +area)
#summary(fit2)
#fit3 = update(forward2, . ~ . +pop)
#summary(fit3)
#fit4 = update(forward2, . ~ . +pop18)
#summary(fit4)
#fit5 = update(forward2, . ~ . +pop65)
#summary(fit5)
#fit6 = update(forward2, . ~ . +docs)
#summary(fit6)
#fit7 = update(forward2, . ~ . +hsgrad)
#summary(fit7)
#fit8 = update(forward2, . ~ . +pcincome)
#summary(fit8)
#fit9 = update(forward2, . ~ . +totalinc)
#summary(fit9)
#
## Enter the one with the lowest p-value: pop18
#
#forward5 = update(forward4, . ~ . + pop18)
#summary(forward5)
```

Test based procedure
