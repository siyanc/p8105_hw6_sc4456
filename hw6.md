hw6
================
Siyan Chen
11/20/2018

Problem1
========

### import data

``` r
homicide_data = read.csv(text = getURL("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv")) %>% 
  janitor::clean_names()
```

### data manipulation

``` r
homicide_data_modify = 
  homicide_data %>%
  mutate(city_state = str_c(city, "_", state),
         solved = str_detect(disposition, "[Cc]losed by arrest"),
         solved = as.numeric(solved)) %>% 
  filter(city_state != "Dallas_TX" & city_state != "Phoenix_AZ" & city_state != "Kansas City_MO" & city_state != "Tulsa_AL") %>% 
  mutate(victim_race = ifelse(victim_race == "White", "White", "Non_White"),
         victim_race = as.factor(victim_race),
         victim_race = fct_relevel(victim_race,"White"),
         victim_age = as.numeric(victim_age))
head(homicide_data_modify)
```

    ##          uid reported_date victim_last victim_first victim_race victim_age
    ## 1 Alb-000001      20100504      GARCIA         JUAN   Non_White         79
    ## 2 Alb-000002      20100216     MONTOYA      CAMERON   Non_White         12
    ## 3 Alb-000003      20100601 SATTERFIELD      VIVIANA       White         10
    ## 4 Alb-000004      20100101    MENDIOLA       CARLOS   Non_White         29
    ## 5 Alb-000005      20100102        MULA       VIVIAN       White         73
    ## 6 Alb-000006      20100126        BOOK    GERALDINE       White         94
    ##   victim_sex        city state      lat       lon           disposition
    ## 1       Male Albuquerque    NM 35.09579 -106.5386 Closed without arrest
    ## 2       Male Albuquerque    NM 35.05681 -106.7153      Closed by arrest
    ## 3     Female Albuquerque    NM 35.08609 -106.6956 Closed without arrest
    ## 4       Male Albuquerque    NM 35.07849 -106.5561      Closed by arrest
    ## 5     Female Albuquerque    NM 35.13036 -106.5810 Closed without arrest
    ## 6     Female Albuquerque    NM 35.15111 -106.5378        Open/No arrest
    ##       city_state solved
    ## 1 Albuquerque_NM      0
    ## 2 Albuquerque_NM      1
    ## 3 Albuquerque_NM      0
    ## 4 Albuquerque_NM      1
    ## 5 Albuquerque_NM      0
    ## 6 Albuquerque_NM      0

### Regression Model

``` r
Balti_df = homicide_data_modify %>% 
  filter(city_state == "Baltimore_MD") 

fit_logistic = 
  Balti_df %>% glm(solved ~victim_age + victim_sex + victim_race, data = ., family = binomial(logit))

# fit logistic model

output_Balti = 
  fit_logistic %>% 
  broom::tidy() %>% 
  mutate(log_OR = estimate,
         OR = exp(estimate)) %>% 
  select(term, log_OR, OR, p.value)

ci_Balti = exp(confint (fit_logistic)) %>% as.data.frame()
### get the confidence interval for odds ratio
rownames(ci_Balti) = NULL
### get ride of row name
cbind(output_Balti, ci_Balti) %>% 
  select(-p.value) %>% 
  filter(term == "victim_raceNon_White") %>% 
  knitr::kable(digits = 3)
```

| term                   |  log\_OR|     OR|  2.5 %|  97.5 %|
|:-----------------------|--------:|------:|------:|-------:|
| victim\_raceNon\_White |   -0.793|  0.453|  0.321|   0.636|

According to the output, the odds ratio for solving homicides comparing non\_white civtims to white victims is 0.453. We are 95% confident that the odds ratio for olving homicides comparing non\_white civtims to white victims is somewhere between 0.321 and 0.636

### GLM for Each City

``` r
homicide_glm =
  homicide_data_modify %>% 
  group_by(city_state) %>% 
  nest() %>% 
  mutate(models = map(data, ~glm(solved ~ victim_age + victim_sex + victim_race, data = .x, family = binomial())), parameters = map(models, broom::tidy)) %>% 
  select(-data) %>% 
  mutate(ci = map(models, confint),
         results = map2(parameters, ci, cbind)) %>% 
  unnest(results) %>% 
  filter(term == "victim_raceNon_White") %>% 
  mutate(OR =exp(estimate)) %>% 
  select(-std.error, -statistic, -p.value, -term, -estimate) %>% 
  rename(confi_low  = `2.5 %`, confi_high = `97.5 %`) %>% 
  mutate(confi_low = exp(confi_low), confi_high = exp(confi_high))

homicide_glm %>% 
  knitr::kable(digits = 3)
```

| city\_state        |  confi\_low|  confi\_high|     OR|
|:-------------------|-----------:|------------:|------:|
| Albuquerque\_NM    |       0.416|        1.124|  0.686|
| Atlanta\_GA        |       0.433|        1.320|  0.767|
| Baltimore\_MD      |       0.321|        0.636|  0.453|
| Baton Rouge\_LA    |       0.299|        1.380|  0.656|
| Birmingham\_AL     |       0.619|        1.759|  1.047|
| Boston\_MA         |       0.045|        0.272|  0.121|
| Buffalo\_NY        |       0.243|        0.811|  0.447|
| Charlotte\_NC      |       0.318|        0.931|  0.555|
| Chicago\_IL        |       0.442|        0.751|  0.575|
| Cincinnati\_OH     |       0.186|        0.554|  0.327|
| Columbus\_OH       |       0.657|        1.191|  0.884|
| Denver\_CO         |       0.352|        0.998|  0.594|
| Detroit\_MI        |       0.495|        0.881|  0.661|
| Durham\_NC         |       0.446|        2.851|  1.153|
| Fort Worth\_TX     |       0.563|        1.286|  0.853|
| Fresno\_CA         |       0.228|        0.861|  0.457|
| Houston\_TX        |       0.738|        1.148|  0.921|
| Indianapolis\_IN   |       0.389|        0.681|  0.516|
| Jacksonville\_FL   |       0.519|        0.891|  0.681|
| Las Vegas\_NV      |       0.613|        1.012|  0.788|
| Long Beach\_CA     |       0.404|        1.702|  0.843|
| Los Angeles\_CA    |       0.523|        0.980|  0.718|
| Louisville\_KY     |       0.286|        0.650|  0.434|
| Memphis\_TN        |       0.536|        1.194|  0.807|
| Miami\_FL          |       0.379|        0.879|  0.577|
| Milwaukee\_wI      |       0.417|        1.022|  0.660|
| Minneapolis\_MN    |       0.351|        1.248|  0.667|
| Nashville\_TN      |       0.647|        1.225|  0.892|
| New Orleans\_LA    |       0.325|        0.801|  0.511|
| New York\_NY       |       0.279|        1.019|  0.548|
| Oakland\_CA        |       0.101|        0.427|  0.217|
| Oklahoma City\_OK  |       0.503|        1.005|  0.711|
| Omaha\_NE          |       0.096|        0.317|  0.180|
| Philadelphia\_PA   |       0.498|        0.873|  0.662|
| Pittsburgh\_PA     |       0.162|        0.498|  0.290|
| Richmond\_VA       |       0.162|        1.204|  0.488|
| San Antonio\_TX    |       0.466|        1.036|  0.698|
| Sacramento\_CA     |       0.442|        1.330|  0.774|
| Savannah\_GA       |       0.308|        1.311|  0.644|
| San Bernardino\_CA |       0.427|        2.134|  0.946|
| San Diego\_CA      |       0.264|        0.699|  0.434|
| San Francisco\_CA  |       0.288|        0.718|  0.458|
| St. Louis\_MO      |       0.423|        0.850|  0.601|
| Stockton\_CA       |       0.205|        0.747|  0.395|
| Tampa\_FL          |       0.611|        2.299|  1.185|
| Tulsa\_OK          |       0.400|        0.850|  0.586|
| Washington\_DC     |       0.258|        1.020|  0.527|

### plot

``` r
homicide_glm %>% 
  mutate(city_state = fct_reorder(city_state, desc(OR))) %>% 
  ggplot(aes(x = city_state, y = OR)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = confi_high, ymin = confi_low)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](hw6_files/figure-markdown_github/unnamed-chunk-5-1.png)

Comment: Tampa\_FL has the highest odds ratio for solving homicides comparing non-white victim to white victims while Boston\_MA has the lowest odds ratio. Durham\_NC has odds ratio with greatest CI range.

Problem 2
=========

### Data Input and Tidy

``` r
birth_df = read_csv("./data/birthweight.csv") %>% 
  janitor::clean_names() %>% 
  mutate(babysex = as.factor(babysex),
         frace = as.factor(frace),
         malform = as.factor(malform),
         mrace = as.factor(mrace))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   gaweeks = col_double(),
    ##   ppbmi = col_double(),
    ##   smoken = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
# change appropriate term from numeric to factor

is.na(birth_df)
# check for NA and there is no NA
```

### Model builting

``` r
library("leaps")
mult.fit = lm(bwt ~., data = birth_df)
summary(mult.fit)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ ., data = birth_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1097.68  -184.86    -3.33   173.09  2344.15 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6265.3914   660.4011  -9.487  < 2e-16 ***
    ## babysex2       28.7073     8.4652   3.391 0.000702 ***
    ## bhead         130.7781     3.4523  37.881  < 2e-16 ***
    ## blength        74.9536     2.0217  37.075  < 2e-16 ***
    ## delwt           4.1007     0.3948  10.386  < 2e-16 ***
    ## fincome         0.2898     0.1795   1.614 0.106551    
    ## frace2         14.3313    46.1501   0.311 0.756168    
    ## frace3         21.2361    69.2960   0.306 0.759273    
    ## frace4        -46.9962    44.6782  -1.052 0.292912    
    ## frace8          4.2969    74.0741   0.058 0.953745    
    ## gaweeks        11.5494     1.4654   7.882 4.06e-15 ***
    ## malform1        9.7650    70.6259   0.138 0.890039    
    ## menarche       -3.5508     2.8951  -1.226 0.220083    
    ## mheight         9.7874    10.3116   0.949 0.342588    
    ## momage          0.7593     1.2221   0.621 0.534418    
    ## mrace2       -151.4354    46.0453  -3.289 0.001014 ** 
    ## mrace3        -91.3866    71.9190  -1.271 0.203908    
    ## mrace4        -56.4787    45.1369  -1.251 0.210901    
    ## parity         95.5411    40.4793   2.360 0.018307 *  
    ## pnumlbw             NA         NA      NA       NA    
    ## pnumsga             NA         NA      NA       NA    
    ## ppbmi           4.3538    14.8913   0.292 0.770017    
    ## ppwt           -3.4716     2.6121  -1.329 0.183913    
    ## smoken         -4.8544     0.5871  -8.269  < 2e-16 ***
    ## wtgain              NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 272.5 on 4320 degrees of freedom
    ## Multiple R-squared:  0.7183, Adjusted R-squared:  0.717 
    ## F-statistic: 524.6 on 21 and 4320 DF,  p-value: < 2.2e-16

``` r
predict_reg = lm(bwt ~ babysex + bhead + blength + delwt + gaweeks + smoken, data = birth_df)
summary(predict_reg)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + gaweeks + 
    ##     smoken, data = birth_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1181.20  -183.63    -9.49   174.43  2506.57 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6285.7402    97.0521 -64.767  < 2e-16 ***
    ## babysex2       30.3364     8.7343   3.473 0.000519 ***
    ## bhead         137.1933     3.5353  38.806  < 2e-16 ***
    ## blength        78.8032     2.0720  38.033  < 2e-16 ***
    ## delwt           2.0766     0.1994  10.416  < 2e-16 ***
    ## gaweeks        14.0748     1.4976   9.398  < 2e-16 ***
    ## smoken         -2.1711     0.5823  -3.729 0.000195 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 282 on 4335 degrees of freedom
    ## Multiple R-squared:  0.6973, Adjusted R-squared:  0.6969 
    ## F-statistic:  1665 on 6 and 4335 DF,  p-value: < 2.2e-16

``` r
df1 = modelr::add_residuals(birth_df, predict_reg)
df2 = modelr::add_predictions(birth_df, predict_reg)
df = merge(df1, df2)

df %>%
  ggplot(aes(x = pred, y = resid)) + geom_point() + geom_hline(yintercept = 0, color = "red")
```

![](hw6_files/figure-markdown_github/unnamed-chunk-7-1.png)

I start with full model. Based on the output, I only keep the coefficient with significant difference 0.01 which includes babysex2, bhead, blength, delwt, gaweeks, smoken for my predictive model.

``` r
fit1 = lm(bwt ~ blength + gaweeks, data = birth_df)
summary(fit1)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ blength + gaweeks, data = birth_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1709.6  -215.4   -11.4   208.2  4188.8 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -4347.667     97.958  -44.38   <2e-16 ***
    ## blength       128.556      1.990   64.60   <2e-16 ***
    ## gaweeks        27.047      1.718   15.74   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 333.2 on 4339 degrees of freedom
    ## Multiple R-squared:  0.5769, Adjusted R-squared:  0.5767 
    ## F-statistic:  2958 on 2 and 4339 DF,  p-value: < 2.2e-16

``` r
fit2 = lm(bwt ~ bhead*blength*babysex, data = birth_df)
summary(fit2)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ bhead * blength * babysex, data = birth_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1132.99  -190.42   -10.33   178.63  2617.96 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            -7176.8170  1264.8397  -5.674 1.49e-08 ***
    ## bhead                    181.7956    38.0542   4.777 1.84e-06 ***
    ## blength                  102.1269    26.2118   3.896 9.92e-05 ***
    ## babysex2                6374.8684  1677.7669   3.800 0.000147 ***
    ## bhead:blength             -0.5536     0.7802  -0.710 0.478012    
    ## bhead:babysex2          -198.3932    51.0917  -3.883 0.000105 ***
    ## blength:babysex2        -123.7729    35.1185  -3.524 0.000429 ***
    ## bhead:blength:babysex2     3.8781     1.0566   3.670 0.000245 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 287.7 on 4334 degrees of freedom
    ## Multiple R-squared:  0.6849, Adjusted R-squared:  0.6844 
    ## F-statistic:  1346 on 7 and 4334 DF,  p-value: < 2.2e-16

### Cross\_Validation

``` r
cv_df = crossv_mc(birth_df, 100) %>% 
  mutate(train = map(train, as.tibble),
         test = map(test, as.tibble))
### create column list for train and test

cv_df =
  cv_df %>% 
  mutate(pred_mod = map(train, ~lm(bwt ~ babysex + bhead + blength + delwt + gaweeks + smoken, data = .x)),
         fit1 = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
         fit2 = map(train, ~lm(bwt ~ bhead*blength*babysex, data = .x))) %>% 
  mutate(rmse_pred = map2_dbl(pred_mod, test, ~ rmse(model = .x, data = .y)),
         rmse_fit1 = map2_dbl(fit1, test, ~ rmse(model = .x, data = .y)),
         rmse_fit2 = map2_dbl(fit2, test, ~ rmse(model = .x, data = .y)))
### Map the three model to each list of train and use test to obtain rmse

cv_df %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = 1:3) %>% 
  mutate(model = as.factor(model),
         model = fct_inorder(model)) %>%
  rename(rmse = `1:3`) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](hw6_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
### make plot of rmse for each model
```

Comment: The rmse plot suggests the predictive model is better than the fit1 (which is the main effects only model) and fit2 (which is three-way interaction) and model fit1 is better than model fit2.
