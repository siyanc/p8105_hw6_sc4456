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
homicide_data = 
  homicide_data %>% 
  mutate(city_state = str_c(city, "_", state),
         solved = str_detect(disposition, "[Cc]losed by arrest"),
         solved = as.numeric(solved))

homicide_data = 
  homicide_data %>% 
  filter(city_state != "Dallas_TX"  & city_state !="Phoenix_AZ" & city_state !="Kansas_MO" & city_state !="Tulsa_AL")

homicide_data =
  homicide_data %>% 
  mutate(victim_race = ifelse(victim_race == "White", "White", "Non_White"), 
         victim_age = as.numeric(victim_age))  
```

### Regression Model

``` r
Balti_df = homicide_data %>% 
  filter(city_state == "Baltimore_MD")
fit_logistic = 
  Balti_df %>% 
  glm(solved ~ victim_age + victim_sex + victim_race, data = ., family = binomial(link = "logit"))

fit_logistic %>% 
  broom::tidy() %>% 
  mutate(log_OR = estimate,
         OR = exp(estimate)) %>% 
  select(term, log_OR, OR, p.value)
```

    ## # A tibble: 4 x 4
    ##   term               log_OR    OR  p.value
    ##   <chr>               <dbl> <dbl>    <dbl>
    ## 1 (Intercept)       0.254   1.29  1.12e- 1
    ## 2 victim_age       -0.00374 0.996 2.17e- 1
    ## 3 victim_sexMale   -0.885   0.413 8.08e-11
    ## 4 victim_raceWhite  0.793   2.21  5.33e- 6
