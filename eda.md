EDA
================
Wenjie Wu

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = case_match(
      id, 
      "USW00094728" ~ "CentralPark_NY", 
      "USW00022534" ~ "Molokai_HI",
      "USS0023B17S" ~ "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10,
    month = floor_date(date, unit = "month")) |> # round down the date
  select(name, id, everything())
```

    ## using cached file: /Users/wenjiewu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:19:26.830591 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/wenjiewu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 10:19:32.098398 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/wenjiewu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 10:19:33.764155 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

Make some plots

``` r
weather_df |>
  ggplot(aes(x = prcp)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 15 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](eda_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
weather_df |>
  filter(prcp > 1000)
```

    ## # A tibble: 3 × 7
    ##   name           id          date        prcp  tmax  tmin month     
    ##   <chr>          <chr>       <date>     <dbl> <dbl> <dbl> <date>    
    ## 1 CentralPark_NY USW00094728 2021-08-21  1130  27.8  22.8 2021-08-01
    ## 2 CentralPark_NY USW00094728 2021-09-01  1811  25.6  17.2 2021-09-01
    ## 3 Molokai_HI     USW00022534 2022-12-18  1120  23.3  18.9 2022-12-01

``` r
weather_df |>
  filter(tmax > 20, tmax < 30) |>
  ggplot(aes(x = tmin, y = tmax, color = name, shape = name)) +
  geom_point()
```

![](eda_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## group_by()

``` r
weather_df |>
  group_by(name) |>
  summarize(
    n_nobs = n(),
    n_dist = n_distinct(month))
```

    ## # A tibble: 3 × 3
    ##   name           n_nobs n_dist
    ##   <chr>           <int>  <int>
    ## 1 CentralPark_NY    730     24
    ## 2 Molokai_HI        730     24
    ## 3 Waterhole_WA      730     24

``` r
weather_df |>
  group_by(name, month) |>
  summarize(
    n_nobs = n())
```

    ## `summarise()` has grouped output by 'name'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 72 × 3
    ## # Groups:   name [3]
    ##    name           month      n_nobs
    ##    <chr>          <date>      <int>
    ##  1 CentralPark_NY 2021-01-01     31
    ##  2 CentralPark_NY 2021-02-01     28
    ##  3 CentralPark_NY 2021-03-01     31
    ##  4 CentralPark_NY 2021-04-01     30
    ##  5 CentralPark_NY 2021-05-01     31
    ##  6 CentralPark_NY 2021-06-01     30
    ##  7 CentralPark_NY 2021-07-01     31
    ##  8 CentralPark_NY 2021-08-01     31
    ##  9 CentralPark_NY 2021-09-01     30
    ## 10 CentralPark_NY 2021-10-01     31
    ## # ℹ 62 more rows

``` r
weather_df |>
  count(name)
```

    ## # A tibble: 3 × 2
    ##   name               n
    ##   <chr>          <int>
    ## 1 CentralPark_NY   730
    ## 2 Molokai_HI       730
    ## 3 Waterhole_WA     730

## 2X2

``` r
weather_df |>
  drop_na(tmax) |>
  filter(name != "Molokai_HI") |>
  mutate(
    cold = case_when(
      tmax < 5 ~"cold",
      tmax >= 5 ~ "not_cold"
    )
  ) |>
  group_by(name, cold) |>
  summarize(count = n())
```

    ## `summarise()` has grouped output by 'name'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 4 × 3
    ## # Groups:   name [2]
    ##   name           cold     count
    ##   <chr>          <chr>    <int>
    ## 1 CentralPark_NY cold        96
    ## 2 CentralPark_NY not_cold   634
    ## 3 Waterhole_WA   cold       319
    ## 4 Waterhole_WA   not_cold   395

### tabyl

``` r
weather_df |>
  drop_na(tmax) |>
  filter(name != "Molokai_HI") |>
  mutate(
    cold = case_when(
      tmax < 5 ~"cold",
      tmax >= 5 ~ "not_cold"
    )
  ) |>
  janitor::tabyl(name, cold)
```

    ##            name cold not_cold
    ##  CentralPark_NY   96      634
    ##    Waterhole_WA  319      395

## General numeric summaries
