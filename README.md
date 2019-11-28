Tutorial on Working Time Series in R
================

``` 
 -- This README.md is generated from README.Rmd. DO NOT edit here. -- 
```

## Background

Though you may not be an econometrician or a quant analyst, dealing with
time series data is common in every analysis line of work. This is a
walkthrough of usual techniques to explore time series data using `R`.

## Setup

The following are the libraries needed for this tutorial. Note that,
there are several time-series objects in `R`, most common ones are `ts`
`xts` and `zoo`. They can be used interchangably most of the time. This
tutorial will rely on `xts` (eXtensible Time Series).

``` r
library(tidyverse)
library(ggfortify)
library(ggthemes)
library(magrittr)
library(janitor)
library(glue)
library(xts)
library(lubridate)
library(quantmod)
library(forecast)
```

## Dataset

We will make use of the air-quality data from the Beijing Municipal
Environmental Monitoring Center. Specifically, the PM25 reading, an
atmospheric particulate matter (PM) that have a diameter of less than
2.5 micrometers.

The comprehensive data set includes hourly air pollutants data from 12
nationally-controlled air-quality monitoring sites. The meteorological
data in each air-quality site are matched with the nearest weather
station from the China Meteorological Administration. The time period is
from March 1st, 2013 to February 28th, 2017. Missing data are denoted as
NA.

Source:
*<https://archive.ics.uci.edu/ml/datasets/Beijing+Multi-Site+Air-Quality+Data>*

Zhang, S., Guo, B., Dong, A., He, J., Xu, Z. and Chen, S.X. (2017)
Cautionary Tales on Air-Quality Improvement in Beijing. Proceedings of
the Royal Society A, Volume 473, No. 2205, Pages 20170457.

``` r
# combine all csv files at once
# convert index to datetime obj
raw <- "Data" %>%
    list.files(full.names = TRUE) %>%
    lapply(., read_csv) %>%
    bind_rows() %>%
    clean_names() %>% 
    mutate(no = glue("{year}-{month}-{day} {hour}:00:00"), 
           no = as.POSIXct(no, tz = "Asia/Shanghai")) %>% 
    select(-c(year:hour)) %>% 
    rename(datetime = no)

# convert to time series
pm25 <- raw %>% 
    group_by(datetime) %>% 
    summarise(value = mean(pm2_5, na.rm = TRUE)) %$% 
    xts(.$value, order.by = .$datetime)
names(pm25) <- "pm25"

head(pm25)
```

    ##                         pm25
    ## 2013-03-01 00:00:00 5.666667
    ## 2013-03-01 01:00:00 6.833333
    ## 2013-03-01 02:00:00 5.666667
    ## 2013-03-01 03:00:00 6.000000
    ## 2013-03-01 04:00:00 4.833333
    ## 2013-03-01 05:00:00 5.500000

## Exploratory

There are 4 ways to deal with missing data, to omit use `na.omit`, to
carry last observation forward use `na.locf`, to carry next observation
backward use `na.locf(fromLast = TRUE)`, to interpolate using linear
approximation use `na.approx`.

``` r
# fill missing values
pm25 <- na.locf(pm25)
```

For plotting, there are several ways. Either using base R default,
through `ggfortify` autoplot method, or simply using `ggplot` as per
usual.

``` r
# very basic
plot(pm25, main = "xts::plot.xts")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# shortcut for ggplot
autoplot(pm25) + labs(x = "", y = "", title = "ggfortify::autoplot.xts")
```

![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# full customization
pm25 %>% 
    ggplot(aes(Index, pm25)) + 
    geom_line(col = "navyblue") + 
    geom_smooth(se = FALSE, col = "red") + 
    scale_x_datetime(date_breaks = "6 months", date_labels = "%b %y") +
    labs(x = "", y = "", title = "the ggplot2 way") + 
    ggthemes::theme_wsj(title_family = "Times", color = "white", base_size = 7)
```

![](README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

## Decompose Series into Periods

Usually we want to break down a long series into periods (weeks, months,
or years) for some calculation. Using `xts` it is simply finding
endpoints and proceed to apply some function periodically. Say, to find
monthly average of PM25.

``` r
# decompose by months or years
ep     <-  endpoints(pm25, on = "months")
monthly_mu <- period.apply(pm25, INDEX = ep, FUN = mean)
indexFormat(monthly_mu) <- "%B %Y"
head(monthly_mu)
```

    ##                  pm25
    ## March 2013  104.71906
    ## April 2013   62.22600
    ## May 2013     81.53193
    ## June 2013   102.14312
    ## July 2013    67.68538
    ## August 2013  60.87607

``` r
# detect peaks
pks <- monthly_mu[quantmod::findPeaks(monthly_mu) - 1]

monthly_mu %>% 
    ggplot(aes(Index, pm25)) + 
    geom_line(col = "dodgerblue", alpha = .7) + 
    geom_point(col = "navyblue") + 
    geom_vline(xintercept = index(pks), lty = 4, col = "salmon")+ 
    labs(title = "PM25 Monthly Average from 2013 to 2017")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

In general, follows a `split-apply-combine` strategy. Another example,
say we would like to determine bi-weekly variance by visualizing 50 and
95 quantile.

``` r
# using split-apply-combine strategy
bi_week_range <- pm25 %>% 
    split("month") %>% 
    lapply(function(x) quantile(first(x, "2 weeks"), c(0.5, 0.9))) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    set_names(nm = c("min", "max"))

bi_week_range %>% 
    mutate(week = 1:length(min)) %>% 
    gather(var, val, -week) %>% 
    ggplot(aes(week, val, col = var, group = week)) +
    geom_line(col = "lightgray") +
    geom_point() + 
    scale_color_brewer(guide = "none",
                       direction = -1,
                       palette = "Set1") + 
    labs(title = "Bi-Weekly 50-90 Quantile, PM25")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Data Transformation

This is merely an exercise to demonstrate how to translate a long format
to wide format without losing time-series property. Note: not a data
frame.

``` r
# specify which @year which @month
get_core <- function(y, m) {
    x = coredata(monthly_mu[year(monthly_mu) == y & month(monthly_mu) == m])
    ifelse(length(x) == 0L, NA, x)
}

# from long to wide
# ** map-map is just shortcut for putting 2 for-loops together
yr = unique(year(monthly_mu))
monthly_wide <- map(1:12, ~ map2_dbl(.x, yr, ~ get_core(..2, ..1))) %>% 
    map(~ ts(., start = min(yr), end = max(yr), frequency = 1)) %>% 
    set_names(month.abb) %>% 
    do.call(cbind, .)
monthly_wide
```

    ## Time Series:
    ## Start = 2013 
    ## End = 2017 
    ## Frequency = 1 
    ##            Jan       Feb       Mar      Apr      May       Jun      Jul
    ## 2013        NA        NA 104.71906 62.22600 81.53193 102.14312 67.68538
    ## 2014  97.93781 153.70431  95.30729 90.69111 62.04569  54.43935 89.91680
    ## 2015  96.29138  93.23196  86.29003 71.04736 55.62051  60.75577 60.93247
    ## 2016  66.87994  42.83319  91.75813 67.05101 53.59896  58.51604 68.23572
    ## 2017 113.95358  68.67326        NA       NA       NA        NA       NA
    ##           Aug      Sep       Oct       Nov       Dec
    ## 2013 60.87607 76.25218  91.69247  73.83220  78.33322
    ## 2014 63.00928 65.81218 119.55397  87.11115  59.51845
    ## 2015 44.47829 48.81169  72.10920 114.82227 149.55659
    ## 2016 45.87336 53.42565  84.10155  97.49787 128.72614
    ## 2017       NA       NA        NA        NA        NA

``` r
# stil a time series
monthly_wide %>% 
    autoplot(facets = FALSE) +
    scale_color_brewer(palette = "Paired")
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

In practice, usually there is no need to do that. To have more control
on plotting, follows tidy data principle (use long format).

``` r
monthly_mu %>% 
    ggplot(aes(
        x   = year(Index),
        y   = pm25,
        col = factor(month(Index))
    )) + 
    geom_line() + 
    facet_wrap(. ~ month(Index, label = TRUE)) + 
    scale_color_brewer(palette = "Paired", guide = "none") + 
    theme_minimal(base_family = "Menlo") + 
    labs(x = "", y = "", title = "PM25 by Month (March 2013 - March 2017)")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Detect Correlation, Trend and Seasonality

On detecting seasonality, use autocorrelation for a quick scan.
Autocorrelation measures the linear relationship between lagged values
of a time series. We won’t go into details about the math behind. You
can find plenty of those by searching online.

We begin by exploring the correlation between windspeed and pm25 measure
using a scatter plot.

``` r
windspeed <- raw %>% 
    group_by(datetime) %>% 
    summarise(value = mean(wspm, na.rm = TRUE)) %$%
    xts(.$value, order.by = .$datetime)

wspm25 <- merge(pm25, windspeed, join = "left")

# a quick scatter point   
qplot(
    x = windspeed,
    y = pm25,
    data = as.data.frame(wspm25),
    alpha = .1
) +
    geom_smooth(method = "lm", se = FALSE) + 
    scale_alpha_continuous(guide = "none") +
    labs(x = "Windspeed", title = "PM25") + 
    theme_minimal(base_family = "Menlo")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

We noticed that the windspeed might have a correlation with pm25. As for
windspeed, it is general known to be seasonal. Is it?

``` r
ep    <- endpoints(wspm25, "months")
wspmu <- wspm25$windspeed %>% 
    period.apply(INDEX = ep, FUN = mean) %>% 
    na.locf()
plot(wspmu)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The plot above does not make it obvious. We can reduce noise by applying
moving average smoothing.

``` r
# moving average to reduce noise
wspmu_smoothed  <- rollapply(wspmu, width = 3, FUN = mean)
# or using forecast package
# wspmu_smoothed <- forecast::ma(wspmu, order = 3)

# detect seasonality using autocorrelation
plot(wspmu_smoothed)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

There you notice the seasonality and upward moving trend.
Autocorrelation `stats::acf` gives us a clearer understanding. When data
have a trend, the autocorrelations for small lags tend to be large and
positive. When data are seasonal, the autocorrelations will be larger
for the seasonal lags (at multiples of the seasonal frequency) than for
other lags.

The blue lines incidicates ±2/√T, representing white noise.

``` r
forecast::ggAcf(wspmu) + labs(title = "Correlogram - Windspeed")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
