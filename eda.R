library(tidyverse)
library(ggfortify)
library(ggthemes)
library(magrittr)
library(janitor)
library(glue)
library(xts)
library(lubridate)

# Convert to Time Series ---------------------------------------------------


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
names(pm25) <- "value"

# fill missing values
pm25 <- na.locf(pm25)


# Exploratory ---------------------------------------------------------------------


# very basic
plot(pm25, main = "PM2.5")

# shortcut for ggplot
autoplot(pm25)

# full customization
pm25 %>% 
    ggplot(aes(Index, value)) + 
    geom_line(col = "navyblue") + 
    geom_smooth(se = FALSE, col = "red") + 
    scale_x_datetime(date_breaks = "3 months", date_labels = "%b %y") +
    labs(x = "", y = "", title = "PM25 in Beijing from 2013-03 to 2017-03") + 
    theme_wsj(title_family = "Times", base_size = 7)

# set common theme for all plots below
old <-
    theme_set(theme_wsj(
        title_family = "Lato",
        base_family = "Menlo",
        base_size = 7,
        color = "white"
    ))

# decompose by months or years
ep     <-  endpoints(pm25, on = "months")
monthly_mu <- period.apply(pm25, INDEX = ep, FUN = mean)
indexFormat(monthly_mu) <- "%B %Y"

(p <- monthly_mu %>% 
    ggplot(aes(Index, value)) + 
    geom_line(col = "dodgerblue", alpha = .7) + 
    geom_point(col = "navyblue") + 
    labs(title = "Monthly Average from 2013 to 2017"))

# detect peaks
pks <- monthly_mu[quantmod::findPeaks(monthly_mu) - 1]
p + geom_vline(xintercept = index(pks), lty = 3, col = "salmon")
    

# Transform Data ----------------------------------------------------------

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

# stil a time series
monthly_wide %>% 
    autoplot(facets = FALSE) +
    scale_color_brewer(palette = "Paired")

# for plotting follows tidy data principle
monthly_mu %>% 
    ggplot(aes(
        x   = year(Index),
        y   = value,
        col = factor(month(Index))
    )) + 
    geom_line() + 
    facet_wrap(. ~ month(Index, label = TRUE)) + 
    scale_color_brewer(palette = "Paired", guide = "none") + 
    theme_minimal(base_family = "Menlo") + 
    labs(x = "", y = "", title = "PM25 by Month (March 2013 - March 2017)")

    
    