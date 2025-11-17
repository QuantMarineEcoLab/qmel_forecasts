library(fpp3)
library(openmeteo)
library(fabletools)
library(tidyverse)
library(lubridate)
library(GGally)

# Read in ginkgo data
ginkgo_dat<-read.csv("projects/gingko/data/gingko.csv")
ginkgo_dat$fall <- "yes"

## Ignore this part.....I was basically going to fit a logistic model -----------
# Convert to tsibble and fill in blank years
ginkgo_dat <- tsibble(ginkgo_dat, index = "Year")
ginkgo_dat<-tsibble::fill_gaps(ginkgo_dat)

# Which years are NA?
missing_years <- ginkgo_dat$Year[is.na(ginkgo_dat$fall)]

# Create another ginkgo dataset by day
ginkgo_expand <- seq(from = ymd("1977-01-01"), to = ymd("2024-11-03"), by = "days")
ginkgo_expand <- data.frame(day = ginkgo_expand, yday = yday(ginkgo_expand))
ginkgo_expand$year <- year(ginkgo_expand$day)

# Remove years that are NA from ginkgo_dat
ginkgo_expand <- ginkgo_expand %>%
  filter(year %in% !!missing_years == F)

# Now go back to ginkgo_dat and drop NAs
ginkgo_dat <- ginkgo_dat %>%
  filter(!is.na(fall))

# Now combine so that julian day of fall is "yes" and all others are "no"
ginkgo_expand <- left_join(ginkgo_expand, ginkgo_dat, by = join_by(year == Year, yday == Julian.day))

ginkgo_expand <- ginkgo_expand %>%
  mutate(fall = ifelse(is.na(fall), "no", fall),
         fall_numeric = ifelse(fall == "no", 0, 1))

# Now turn it into a tsibble?
ginkgo_tsibble <- tsibble(ginkgo_expand, index = "day")

# Fill gaps again
ginkgo_tsibble<-tsibble::fill_gaps(ginkgo_tsibble)

autoplot(ginkgo_tsibble, fall_numeric)

### Yearly data ------------------------------------
ginkgo_dat_year<-tsibble::fill_gaps(ginkgo_dat)

# Refer to Ch 7 in the forecasting textbook, "Time series regression models".
# Let's think of a couple of covariates.
# 1) Precipitation
# 2) soil temp
# 3) wind speed
# 4) air temp? (but this will be correlated with soil temp)
# 5) photo period....?
# Maybe use soil and air temp for a model selection process

# Let's use era5 data....
# Default units:
# list(temperature_unit = "celsius",windspeed_unit = "kmh", precipitation_unit = "mm"

gingko_coords<-c(43.136806, -70.934278)

# Time at temps below frost since last frost (Average May 10)

# Need hourly data -----
# Time at temps below frost (<=0) since start of growing period
# Time at temps below deep freeze (<= -3.89) since start of growing period

nh_temp <- weather_history(gingko_coords,
                           start="1977-01-01",
                           end="2024-11-03",
                           hourly=c("temperature_2m", "apparent_temperature"))

nh_temp <- nh_temp %>%
  mutate(year = year(datetime))

# Remove NAs
nh_temp <- nh_temp %>%
  filter(!is.na(datetime)) %>%
  mutate(yday = yday(datetime),
         date = date(datetime))

# Get first day of growing season
years <- unique(nh_temp$year)

start_of_szn <- vector(length = length(years))
for(n in 1:length(years)){
  temp <- nh_temp %>% filter(year == years[n])
  
  temp <- temp %>% mutate(over_frost = ifelse(hourly_apparent_temperature > 0, 1, 0))
  
  temp <- temp %>% 
    group_by(year, date, yday) %>%
    summarize(sum_frost = sum(over_frost))
  
  temp <- temp %>% filter(sum_frost < 24 & month(date) <8)
  
  start_of_szn[n] <- (temp$yday)[nrow(temp)]+1
}

start_of_growing_season <- data.frame(year = years, start_of_szn = start_of_szn)

names(ginkgo_dat) <- c("year", "fall_yday", "fall")

# Make full dataset
ginkgo_dat <- left_join(ginkgo_dat, start_of_growing_season, by= "year")

# Add length of growing season?
ginkgo_dat <- ginkgo_dat %>%
  mutate(growing_season = fall_yday-start_of_szn+1)

# Join ginkgo_dat to nh_temp
nh_temp_reduced <- full_join(nh_temp, ginkgo_dat, 
                             by = join_by(year, yday >= start_of_szn,
                                          yday <= fall_yday))

nh_temp_reduced <- nh_temp_reduced %>%
  filter(!is.na(fall_yday))

# Some viz
ggplot(nh_temp %>% filter(year == 1982))+
  geom_point(aes(x = datetime, y = hourly_temperature_2m), color = "blue")+
  geom_point(aes(x = datetime, y = hourly_apparent_temperature), color = "red")+
  geom_hline(aes(yintercept = 0))

ggplot(ginkgo_dat)+
  geom_point(aes(x = year, y = start_of_szn))

ggplot(ginkgo_dat)+
  geom_point(aes(x = year, y = growing_season))

# Now summarize number of days below frost
nh_temp_sum <- nh_temp_reduced %>%
  mutate(below_zero = ifelse(hourly_apparent_temperature <= 0, 1, 0),
         below_deep_freeze = ifelse(hourly_apparent_temperature <= -3.89, 1, 0)) %>%
  group_by(year) %>%
  summarize(hours_below_zero = sum(below_zero),
            hour_deep_freeze = sum(below_deep_freeze))

# Need daily data ------
nh_precip <- weather_history(gingko_coords,
                           start="1977-01-01",
                           end="2024-11-03",
                           daily=c("precipitation_sum", "wind_speed_10m_max",
                                    "wind_gusts_10m_max"))

# Filter to just growing season
nh_precip <- nh_precip %>%
  mutate(year = year(date), yday = yday(date))

nh_precip_reduced <- full_join(nh_precip, ginkgo_dat,
                               by = join_by(year, yday >= start_of_szn,
                                            yday <= fall_yday))

nh_precip_reduced <- nh_precip_reduced %>%
  filter(!is.na(fall_yday))
  
# Cumulative precipitation since start of growing period
# Day of max wind speed?
nh_precip_sum <- nh_precip_reduced %>%
  group_by(year) %>%
  summarize(total_precip = sum(daily_precipitation_sum),
            max_wind_speed = max(daily_wind_speed_10m_max),
            max_wind_gust = max(daily_wind_gusts_10m_max),
            day_wind_speed = yday[tail(which(daily_wind_speed_10m_max==max_wind_speed),1)],
            day_wind_gust = yday[tail(which(daily_wind_gusts_10m_max==max_wind_gust),1)])

# Join precip and temp
nh_weather <- left_join(nh_precip_sum, nh_temp_sum, by = join_by(year))

# Join weather and ginkgo data
ginkgo_dat <- left_join(ginkgo_dat, nh_weather, by= join_by(year))

# set ginkgo_dat to tsibble
ginkgo_dat <- tsibble(ginkgo_dat, index = "year")
ginkgo_dat<-tsibble::fill_gaps(ginkgo_dat)

ggcorr(data = ginkgo_dat)

fit_ginkgo <- ginkgo_dat %>%
  model(lm = TSLM(fall_yday ~ trend() + growing_season + total_precip + hours_below_zero +
                    hour_deep_freeze + day_wind_gust))

# fc_ginkgo <- fit_ginkgo %>% forecast(h = 5)

# Forecast growing season, total precip, hours below zero, hour deep freeze, day wind gust
fit_growing_season <- ginkgo_dat |>
  model(TSLM(growing_season ~ trend())) |>
  forecast(h = 5)

fit_precip <- ginkgo_dat |>
  model(TSLM(total_precip ~ trend())) |>
  forecast(h = 5)

fit_below_zero <- ginkgo_dat |>
  model(TSLM(hours_below_zero ~ trend())) |>
  forecast(h = 5)

fit_deep_freeze <- ginkgo_dat |>
  model(TSLM(hour_deep_freeze ~ trend())) |>
  forecast(h = 5)

fit_wind_gust <- ginkgo_dat |>
  model(TSLM(day_wind_gust ~ trend())) |>
  forecast(h = 5)

autoplot(fit_wind_gust, ginkgo_dat)

future_scenarios <- scenarios(
  future = new_data(ginkgo_dat, 5) %>% 
    mutate(growing_season=fit_growing_season$.mean, total_precip=fit_precip$.mean,
           hours_below_zero=fit_below_zero$.mean, hour_deep_freeze=fit_deep_freeze$.mean,
           day_wind_gust = fit_wind_gust$.mean),
  names_to = "Scenario")

fc <- forecast(fit_ginkgo, new_data = future_scenarios)

ginkgo_dat %>%
  autoplot(fall_yday) +
  autolayer(fc)
  # labs(y = "% change in US consumption")

# Test just plotting trend?
fit_ginkgo2 <- ginkgo_dat %>%
  model(TSLM(fall_yday ~ trend())) %>%
  forecast(h=5)

fit_ginkgo2

# You get the same result.

# Test just the linear predictors?
fit_ginkgo3 <- ginkgo_dat %>%
  model(lm = TSLM(fall_yday ~ growing_season + total_precip + hours_below_zero +
                    hour_deep_freeze + day_wind_gust))

fc3 <- forecast(fit_ginkgo3, new_data = future_scenarios)

ginkgo_dat %>%
  autoplot(fall_yday) +
  autolayer(fc3)



