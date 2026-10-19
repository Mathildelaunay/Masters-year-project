library(dplyr)
library(lubridate)
library(tidyr)

# Want to get a list of the first day of the week for all the dates with observations
mosquito_climate <- mosquito_climate %>%
  mutate(
    date = as.Date(date),
    week_start = floor_date(date, unit = "week", week_start = 1)
  )

# Create a dataset which includes country, region, coordinates, 
# first day of every week,
# mean value of egg count in that week (NA if week is not observed), 
# elevation and distance to coast.
abundance_weekly <- mosquito_climate %>%
  group_by(
    Country, Region, latitude, longitude, week_start
  ) %>%
  summarise(
    abundance = if (all(is.na(value))) {
      NA_real_
    } else {
      mean(value, na.rm = TRUE)
    },
    elevation = first(elevation),
    dist_coast_km = first(dist_coast_km),
    .groups = "drop"
  )

# Keep one weather record per location per day
climate_daily <- mosquito_climate %>%
  distinct(
    Country, latitude, longitude, date, week_start,
    temperature_2m_mean,
    temperature_2m_max,
    temperature_2m_min,
    precipitation_sum,
    wind_speed_10m_max,
    relative_humidity_2m_mean
  )

# Calculate weekly climate summaries
climate_weekly <- climate_daily %>%
  group_by(Country, latitude, longitude, week_start) %>%
  summarise(
    temp_mean = mean(temperature_2m_mean, na.rm = TRUE),
    
    temp_max = if (all(is.na(temperature_2m_max))) {
      NA_real_
    } else {
      max(temperature_2m_max, na.rm = TRUE)
    },
    
    temp_min = if (all(is.na(temperature_2m_min))) {
      NA_real_
    } else {
      min(temperature_2m_min, na.rm = TRUE)
    },
    
    precipitation = if (all(is.na(precipitation_sum))) {
      NA_real_
    } else {
      sum(precipitation_sum, na.rm = TRUE)
    },
    
    wind_speed = if (all(is.na(wind_speed_10m_max))) {
      NA_real_
    } else {
      max(wind_speed_10m_max, na.rm = TRUE)
    },
    
    humidity = mean(relative_humidity_2m_mean, na.rm = TRUE),
    
    .groups = "drop"
  )

# Combine abundance and climate
mosquito_weekly <- abundance_weekly %>%
  left_join(
    climate_weekly,
    by = c("Country", "latitude", "longitude", "week_start")
  ) %>%
  mutate(
    year = isoyear(week_start),
    week = isoweek(week_start)
  ) %>%
  arrange(Country, latitude, longitude, week_start)


# Add extra rows for every week with no observations
mosquito_weekly_complete <- mosquito_weekly %>%
  group_by(Country, latitude, longitude) %>%
  complete(
    week_start = seq.Date(
      min(week_start),
      max(week_start),
      by = "week"
    )
  ) %>%
  ungroup() %>%
  mutate(
    year = isoyear(week_start),
    week = isoweek(week_start)
  ) %>%
  arrange(Country, latitude, longitude, week_start)


# Prepare the daily climate data
climate_daily_full <- climate_full %>%
  mutate(
    date = as.Date(time),
    week_start = floor_date(date, unit = "week", week_start = 1)
  )

# Summarise the weather for each location and calendar week
climate_weekly_full <- climate_daily_full %>%
  group_by(lat, lon, week_start) %>%
  summarise(
    temp_mean = mean(temperature_2m_mean, na.rm = TRUE),
    
    temp_max = if (all(is.na(temperature_2m_max))) {
      NA_real_
    } else {
      max(temperature_2m_max, na.rm = TRUE)
    },
    
    temp_min = if (all(is.na(temperature_2m_min))) {
      NA_real_
    } else {
      min(temperature_2m_min, na.rm = TRUE)
    },
    
    precipitation = if (all(is.na(precipitation_sum))) {
      NA_real_
    } else {
      sum(precipitation_sum, na.rm = TRUE)
    },
    
    wind_speed = if (all(is.na(wind_speed_10m_max))) {
      NA_real_
    } else {
      max(wind_speed_10m_max, na.rm = TRUE)
    },
    
    humidity = mean(relative_humidity_2m_mean, na.rm = TRUE),
    
    .groups = "drop"
  )

mosquito_weekly <- mosquito_weekly_complete %>%
  select(
    -temp_mean, -temp_max, -temp_min,
    -precipitation, -wind_speed, -humidity
  ) %>%
  left_join(
    climate_weekly_full,
    by = c(
      "latitude" = "lat",
      "longitude" = "lon",
      "week_start" = "week_start"
    )
  ) %>%
  arrange(Country, latitude, longitude, week_start)

mosquito_weekly_lags <- mosquito_weekly %>%
  group_by(Country, latitude, longitude) %>%
  arrange(week_start, .by_group = TRUE) %>%
  mutate(

    temp_mean_lag1 = lag(temp_mean, 1),
    temp_mean_lag2 = lag(temp_mean, 2),
    temp_mean_lag3 = lag(temp_mean, 3),
    temp_mean_lag4 = lag(temp_mean, 4),
    
    temp_max_lag1 = lag(temp_max, 1),
    temp_max_lag2 = lag(temp_max, 2),
    temp_max_lag3 = lag(temp_max, 3),
    temp_max_lag4 = lag(temp_max, 4),
    
    temp_min_lag1 = lag(temp_min, 1),
    temp_min_lag2 = lag(temp_min, 2),
    temp_min_lag3 = lag(temp_min, 3),
    temp_min_lag4 = lag(temp_min, 4),

    precipitation_lag1 = lag(precipitation, 1),
    precipitation_lag2 = lag(precipitation, 2),
    precipitation_lag3 = lag(precipitation, 3),
    precipitation_lag4 = lag(precipitation, 4),

    humidity_lag1 = lag(humidity, 1),
    humidity_lag2 = lag(humidity, 2),
    humidity_lag3 = lag(humidity, 3),
    humidity_lag4 = lag(humidity, 4),

    wind_speed_lag1 = lag(wind_speed, 1),
    wind_speed_lag2 = lag(wind_speed, 2),
    wind_speed_lag3 = lag(wind_speed, 3),
    wind_speed_lag4 = lag(wind_speed, 4)
  ) %>%
  ungroup()

# Replace the NA values in the descriptive columns of the non-observed dates
mosquito_weekly_lags <- mosquito_weekly_lags %>%
  group_by(Country, latitude, longitude) %>%
  mutate(
    elevation = if (all(is.na(elevation))) {
      NA_real_
    } else {
      first(elevation[!is.na(elevation)])
    },
    dist_coast_km = if (all(is.na(dist_coast_km))) {
      NA_real_
    } else {
      first(dist_coast_km[!is.na(dist_coast_km)])
    },
    Region = if (all(is.na(Region))) {
      NA_real_
    } else {
      first(Region[!is.na(Region)])
    }
  ) %>%
  ungroup()

# Create a new dataset with four new columns:
# 2 representing the broad annual variation 
# 2 representing shorter seasonal variation
mosquito_weekly_fourier <- mosquito_weekly_lags %>%
  mutate(
    week_of_year = lubridate::isoweek(week_start),
    week_of_year = pmin(week_of_year, 52),
    
    S1 = sin(2 * pi * week_of_year / 52),
    C1 = cos(2 * pi * week_of_year / 52),
    
    S2 = sin(4 * pi * week_of_year / 52),
    C2 = cos(4 * pi * week_of_year / 52)
  )