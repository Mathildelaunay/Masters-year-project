library(dplyr)
library(lubridate)
library(tidyr)

# Create a dataset which includes country, region, coordinates, 
# first day of every week, mean value of egg count in that week, 
# elevation and distance to coast.
abundance_weekly <- mosquito_daily %>%
  group_by(
    Country, Region, latitude, longitude, week_start
  ) %>%
  summarise(
    abundance = if (all(is.na(value))) {
      NA_real_
    } else {
      median(value, na.rm = TRUE)
    },
    elevation = first(elevation),
    dist_coast_km = first(dist_coast_km),
    .groups = "drop"
  )

# Calculate weekly climate summaries using climate_daily defined in script 03
climate_weekly <- climate_daily %>%
  group_by(Country, latitude, longitude, week_start) %>%
  summarise(
    median_temp = median(temperature_2m_mean, na.rm = TRUE),
    
    max_temp = if (all(is.na(temperature_2m_max))) {
      NA_real_
    } else {
      max(temperature_2m_max, na.rm = TRUE)
    },
    
    min_temp = if (all(is.na(temperature_2m_min))) {
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
    
    humidity = median(relative_humidity_2m_mean, na.rm = TRUE),
    
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


# Add extra rows for every week with no observations.
# This will add 16400 rows to the dataset
full_mosquito_weekly <- mosquito_weekly %>%
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


# Prepare the daily climate data with a week start inidicator.
full_climate_daily <- climate %>%
  mutate(
    date = as.Date(time),
    week_start = floor_date(date, unit = "week", week_start = 1)
  )

# Summarise the weather for each location and calendar week
full_climate_weekly <- full_climate_daily %>%
  group_by(lat, lon, week_start) %>%
  summarise(
    median_temp = median(temperature_2m_mean, na.rm = TRUE),
    
    max_temp = if (all(is.na(temperature_2m_max))) {
      NA_real_
    } else {
      max(temperature_2m_max, na.rm = TRUE)
    },
    
    min_temp = if (all(is.na(temperature_2m_min))) {
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
    
    humidity = median(relative_humidity_2m_mean, na.rm = TRUE),
    
    .groups = "drop"
  )

# Now we can add on to full_mosquito_weekly by joining it with climate data.
complete_mosquito_weekly <- full_mosquito_weekly %>%
  select(
    -median_temp, -max_temp, -min_temp,
    -precipitation, -wind_speed, -humidity
  ) %>%
  left_join(
    full_climate_weekly,
    by = c(
      "latitude" = "lat",
      "longitude" = "lon",
      "week_start" = "week_start"
    )
  ) %>%
  arrange(Country, latitude, longitude, week_start)

# Calculate all of the lags to include as potential variables:
lags_mosquito_weekly <- complete_mosquito_weekly %>%
  group_by(Country, latitude, longitude) %>%
  arrange(week_start, .by_group = TRUE) %>%
  mutate(
    
    median_temp_lag1 = lag(median_temp, 1),
    median_temp_lag2 = lag(median_temp, 2),
    median_temp_lag3 = lag(median_temp, 3),
    median_temp_lag4 = lag(median_temp, 4),
    
    max_temp_lag1 = lag(max_temp, 1),
    max_temp_lag2 = lag(max_temp, 2),
    max_temp_lag3 = lag(max_temp, 3),
    max_temp_lag4 = lag(max_temp, 4),
    
    min_temp_lag1 = lag(min_temp, 1),
    min_temp_lag2 = lag(min_temp, 2),
    min_temp_lag3 = lag(min_temp, 3),
    min_temp_lag4 = lag(min_temp, 4),
    
    # Precipitation accumulated over windows including the current week
    precipitation_2w = precipitation +
      lag(precipitation, 1),
    
    precipitation_3w = precipitation +
      lag(precipitation, 1) +
      lag(precipitation, 2),
    
    precipitation_4w = precipitation +
      lag(precipitation, 1) +
      lag(precipitation, 2) +
      lag(precipitation, 3),
    
    humidity_lag1 = lag(humidity, 1),
    humidity_lag2 = lag(humidity, 2),
    humidity_lag3 = lag(humidity, 3),
    humidity_lag4 = lag(humidity, 4),
    
    wind_speed_lag1 = lag(wind_speed, 1),
    wind_speed_lag2 = lag(wind_speed, 2),
    wind_speed_lag3 = lag(wind_speed, 3),
    wind_speed_lag4 = lag(wind_speed, 4), 
    
    AR1 = lag(abundance, 1),
    AR2 = lag(abundance, 2)
  ) %>%
  ungroup()

# Add the other variables to complete the dataset and also
# replace the NA values in the descriptive columns of the non-observed dates:
lags_mosquito_weekly <- lags_mosquito_weekly %>%
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
mosquito_weekly_fourier <- lags_mosquito_weekly %>%
  mutate(
    week_of_year = lubridate::isoweek(week_start),
    week_of_year = pmin(week_of_year, 52),
    
    S1 = sin(2 * pi * week_of_year / 52),
    C1 = cos(2 * pi * week_of_year / 52),
    
    S2 = sin(4 * pi * week_of_year / 52),
    C2 = cos(4 * pi * week_of_year / 52)
  )