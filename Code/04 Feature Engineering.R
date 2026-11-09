library(dplyr)
library(lubridate)
library(tidyr)
if(!require("slider"))install.packages("slider")
library(slider)

# Define when the week starts are for every observations
mosquito_daily <- mosquito_climate %>%
  mutate(
    date = as.Date(date),
    week_start = floor_date(date, unit = "week", week_start = 1)
  )

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
  ) %>%
  arrange(Country, latitude, longitude, week_start)

# Add extra rows for every week with no observations.
# This will add 16400 rows to the dataset
complete_weekly_abundance <- abundance_weekly %>%
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

# Prepare the daily climate data with a week start indicator.
full_climate_daily <- climate %>%
  mutate(
    date = as.Date(time),
    week_start = floor_date(date, unit = "week", week_start = 1)
  )

# Summarise the weather for each location and calendar week
full_climate_weekly <- full_climate_daily %>%
  group_by(lat, lon, week_start) %>%
  summarise(
    med_temp       = median(temperature_2m_mean, na.rm = TRUE),
    max_temp       = median(temperature_2m_max, na.rm = TRUE),
    min_temp       = median(temperature_2m_min, na.rm = TRUE),
    precipitation  = sum(precipitation_sum, na.rm = TRUE),
    max_wind_speed = median(wind_speed_10m_max, na.rm = TRUE),
    max_humidity   = median(relative_humidity_2m_max, na.rm = TRUE),
    min_humidity   = median(relative_humidity_2m_min, na.rm = TRUE),
    med_humidity   = median(relative_humidity_2m_mean, na.rm = TRUE),
    photoperiod    = median(daylight_duration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # make sure every location has every week between its first and last
  group_by(lat, lon) %>%
  complete(week_start = seq.Date(min(week_start), max(week_start), by = "week")) %>%
  ungroup()

# Biological threshold: Ae. albopictus lower developmental zero ~10.4 °C.
# Temperatures below it are set to 0 (NAs stay NA).
temp_threshold <- 10.4

full_climate_weekly <- full_climate_weekly %>%
  mutate(
    across(
      c(med_temp, max_temp, min_temp),
      ~ if_else(.x < temp_threshold, 0, .x),
      .names = "{.col}_threshold"
    )
  )

# Window = current week + the (n - 1) weeks before it, based on dates.
moving_median <- function(x, index, n) {
  slide_index_dbl(
    x, index,
    ~ median(.x, na.rm = TRUE),
    .before = lubridate::days(7 * (n - 1)),
    .complete = TRUE
  )
}

mm_vars <- c(
  "med_temp", "max_temp", "min_temp",
  "max_wind_speed",
  "max_humidity", "min_humidity", "med_humidity",
  "photoperiod"
)

full_climate_weekly <- full_climate_weekly %>%
  group_by(lat, lon) %>%
  arrange(week_start, .by_group = TRUE) %>%
  mutate(
    across(
      all_of(mm_vars),
      list(
        MM2 = ~ moving_median(.x, week_start, 2),
        MM3 = ~ moving_median(.x, week_start, 3)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()

temp_vars <- c(
  "med_temp", "max_temp", "min_temp",
  "med_temp_MM2", "max_temp_MM2", "min_temp_MM2",
  "med_temp_MM3", "max_temp_MM3", "min_temp_MM3"
)

full_climate_weekly <- full_climate_weekly %>%
  mutate(
    across(
      all_of(temp_vars),
      ~ if_else(.x < temp_threshold, 0, .x),
      .names = "{.col}_threshold"
    )
  )

# Now we can add on to complete_weekly_abundance by joining it with climate data.
complete_mosquito_weekly <- complete_weekly_abundance %>%
  left_join(
    full_climate_weekly,
    by = c("latitude" = "lat", "longitude" = "lon", "week_start" = "week_start")
  ) %>%
  arrange(Country, latitude, longitude, week_start)


# Calculate the precipitation sums and auto-regressive variables:
lags_mosquito_weekly <- complete_mosquito_weekly %>%
  group_by(Country, latitude, longitude) %>%
  arrange(week_start, .by_group = TRUE) %>%
  mutate(
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
      NA_character_
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

ARdataset <- mosquito_weekly_fourier[complete.cases(mosquito_weekly_fourier[, c("abundance")]),]
