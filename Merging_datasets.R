library(dplyr)

climate_all$time <- as.Date(climate_all$time)

climate_clean <- climate_all %>%
  select(
    lat,
    lon,
    time,
    temperature_2m_mean,
    temperature_2m_max,
    temperature_2m_min,
    wind_speed_10m_max,
    daylight_duration,
    precipitation_sum,
    dist_coast_km,
    elevation
  )

mosquito_climate <- mosquitos %>%
  left_join(
    climate_clean,
    by = c(
      "latitude" = "lat",
      "longitude" = "lon",
      "date" = "time"
    )
  )

dim(mosquito_climate)
head(mosquito_climate)
