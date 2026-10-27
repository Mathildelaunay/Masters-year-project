# Downloading the climate data to go alongside the mosquito dataset

# Daily climate data (2010-2022) for 149 locations, downloaded from the
# Open-Meteo Historical Weather API: 
#   Zippenfenig, P. (2023) Open-Meteo.com Weather API. Zenodo. doi: 10.5281/ZENODO.7970649.
#   https://open-meteo.com/en/docs/historical-weather-api
#
# Data in the analysed dataset were downloaded in two batches (13 Oct 2026 and
# 17 Oct 2026)
#
# Variables (daily):
#   temperature (mean, max, min) in °C, max wind speed in km/h, daylight duration in seconds,
#   precipitation sum in mm/day, relative humidity (mean, max, min) in %.
# Also adds:
#   - elevation in m (Open-Meteo Elevation API, 90 m DEM)
#   - distance to coast in km (Natural Earth 1:50m coastline)
#
# Input : the mosquito data frame (columns: latitude, longitude, date), which
#         must be loaded before running this script. From it, the script builds
#         open_meteo_requests.txt (one row per location, with that location's
#         first and last sampling date).
# Output: climate_all.csv

library(httr2)
library(dplyr)
library(readr)
library(purrr)
library(sf)
library(lubridate)
library(tidyr)
library(rnaturalearth)
if(!require("rnaturalearth"))install.packages("rnaturalearth")
if(!require("rnaturalearthdata"))install.packages("rnaturalearthdata")

# 1. Locations ---------------------------------------------------------------
locs <- read_csv(
  "open_meteo_requests.txt",
  col_names = c("lat", "lon", "blank1", "blank2", "start", "end"),
  col_types = cols(lat = "d", lon = "d", blank1 = "c", blank2 = "c",
                   start = col_date(), end = col_date())
) |>
  select(-blank1, -blank2) |>
  mutate(id = sprintf("%03d", row_number()), .before = 1)

dir.create("out", showWarnings = FALSE)

daily_vars <- paste(
  "temperature_2m_mean", "temperature_2m_max", "temperature_2m_min",
  "wind_speed_10m_max", "daylight_duration", "precipitation_sum",
  sep = ","
)

# 2. Download functions ------------------------------------------------------
fetch_one <- function(id, lat, lon, start, end) {
  f <- file.path("out", paste0("loc_", id, ".csv"))
  if (file.exists(f)) return(invisible(NULL))   # resume support
  
  resp <- request("https://archive-api.open-meteo.com/v1/archive") |>
    req_url_query(
      latitude = lat, longitude = lon,
      start_date = as.character(start), end_date = as.character(end),
      daily = daily_vars,
      timezone = "auto"
    ) |>
    req_retry(
      max_tries = 6,
      is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
      backoff = \(i) 20 * i
    ) |>
    req_perform()
  
  j <- resp_body_json(resp, simplifyVector = TRUE)
  
  as_tibble(j$daily) |>
    mutate(id = id, lat = lat, lon = lon,
           grid_elevation = j$elevation, .before = 1) |>
    write_csv(f)
  
  Sys.sleep(1)
}

safe_fetch <- safely(fetch_one)

# Wait and retry on rate limits / temporary server errors
fetch_with_wait <- function(id, lat, lon, start, end, max_waits = 12) {
  for (k in seq_len(max_waits)) {
    out <- safe_fetch(id, lat, lon, start, end)
    if (is.null(out$error)) return(out)
    msg <- conditionMessage(out$error)
    if (grepl("429|limit|50[0-4]|Gateway|Unavailable", msg, ignore.case = TRUE)) {
      message("Temporary error at ", id, " (", msg, "); sleeping 5 min...")
      Sys.sleep(300)
    } else {
      message("Other error at ", id, ": ", msg)
      return(out)
    }
  }
  out
}

# 3. Download everything that is still missing -------------------------------
todo <- locs |>
  filter(!file.exists(file.path("out", paste0("loc_", id, ".csv"))))

message(nrow(todo), " locations left to download")

res <- pmap(
  list(todo$id, todo$lat, todo$lon, todo$start, todo$end),
  \(id, lat, lon, start, end) {
    message("Fetching ", id)
    fetch_with_wait(id, lat, lon, start, end)
  }
)

stopifnot(length(list.files("out")) == nrow(locs))   # should be 149

# 4. Combine the per-location files -----------------------------------------
all_data <- list.files("out", full.names = TRUE) |>
  map(\(f) read_csv(f, col_types = cols(id = col_character(),
                                        .default = col_guess()))) |>
  bind_rows()

# 5. Point elevation (up to 100 coordinates per call) ------------------------
get_elev <- function(d) {
  request("https://api.open-meteo.com/v1/elevation") |>
    req_url_query(
      latitude  = paste(d$lat, collapse = ","),
      longitude = paste(d$lon, collapse = ",")
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("elevation")
}

locs$elevation <- locs |>
  mutate(chunk = (row_number() - 1) %/% 100) |>
  group_split(chunk) |>
  map(get_elev) |>
  unlist()

# 6. Distance to coast -------------------------------------------------------
pts <- st_as_sf(locs, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(3035)    # metric projection for Europe

coast <- ne_coastline(scale = 50, returnclass = "sf") |>
  st_transform(3035) |>
  st_union()

locs$dist_coast_km <- as.numeric(st_distance(pts, coast)) / 1000

# 7. Merge and save ----------------------------------------------------------
# grid_elevation is dropped: it is identical to the point elevation from the
# Elevation API (both use the 90 m DEM).
final_without_humidity <- all_data |>
  select(-grid_elevation) |>
  left_join(select(locs, id, elevation, dist_coast_km), by = "id")

#write_csv(final_without_humidity, "climate_without_humidity.csv")    un-tag if not done yet

# Adding humidity data

dir.create("out_humidity", showWarnings = FALSE)

fetch_humidity <- function(id, lat, lon, start, end) {
  f <- file.path("out_humidity", paste0("hum_", id, ".csv"))
  if (file.exists(f)) return(invisible(NULL)) # Safe to run the code again
  
  resp <- request("https://archive-api.open-meteo.com/v1/archive") |>
    req_url_query(
      latitude = lat, longitude = lon,
      start_date = as.character(start), end_date = as.character(end),
      daily = "relative_humidity_2m_mean,relative_humidity_2m_max,relative_humidity_2m_min",
      timezone = "auto"
    ) |>
    req_retry(
      max_tries = 6,
      is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
      backoff = \(i) 20 * i
    ) |>
    req_perform()
  
  j <- resp_body_json(resp, simplifyVector = TRUE)
  
  as_tibble(j$daily) |>
    mutate(id = id, .before = 1) |>
    write_csv(f)
  
  Sys.sleep(1)
}

safe_hum <- safely(fetch_humidity)

pwalk(
  list(locs$id, locs$lat, locs$lon, locs$start, locs$end),
  \(id, lat, lon, start, end) {
    message("Fetching ", id)
    for (k in 1:12) {
      out <- safe_hum(id, lat, lon, start, end)
      if (is.null(out$error)) break
      message("Error at ", id, ": ", conditionMessage(out$error), "; sleeping 5 min...")
      Sys.sleep(300)
    }
  }
)

humidity <- list.files("out_humidity", full.names = TRUE) |>
  map(\(f) read_csv(f, col_types = cols(id = col_character(), .default = col_guess()))) |>
  bind_rows() |>
  mutate(time = as.Date(time))

climate_full <- read_csv("climate_without_humidity.csv",
                         col_types = cols(id = col_character(), .default = col_guess())) |>
  mutate(time = as.Date(time)) |>
  left_join(humidity, by = c("id", "time"))

#write_csv(climate_full, "climate.csv")     #un-tag if not downloaded yet


# Next, merge the dataset containing all of the climate data with the mosquito dataset

#climate <- read_csv("climate.csv")   un-tag if the file has no been imported

climate$time <- as.Date(climate$time)

climate <- climate %>%
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
    elevation,
    relative_humidity_2m_mean,
    relative_humidity_2m_max,
    relative_humidity_2m_min
  )

mosquito_climate <- mosquito %>%
  left_join(
    climate,
    by = c(
      "latitude" = "lat",
      "longitude" = "lon",
      "date" = "time"
    )
  )

library(dplyr)
library(ggplot2)
library(lubridate)

# Summary of daily climate data by location and date
climate_daily <- mosquito_climate %>%
  distinct(
    Country, latitude, longitude, date,
    temperature_2m_mean,
    precipitation_sum,
    wind_speed_10m_max,
    daylight_duration,
    relative_humidity_2m_mean
  ) %>%
  mutate(month = month(date))

# Monthly averages for each country
climate_monthly <- climate_daily %>%
  group_by(Country, month) %>%
  summarise(
    temperature = mean(temperature_2m_mean, na.rm = TRUE),
    precipitation = mean(precipitation_sum, na.rm = TRUE),
    wind_speed = mean(wind_speed_10m_max, na.rm = TRUE),
    daylight_hours = mean(daylight_duration, na.rm = TRUE) / 3600,
    humidity = mean(relative_humidity_2m_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Create a seasonal mean monthly abundance dataset
abundance_monthly_seasonal <- mosquito %>%
  mutate(month = lubridate::month(date)) %>%
  group_by(Country, month) %>%
  summarise(
    abundance = if (all(is.na(value))) {
      NA_real_
    } else {
      mean(value, na.rm = TRUE)
    },
    .groups = "drop"
  )

# Merge mosquito abundance with climate summaries
seasonal_data <- abundance_monthly_seasonal %>%
  left_join(climate_monthly, by = c("Country", "month"))

# Create a function to plot abundance against a climate variable
plot_seasonal <- function(data, climate_var, climate_label) {
  
  scale_factor <- max(data$abundance, na.rm = TRUE) /
    max(data[[climate_var]], na.rm = TRUE)
  
  plot_data <- data %>%
    mutate(climate_scaled = .data[[climate_var]] * scale_factor)
  
  ggplot(plot_data, aes(x = month)) +
    geom_line(
      aes(y = abundance, colour = "Mean egg abundance"),
      linewidth = 1
    ) +
    geom_point(
      aes(y = abundance, colour = "Mean egg abundance")
    ) +
    geom_line(
      aes(y = climate_scaled, colour = climate_label),
      linewidth = 1
    ) +
    geom_point(
      aes(y = climate_scaled, colour = climate_label)
    ) +
    facet_wrap(~ Country) +
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb
    ) +
    scale_y_continuous(
      name = "Mean egg abundance",
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = climate_label
      )
    ) +
    scale_colour_manual(
      values = c(
        "Mean egg abundance" = "black",
        setNames("red", climate_label)
      )
    ) +
    labs(
      title = paste(
        "Seasonal mosquito abundance and",
        tolower(climate_label)
      ),
      x = "Month",
      colour = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Output the plots for visualisation
p_temperature <- plot_seasonal(
  seasonal_data, "temperature", "Mean temperature (°C)"
)

p_precipitation <- plot_seasonal(
  seasonal_data, "precipitation",
  "Mean daily precipitation (mm/day)"
)

p_humidity <- plot_seasonal(
  seasonal_data, "humidity", "Mean relative humidity (%)"
)

p_daylight <- plot_seasonal(
  seasonal_data, "daylight_hours", "Daylight duration (hours)"
)

p_wind <- plot_seasonal(
  seasonal_data, "wind_speed",
  "Mean daily maximum wind speed (m/s)"
)
p_precipitation
# Save the plots as a pdf

pdf(
  "visualisation_2.pdf",
  width = 10,
  height = 6
)

print(p_temperature)
print(p_precipitation)
print(p_humidity)
print(p_daylight)
print(p_wind)

dev.off()

# This is an average across the available records, so locations with more 
# recorded days can contribute more heavily than locations with fewer records. 
# It is not necessarily an equally weighted average of each location.

# Want to get a list of the first day of the week for all the dates with observations
mosquito_daily <- mosquito_climate %>%
  mutate(
    date = as.Date(date),
    week_start = floor_date(date, unit = "week", week_start = 1)
  )

# Create a dataset which includes country, region, coordinates, 
# first day of every week,
# mean value of egg count in that week (NA if week is not observed), 
# elevation and distance to coast.
abundance_weekly <- mosquito_daily %>%
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
climate_daily <- mosquito_daily %>%
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

# Check for any NA values
mosquito_weekly_complete %>%
  summarise(
    total_rows = n(),
    missing_abundance = sum(is.na(abundance)),
    missing_temp = sum(is.na(temp_mean)),
    missing_precipitation = sum(is.na(precipitation)),
    missing_humidity = sum(is.na(humidity))
  )


# Prepare the daily climate data
climate_daily_full <- climate %>%
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

mosquito_weekly_final <- mosquito_weekly_complete %>%
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

mosquito_weekly_lags <- mosquito_weekly_final %>%
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