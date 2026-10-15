# Downloading the climate data to go alongside the mosquito dataset
# ---------------------------------------------------------------------------
# Daily climate data (2010-2022) for 149 locations, downloaded from the
# Open-Meteo Historical Weather API:
#   https://open-meteo.com/en/docs/historical-weather-api
# Data downloaded on 13 Oct 2026.
#
# Also adds:
#   - point elevation (Open-Meteo Elevation API, 90 m DEM)
#   - distance to coast (Natural Earth 1:50m coastline)
#
# Input : open_meteo_requests.txt  (headerless: lat, lon, , , start, end)
# Output: climate_all.csv
#
# Re-running is safe: locations already saved in out/ are not downloaded again.
# ---------------------------------------------------------------------------

library(httr2)
library(dplyr)
library(readr)
library(purrr)
library(sf)
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

stopifnot(length(list.files("out")) == nrow(locs))   

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

summary(locs$dist_coast_km)  

# 7. Merge and save ----------------------------------------------------------
# grid_elevation is dropped: it is identical to the point elevation from the
# Elevation API (both use the 90 m DEM).
final <- all_data |>
  select(-grid_elevation) |>
  left_join(select(locs, id, elevation, dist_coast_km), by = "id")

write_csv(final, "climate_all.csv")
 
# 8. Merge with mosquito dataset ----------------------------------------------------------
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