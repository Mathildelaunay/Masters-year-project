library(dplyr)
library(ggplot2)
library(lubridate)

# Select a year for visualisation
selected_year <- 2018

# Want to get a list of the first day of the week for all the dates with observations
mosquito_daily <- mosquito_climate %>%
  mutate(
    date = as.Date(date),
    week_start = floor_date(date, unit = "week", week_start = 1)
  ) # Using floor_date here is what extracts the first day of the week

climate_daily <- mosquito_daily %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  ) %>%
  distinct(
    Country, latitude, longitude, date, year, month, week_start,
    temperature_2m_mean,
    precipitation_sum,
    wind_speed_10m_max,
    daylight_duration,
    relative_humidity_2m_mean
  ) 
# This is done because there are some days which have more than 1 observations.
# This prevents from counting climate variables twice.
# This is important for the following calculations:

# Weekly median climate values within the selected year
# Only visualise weeks in April-August
EDA_climate_weekly <- climate_daily %>%
  filter(year == selected_year,
         month >= 4,
         month <= 8) %>%
  group_by(Country, month, week_start) %>%
  summarise(
    temperature = median(temperature_2m_mean, na.rm = TRUE),
    precipitation = median(precipitation_sum, na.rm = TRUE),
    wind_speed = median(wind_speed_10m_max, na.rm = TRUE),
    daylight_hours = median(daylight_duration, na.rm = TRUE) / 3600,
    humidity = median(relative_humidity_2m_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Weekly median mosquito abundance within the selected year, April-August
EDA_abundance_weekly <- mosquito_daily %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  filter(year == selected_year,
         month >= 4,
         month <= 8) %>%
  group_by(Country, month, week_start) %>%
  summarise(
    abundance = if (all(is.na(value))) {
      NA_real_
    } else {
      median(value, na.rm = TRUE)
    },
    .groups = "drop"
  )

# Combine abundance and climate data
EDA_Mosquito_climate_weekly <- EDA_abundance_weekly %>%
  left_join(EDA_climate_weekly, by = c("Country", "week_start"))

# Create a function to plot abundance against a climate variable
plot_weekly <- function(data, climate_var, climate_label) {
  
  scale_factor <- max(data$abundance, na.rm = TRUE) /
    max(data[[climate_var]], na.rm = TRUE)
  
  plot_data <- data %>%
    mutate(climate_scaled = .data[[climate_var]] * scale_factor)
  
  ggplot(plot_data, aes(x = week_start)) +
    geom_line(
      aes(y = abundance, colour = "Median mosquito abundance"),
      linewidth = 1
    ) +
    geom_point(
      aes(y = abundance, colour = "Median mosquito abundance")
    ) +
    geom_line(
      aes(y = climate_scaled, colour = climate_label),
      linewidth = 1
    ) +
    geom_point(
      aes(y = climate_scaled, colour = climate_label)
    ) +
    facet_wrap(~ Country) +
    
    scale_y_continuous(
      name = "Median mosquito abundance",
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = climate_label
      )
    ) +
    scale_colour_manual(
      values = c(
        "Median mosquito abundance" = "black",
        setNames("red", climate_label)
      )
    ) +
    labs(
      title = paste(
        "Weekly median mosquito abundance and",
        tolower(climate_label)
      ),
      x = "Week",
      colour = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Output the plots for visualisation
p_temperature <- plot_weekly(
  EDA_Mosquito_climate_weekly, "temperature", "Median temperature (°C)"
)

p_precipitation <- plot_weekly(
  EDA_Mosquito_climate_weekly, "precipitation",
  "Median daily precipitation (mm/day)"
)

p_humidity <- plot_weekly(
  EDA_Mosquito_climate_weekly, "humidity", "Median relative humidity (%)"
)

p_daylight <- plot_weekly(
  EDA_Mosquito_climate_weekly, "daylight_hours", "Median daylight duration (hours)"
)

p_wind <- plot_weekly(
  EDA_Mosquito_climate_weekly, "wind_speed",
  "Median daily maximum wind speed (m/s)"
)