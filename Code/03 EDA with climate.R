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
# This is done because there are some days which have more than 1 observations.
# This prevents from counting climate variables twice.
# This is important for the following calculations:

# Monthly calculations for the visualisation
climate_monthly <- climate_daily %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(Country, year, month) %>%
  summarise(
    temperature = median(temperature_2m_mean, na.rm = TRUE),
    precipitation = median(precipitation_sum, na.rm = TRUE),
    wind_speed = median(wind_speed_10m_max, na.rm = TRUE),
    daylight_hours = median(daylight_duration, na.rm = TRUE) / 3600,
    humidity = median(relative_humidity_2m_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Country, month) %>%
  summarise(
    temperature = median(temperature, na.rm = TRUE),
    precipitation = median(precipitation, na.rm = TRUE),
    wind_speed = median(wind_speed, na.rm = TRUE),
    daylight_hours = median(daylight_hours, na.rm = TRUE),
    humidity = median(humidity, na.rm = TRUE),
    .groups = "drop"
  )

# Create a seasonal median monthly abundance dataset
abundance_monthly_seasonal <- mosquito %>%
  mutate(month = lubridate::month(date)) %>%
  group_by(Country, month) %>%
  summarise(
    abundance = if (all(is.na(value))) {
      NA_real_
    } else {
      median(value, na.rm = TRUE)
    },
    .groups = "drop"
  )

# Merge monthly median mosquito abundance with climate monthly summaries
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
      aes(y = abundance, colour = "Median egg abundance"),
      linewidth = 1
    ) +
    geom_point(
      aes(y = abundance, colour = "Median egg abundance")
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
      name = "Median egg abundance",
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = climate_label
      )
    ) +
    scale_colour_manual(
      values = c(
        "Median egg abundance" = "black",
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
  seasonal_data, "temperature", "Median temperature (°C)"
)

p_precipitation <- plot_seasonal(
  seasonal_data, "precipitation",
  "Median precipitation (mm/day)"
)

p_humidity <- plot_seasonal(
  seasonal_data, "humidity", "Median relative humidity (%)"
)

p_daylight <- plot_seasonal(
  seasonal_data, "daylight_hours", "Median daylight duration (hours)"
)

p_wind <- plot_seasonal(
  seasonal_data, "wind_speed",
  "Median daily maximum wind speed (m/s)"
)

pdf(
  "Feature Engineering Motivation.pdf",
  width = 10,
  height = 6
)

print(p_temperature)
print(p_precipitation)
print(p_humidity)
print(p_daylight)
print(p_wind)

dev.off()
