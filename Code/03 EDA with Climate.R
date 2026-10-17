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

# Merge mosquito abundance with climate summaries
seasonal_data <- abundance_monthly %>%
  left_join(climate_monthly, by = c("Country", "month"))

# Create a function to plot abundance against a climate variable
plot_seasonal <- function(data, climate_var, climate_label) {
  
  scale_factor <- max(data$abundance, na.rm = TRUE) /
    max(data[[climate_var]], na.rm = TRUE)
  
  plot_data <- data %>%
    mutate(climate_scaled = .data[[climate_var]] * scale_factor)
  
  ggplot(plot_data, aes(x = month)) +
    geom_line(
      aes(y = abundance, colour = "Mosquito abundance"),
      linewidth = 1
    ) +
    geom_point(
      aes(y = abundance, colour = "Mosquito abundance")
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
        "Mosquito abundance" = "black",
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