install.packages("patchwork")
library(patchwork)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#ARdataset <- read_csv("ARdataset.csv")

# Check abundance distribution in the cold months
winter_abundance <- ARdataset %>%
  mutate(
    month_num = lubridate::month(week_start)
  ) %>%
  filter(month_num %in% c(11, 12, 1, 2, 3)) %>%
  group_by(Country, month_num) %>%
  summarise(
    median_abundance = median(abundance, na.rm = TRUE),
    max_abundance = max(abundance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month = factor(
      month.abb[month_num],
      levels = month.abb[c(11, 12, 1, 2, 3)]
    )
  )

# Plot
ggplot(
  winter_abundance,
  aes(x = month, y = max_abundance, group = Country)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "red"
  ) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(
    title = "Maximum mosquito abundance during cold months",
    x = "Month",
    y = "Maximum weekly egg abundance"
  ) +
  theme_minimal()

# Looking at yearly plots when including feature engineering--------------------
# Function to calculate median while accounting for missing values
safe_median <- function(x) {
  if (all(is.na(x))) NA_real_
  else median(x, na.rm = TRUE)
}

# Variables needed for the plots
plot_vars <- c(
  # Temperature
  "med_temp", "max_temp", "min_temp",
  paste0(rep(c("med_temp", "max_temp", "min_temp"), each = 2),
         c("_MM2", "_MM3")),
  unlist(lapply(c("med_temp", "max_temp", "min_temp"),
                function(x) paste0(x, "_lag", 1:3))),
  
  # Humidity
  "med_humidity", "max_humidity", "min_humidity",
  unlist(lapply(c("med_humidity", "max_humidity", "min_humidity"),
                function(x) paste0(x, c("_MM2", "_MM3")))),
  unlist(lapply(c("med_humidity", "max_humidity", "min_humidity"),
                function(x) paste0(x, "_lag", 1:3))),
  
  # Photoperiod
  "photoperiod", "photoperiod_MM2", "photoperiod_MM3",
  paste0("photoperiod_lag", 1:3),
  
  # Wind
  "max_wind_speed", "max_wind_speed_MM2", "max_wind_speed_MM3",
  paste0("wind_speed_lag", 1:3),
  
  # Precipitation
  "precipitation", paste0("precipitation_", 2:4, "w"),
  
  # Fourier terms
  "S1", "C1", "S2", "C2"
)

plot_vars <- unique(plot_vars)

# Check that all variables exist
stopifnot(all(plot_vars %in% names(ARdataset)))

# Monthly summaries for each year
monthly_data <- ARdataset %>%
  mutate(
    month = month(week_start),
    calendar_year = year(week_start)
  ) %>%
  group_by(Country, calendar_year, month) %>%
  summarise(
    abundance = safe_median(abundance),
    across(all_of(plot_vars), safe_median),
    .groups = "drop"
  ) %>%
  group_by(Country, month) %>%
  summarise(
    abundance = safe_median(abundance),
    across(all_of(plot_vars), safe_median),
    .groups = "drop"
  )

# Converting photperiod from seconds to hours
monthly_data <- monthly_data %>%
  mutate(
    across(
      starts_with("photoperiod"),
      ~ .x / 3600
    )
    
# Create a plotting function    
plot_climate <- function(data, variables, title, y_label) {

  plot_data <- data %>%
    select(Country, month, all_of(variables)) %>%
    pivot_longer(
      cols = all_of(variables),
      names_to = "Variable",
      values_to = "Value"
    )

  # Abundance range
  abundance_range <- range(data$abundance, na.rm = TRUE)

  # Climate variable range
  climate_range <- range(plot_data$Value, na.rm = TRUE)

  # Expand constant ranges, if necessary
  if (diff(abundance_range) == 0) {
    abundance_range <- abundance_range + c(-0.5, 0.5)
  }

  if (diff(climate_range) == 0) {
    climate_range <- climate_range + c(-0.5, 0.5)
  }

  # Linear transformation for secondary axis
  scale_factor <- diff(abundance_range) / diff(climate_range)

  offset <- abundance_range[1] -
    climate_range[1] * scale_factor

  plot_data <- plot_data %>%
    mutate(
      Value_scaled = Value * scale_factor + offset
    )

  ggplot() +
    geom_line(
      data = data,
      aes(
        x = month,
        y = abundance,
        colour = "Abundance",
        group = Country
      ),
      linewidth = 1
    ) +
    geom_line(
      data = plot_data,
      aes(
        x = month,
        y = Value_scaled,
        colour = Variable,
        group = Variable
      ),
      linewidth = 0.8
    ) +
    facet_wrap(~ Country) +
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb
    ) +
    scale_y_continuous(
      name = "Median weekly egg abundance",
      sec.axis = sec_axis(
        ~ (. - offset) / scale_factor,
        name = y_label
      )
    ) +
    labs(
      title = title,
      x = "Month",
      colour = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

# Base variable, lag variable prefix, descriptive label and units
feature_info <- list(
  med_temp = list(
    lag = "med_temp",
    label = "Median daily mean temperature",
    unit = "Temperature (°C)"
  ),
  max_temp = list(
    lag = "max_temp",
    label = "Median daily maximum temperature",
    unit = "Temperature (°C)"
  ),
  min_temp = list(
    lag = "min_temp",
    label = "Median daily minimum temperature",
    unit = "Temperature (°C)"
  ),
  med_humidity = list(
    lag = "med_humidity",
    label = "Median daily mean humidity",
    unit = "Relative humidity (%)"
  ),
  max_humidity = list(
    lag = "max_humidity",
    label = "Median daily maximum humidity",
    unit = "Relative humidity (%)"
  ),
  min_humidity = list(
    lag = "min_humidity",
    label = "Median daily minimum humidity",
    unit = "Relative humidity (%)"
  ),
  photoperiod = list(
    lag = "photoperiod",
    label = "Photoperiod",
    unit = "Photoperiod (hours)"
  ),
  max_wind_speed = list(
    lag = "wind_speed",
    label = "Median daily maximum wind speed",
    unit = "Wind speed (km/h)"
  )
)


# Create the temperature, humidity, photoperiod and wind plots
plots <- list()

for (var in names(feature_info)) {
  
  info <- feature_info[[var]]
  
  # Original + moving medians
  mm_vars <- c(
    var,
    paste0(var, "_MM2"),
    paste0(var, "_MM3")
  )
  
  plots[[paste0(var, "_MM")]] <- plot_climate(
    monthly_data,
    mm_vars,
    paste(info$label, "- Moving Medians"),
    info$unit
  )
  
  # Original + lags
  lag_vars <- c(
    var,
    paste0(info$lag, "_lag", 1:3)
  )
  
  plots[[paste0(var, "_lags")]] <- plot_climate(
    monthly_data,
    lag_vars,
    paste(info$label, "- Lagged Variables"),
    info$unit
  )
}

# Plot for precipitation
plots[["precipitation"]] <- plot_climate(
  monthly_data,
  c(
    "precipitation",
    "precipitation_2w",
    "precipitation_3w",
    "precipitation_4w"
  ),
  "Seasonal Precipitation and Mosquito Abundance",
  "Cumulative precipitation (mm)"
)

pdf(
  "EDA_feature_groups.pdf",
  width = 11,
  height = 7,
  onefile = TRUE
)

for (plot_name in names(plots)) {
  print(plots[[plot_name]])
}

dev.off()