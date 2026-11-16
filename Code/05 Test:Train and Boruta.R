if(!require("ranger"))install.packages("ranger")
library(dplyr)
library(lubridate)
library(Boruta)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ranger)
library(Boruta)
library(tibble)
library(forcats)

# Create a train/test split ----------------------------------------------------
location_summary <- ARdataset %>%
  group_by(Country, latitude, longitude) %>%
  summarise(
    n_obs = n(),
    
    span_days = as.numeric(
      max(week_start) - min(week_start)
    ),
    
    n_winter = sum(
      month(week_start) %in% c(11, 12, 1, 2, 3)
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    eligible = span_days >= 365
  )

set.seed(123) # For reproducibility 

eligible_locations <- location_summary %>%
  filter(eligible)

target_obs <- 0.15 * nrow(ARdataset)

total_obs <- nrow(ARdataset)
total_winter <- sum(location_summary$n_winter)

best_difference <- Inf
best_test <- NULL

for (i in 1:10000) {
  
  # Randomly order eligible locations
  candidate <- eligible_locations %>%
    slice_sample(prop = 1) %>%
    mutate(
      cumulative_obs = cumsum(n_obs)
    )
  
  # Select locations until reaching approximately 15%
  n_select <- which.min(
    abs(candidate$cumulative_obs - target_obs)
  )
  
  candidate <- candidate %>%
    slice(1:n_select)
  
  # Check the proportion of winter observations in training
  train_obs <- total_obs - sum(candidate$n_obs)
  train_winter <- total_winter - sum(candidate$n_winter)
  
  winter_prop <- train_winter / train_obs
  
  difference <- abs(sum(candidate$n_obs) - target_obs)
  
  if (winter_prop >= 0.20 && difference < best_difference) {
    best_difference <- difference
    best_test <- candidate
  }
}

# Make the test and train datasets
if (is.null(best_test)) {
  stop("No split satisfying the winter requirement was found.")
}

test <- ARdataset %>%
  semi_join(
    best_test,
    by = c("Country", "latitude", "longitude")
  )

train <- ARdataset %>%
  anti_join(
    best_test,
    by = c("Country", "latitude", "longitude")
  )

# Percentage of observations in testing
100 * nrow(test) / nrow(ARdataset)

# Percentage of winter observations in training
100 * mean(
  month(train$week_start) %in% c(11, 12, 1, 2, 3)
)

# Number of locations in test set
nrow(best_test) # This is 20 with this seed

nrow(location_summary) - nrow(best_test)

# Verify minimum observation span of test locations
min(best_test$span_days) # Returns 721

# Check which percentage of locations made it to the train for every country 
location_split <- location_summary %>%
  mutate(
    Set = if_else(
      paste(Country, latitude, longitude) %in%
        paste(best_test$Country, best_test$latitude, best_test$longitude),
      "Test",
      "Train"
    )
  ) %>%
  count(Country, Set) %>%
  group_by(Country) %>%
  mutate(
    Percentage = round(100 * n / sum(n), 2)
  ) %>%
  ungroup()

location_split
# Albania is 25/75 Test/Train
# France is only used for Train
# Italy is 13.2/86.8 Test/Train
# Switzerland is only used for Train

# Make a map showing where the locations of the test/train sets are ------------
map_locations <- bind_rows(
  train %>%
    distinct(Country, latitude, longitude) %>%
    mutate(Set = "Train"),
  
  test %>%
    distinct(Country, latitude, longitude) %>%
    mutate(Set = "Test")
)

# European country boundaries
europe <- ne_countries(
  continent = "Europe",
  scale = "medium",
  returnclass = "sf"
)

# Convert mosquito locations to spatial points
locations_sf <- st_as_sf(
  map_locations,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Plot
map <- ggplot() +
  geom_sf(
    data = europe,
    fill = "grey95",
    colour = "grey60",
    linewidth = 0.3
  ) +
  geom_sf(
    data = locations_sf,
    aes(colour = Set),
    size = 2,
    alpha = 0.8
  ) +
  scale_colour_manual(
    values = c(
      "Train" = "steelblue",
      "Test" = "red3"
    )
  ) +
  coord_sf(
    xlim = c(5, 21),
    ylim = c(37, 48),
    expand = FALSE
  ) +
  labs(
    colour = "Dataset"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(colour = "grey85"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Save the plot
pdf(
  "Geographical Distribution of Training and Testing Locations.pdf",
  width = 10,
  height = 6
)

print(map)

dev.off()

# Split the training set into cold and warm periods ----------------------------
# Cold months: November to March
train_cold <- train %>%
  filter(month(week_start) %in% c(11, 12, 1, 2, 3))

# Warm months: April to October
train_warm <- train %>%
  filter(month(week_start) %in% 4:10)

set.seed(123) 

# Number of observations in each dataset
n_cold <- nrow(train_cold)
n_warm <- nrow(train_warm)

# Proportion of warm observations to retain
sampling_prop <- n_cold / n_warm

sampling_prop # Retains 72.5023% of the observations in the warm dataset

# Use a sub-sample of the training data on the warm months
set.seed(123)

warm_sample <- train_warm %>%
  group_by(Country, year) %>%
  slice_sample(prop = sampling_prop) %>%
  ungroup()

# Get the datasets ready for Boruta
boruta_cold_data <- train_cold %>%
  select(
    -Country, -Region,
    -latitude, -longitude,
    -week_start, -year, -week, -week_of_year,
    -AR1, -AR2
  )

boruta_warm_data <- train_warm %>%
  select(
    -Country, -Region,
    -latitude, -longitude,
    -week_start, -year, -week, -week_of_year,
    -AR1, -AR2
  )

boruta_warm_sample <- warm_sample %>%
  select(
    -Country, -Region,
    -latitude, -longitude,
    -week_start, -year, -week, -week_of_year,
    -AR1, -AR2
  )

# Run Boruta -------------------------------------------------------------------
# First run ------

set.seed(123)

boruta_cold1 <- Boruta(
  abundance ~ .,
  data = boruta_cold_data,
  maxRuns = 300,
  doTrace = 1
) 
# This took 41 minutes to run 259 iterations. 
# Still had 9 attributes left at the end.
# Rejected 31, confirmed 54.
# 54 warnings.


set.seed(123)

boruta_warm1 <- Boruta(
  abundance ~ .,
  data = boruta_warm_data,
  maxRuns = 300,
  doTrace = 1
) 
# 94 features were rejected in 14 iterations.
# Slightly unexpected.


# Try ranger ------

set.seed(123)

boruta_cold_ranger <- Boruta(
  abundance ~ .,
  data = boruta_cold_data,
  getImp = getImpRfZ,
  maxRuns = 200,
  num.trees = 200,
  num.threads = 4,
  doTrace = 1
)
# Took 6 mins.
# 53 Confirmed, 24 Rejected, 17 Tentative.
# No warnings.


set.seed(123)

boruta_warm_ranger <- Boruta(
  abundance ~ .,
  data = boruta_warm_data,
  getImp = getImpRfZ,
  maxRuns = 200,
  num.trees = 200,
  num.threads = 4,
  doTrace = 1
)
# Took 1.1 hours
# All rejected except for 4 tentatives:
# med_temp_lag2_TH10_4, min_temp_MM3, min_temp_MM3_TH10_4, min_temp_MM3_TH15.
# No warnings.

# Try again with more iterations -----

set.seed(123)

boruta_cold <- Boruta(
  abundance ~ .,
  data = boruta_cold_data,
  getImp = getImpRfZ,
  maxRuns = 300,
  num.trees = 200,
  num.threads = 4,
  doTrace = 1
)
# This ran for 14 minutes
# Confirmed 58 features, rejected 25 and classified 11 as tentative.

set.seed(123)

boruta_warm <- Boruta(
  abundance ~ .,
  data = boruta_warm_data,
  getImp = getImpRfZ,
  maxRuns = 300,
  num.trees = 200,
  num.threads = 4,
  doTrace = 1
)
# This ran for 1.7 hours
# Confirmed 92 and left 2 as tentative. 
# Took too long, use a sub-sample to see if results differ.

# Sub-sampled run -------
set.seed(123)

boruta_warm_sampled <- Boruta(
  abundance ~ .,
  data = boruta_warm_sample,
  getImp = getImpRfZ,
  maxRuns = 300,
  num.trees = 200,
  num.threads = 4,
  doTrace = 1
)
# Ran for 50 minutes
# Confirmed 88 features and left 6 as tentative.

# Save the results to avoid re-running: un-tag to save.
#saveRDS(boruta_cold, "boruta_cold_original.rds")
#saveRDS(boruta_warm_sampled, "boruta_warm_sampled_original.rds")

# Check results-----------------------------------------------------------------

cold_results <- attStats(boruta_cold) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(medianImp))

warm_results <- attStats(boruta_warm_sampled) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(medianImp))

# Compare Boruta results for warm and cold months
boruta_comparison <- full_join(
  cold_results %>%
    select(
      Variable,
      Cold = decision,
      Cold_Importance = medianImp
    ),
  
  warm_results %>%
    select(
      Variable,
      Warm = decision,
      Warm_Importance = medianImp
    ),
  
  by = "Variable"
)

# View(boruta_comparison)  # un-tag to view results table

# Check which variables were rejected for the cold months, but confirmed in the warm months
cold_rejected_warm_confirmed <- boruta_comparison %>%
  filter(
    Cold == "Rejected",
    Warm == "Confirmed"
  ) %>%
  arrange(desc(Warm_Importance))

# View(cold_rejected_warm_confirmed)  # un-tag to view results table
 
# Check which features were confirmed in both runs
confirmed_both <- boruta_comparison %>%
  filter(
    Cold == "Confirmed",
    Warm == "Confirmed"
  )

# View(confirmed_both)  # un-tag to view results table

# Check which features were categoried differently in both runs
different_results <- boruta_comparison %>%
  filter(Cold != Warm)

# View(different_results)  # un-tag to view results table

# Use TentativeRoughFix
boruta_cold_fixed <- TentativeRoughFix(boruta_cold)
boruta_warm_fixed <- TentativeRoughFix(boruta_warm_sampled)

# Compare before and after for the cold training dataset
table(boruta_cold$finalDecision)
table(boruta_cold_fixed$finalDecision)

# Compare before and after for the (sub-sampled) warm training dataset
table(boruta_warm_sampled$finalDecision)
table(boruta_warm_fixed$finalDecision)

# Now compare results between the two datasets
cold_results <- attStats(boruta_cold_fixed) %>%
  rownames_to_column("Variable") %>%
  select(Variable, Cold = decision)

warm_results <- attStats(boruta_warm_fixed) %>%
  rownames_to_column("Variable") %>%
  select(Variable, Warm = decision)

boruta_comparison <- full_join(
  cold_results,
  warm_results,
  by = "Variable"
)

# View(boruta_comparison)  # un-tag to view results table


# Sort out features by categories 
boruta_comparison <- boruta_comparison %>%
  mutate(
    Group = case_when(
      grepl("temp", Variable) ~ "Temperature",
      grepl("humidity", Variable) ~ "Humidity",
      grepl("wind", Variable) ~ "Wind",
      grepl("precipitation", Variable) ~ "Precipitation",
      grepl("photoperiod", Variable) ~ "Photoperiod",
      Variable %in% c("S1", "C1", "S2", "C2") ~ "Seasonality",
      Variable %in% c("elevation", "dist_coast_km") ~ "Geography",
      TRUE ~ "Other"
    )
  )

# Within the same categories as above, compare feature results
boruta_comparison <- boruta_comparison %>%
  mutate(
    Relevance = case_when(
      Cold == "Confirmed" & Warm == "Confirmed" ~ "Both",
      Cold == "Confirmed" & Warm == "Rejected" ~ "Cold only",
      Cold == "Rejected" & Warm == "Confirmed" ~ "Warm only",
      TRUE ~ "Neither"
    )
  )

# boruta_comparison %>% arrange(Group, Relevance) %>% View()  # un-tag to view results table

# Create plots to visualise the results ----------------------------------------
# Heatmap comparing the porportion of confirmed predictors for 
# every feature type between the cold and warm datasets
ggplot(
  group_summary,
  aes(x = Group, y = Percentage, fill = Season)
) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Cold" = "steelblue",
      "Warm" = "coral"
    )
  ) +
  labs(
    title = "Boruta Feature Selection by Environmental Factor",
    x = "Environmental Factor",
    y = "Confirmed Predictors (%)",
    fill = "Season:"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  )

# Create a function for visualisation
make_boruta_results <- function(boruta_obj, boruta_fixed_obj, season_name) {
  
  original <- attStats(boruta_obj) %>%
    rownames_to_column("Variable") %>%
    select(Variable, original_decision = decision, medianImp)
  
  fixed <- attStats(boruta_fixed_obj) %>%
    rownames_to_column("Variable") %>%
    select(Variable, fixed_decision = decision)
  
  original %>%
    left_join(fixed, by = "Variable") %>%
    mutate(
      Season = season_name,
      Final_status = case_when(
        original_decision == "Confirmed" ~ "Confirmed",
        original_decision == "Rejected" ~ "Rejected",
        original_decision == "Tentative" & fixed_decision == "Confirmed" ~ "Tentative → Confirmed",
        original_decision == "Tentative" & fixed_decision == "Rejected" ~ "Tentative → Rejected",
        TRUE ~ "Other"
      )
    ) 
}
# This compares the decisions before and after running TentativeRoughFix
# so that we are able to see which features Boruta was still uncertain about.

# Run the function on cold
cold_results <- make_boruta_results(
  boruta_cold, boruta_cold_fixed, "Cold"
)
# Run the function on warm
warm_results <- make_boruta_results(
  boruta_warm_sampled, boruta_warm_fixed, "Warm"
)
# Create a dataset including the results of both cold and warm.
boruta_results_all <- bind_rows(cold_results, warm_results)

# To the table, add the feature categories
boruta_results_all <- boruta_results_all %>%
  mutate(
    Group = case_when(
      grepl("temp", Variable) ~ "Temperature",
      grepl("humidity", Variable) ~ "Humidity",
      grepl("wind", Variable) ~ "Wind",
      grepl("precipitation", Variable) ~ "Precipitation",
      grepl("photoperiod", Variable) ~ "Photoperiod",
      Variable %in% c("S1", "C1", "S2", "C2") ~ "Seasonality",
      Variable %in% c("elevation", "dist_coast_km") ~ "Geography",
      TRUE ~ "Other"
    )
  )

# Order the table by importance
variable_order <- boruta_results_all %>%
  group_by(Variable, Group) %>%
  summarise(maxImp = max(medianImp, na.rm = TRUE), .groups = "drop") %>%
  arrange(Group, maxImp)

boruta_results_all <- boruta_results_all %>%
  mutate(
    Variable = factor(Variable, levels = variable_order$Variable)
  )

# Create a heatmap with separate plots for the separate categories. 
ggplot(
  boruta_results_all,
  aes(x = Season, y = fct_rev(Variable), fill = Final_status)
) +
  geom_tile(color = "white") +
  facet_wrap(~ Group, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Confirmed" = "forestgreen",
      "Rejected" = "firebrick",
      "Tentative → Confirmed" = "aquamarine",
      "Tentative → Rejected" = "#E066FF"
    )
  ) +
  labs(
    title = "Boruta Feature Selection Results by Variable Category",
    x = NULL,
    y = NULL,
    fill = "Decision"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )
# Hard to discern the humidity and temperature results

# Plot the humidity results heatmap ---------
humidity_boruta <- boruta_results_all %>%
  filter(Group == "Humidity") %>%
  ggplot(aes(x = Season, y = fct_rev(Variable), fill = Final_status)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Confirmed" = "forestgreen",
      "Rejected" = "firebrick",
      "Tentative → Confirmed" = "aquamarine",
      "Tentative → Rejected" = "#E066FF"
    )
  ) +
  labs(
    title = "Boruta Results: Humidity Variables",
    x = NULL,
    y = NULL,
    fill = "Decision"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# Create a pdf for this plot
pdf(
  "Boruta Results: Humidity Variables.pdf",
  width = 10,
  height = 6
)

print(humidity_boruta)

dev.off()

# Plot the temperature results heatmap ---------
temperature_boruta <- boruta_results_all %>%
  filter(Group == "Temperature") %>%
  ggplot(aes(x = Season, y = fct_rev(Variable), fill = Final_status)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Confirmed" = "forestgreen",
      "Rejected" = "firebrick",
      "Tentative → Confirmed" = "aquamarine",
      "Tentative → Rejected" = "#E066FF"
    )
  ) +
  labs(
    title = "Boruta Results: Temperature Variables",
    x = NULL,
    y = NULL,
    fill = "Decision"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# Create a pdf for this plot
pdf(
  "Boruta Results: Temperature Variables.pdf",
  width = 10,
  height = 6
)

print(temperature_boruta)

dev.off()