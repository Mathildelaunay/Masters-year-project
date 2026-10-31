if(!require("ggplot2"))install.packages("ggplot2")
if(!require("maps"))install.packages("maps")
if(!require("plotly"))install.packages("plotly")
if(!require("tidyverse"))install.packages("tidyverse")
if(!require("readxl"))install.packages("readxl")
if(!require("openxlsx"))install.packages("openxlsx")
if(!require("lubridate"))install.packages("lubridate")

library(ggplot2)
library(maps)
library(plotly)
library(readxl)
library(lubridate)
library(dplyr)

# After uploading the dataset from the xlsx file into R:
mosquito <- Vectabundace_v015 

# Cleaning the dataset:
mosquito <- mosquito[complete.cases(mosquito[, c("value")]),]
mosquito[ , c("trap_type", "larvicide_presence", "Canonical_name", "kingdom", "phylum", "class", "order", "family", "genus", "species", "life_stage", "EPSG", "DOI_website", "contact_person_email", "contact_person", "Institute")] <- list(NULL)

# Make sure that the R reads the dates as dates:
mosquito$date <- as.Date(mosquito$date, origin = "1899-12-30") 

# Getting the monthly mean values of mosquito abundance for every country:
monthly_mean_abundance <- mosquito %>%
  mutate(month = month(date)) %>%
  group_by(Country, year, month) %>%
  summarise(
    abundance = mean(value),
    .groups = "drop"
  )

# Next, plot the monthly mean mosquito abundance for every country:
p1 <- ggplot(
  monthly_mean_abundance,
  aes(
    x = month,
    y = abundance,
    group = year,
    colour = factor(year)
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Mosquito abundance throughout the year",
    x = "Month",
    y = "Mean egg abundance",
    colour = "Year"
  )

# The output plot shows that there is an unusual peak in egg abundance in August 2014 in Albania 
# Let's instead plot the monthly median to deal with outliers:

# Look at the distribution of the median abundance values instead of the mean:
monthly_median_abundance <- mosquito %>%
  mutate(month = month(date)) %>%
  group_by(Country, year, month) %>%
  summarise(
    abundance = median(value),
    .groups = "drop"
  )

p2 <- ggplot(
  monthly_median_abundance,
  aes(
    x = month,
    y = abundance,
    group = year,
    colour = factor(year)
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Mosquito abundance throughout the year",
    x = "Month",
    y = "Median egg abundance",
    colour = "Year"
  )

# Look at which months contain observations for every country and every year:
monthly_coverage <- mosquito_climate %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  group_by(Country, year) %>%
  summarise(
    months_observed = paste(
      month.abb[sort(unique(month))],
      collapse = ", "
    ),
    months_not_observed = paste(
      month.abb[setdiff(1:12, unique(month))],
      collapse = ", "
    ),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  arrange(Country, year)

