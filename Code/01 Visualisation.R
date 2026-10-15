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

mosquito <- read.xlsx("Vectabundace_v015.xlsx")
#I cleaned the dataset by removing the NA values from the value column as the article discussing the dataset says "If an ovitrap is found dry or overturned, the masonite stick is not considered, and therefore the value assigned to that ovitrap is NA"
#Then, I removed the columns that describe the species it is investigating and therefore were the same for every observation.
mosquito <- mosquito[complete.cases(mosquito[, c("value")]),]
mosquito[ , c("trap_type", "larvicide_presence", "Canonical_name", "kingdom", "phylum", "class", "order", "family", "genus", "species", "life_stage")] <- list(NULL)

mosquito$date <- as.Date(mosquito$date, origin = "1899-12-30")

abundance_monthly <- mosquito %>%
  mutate(month = month(date)) %>%
  group_by(Country, year, month) %>%
  summarise(
    abundance = mean(value),
    .groups = "drop"
  )

ggplot(
  abundance_monthly,
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
#The output plot shows that there is an unusual peak in egg abundance in August 2014 in Albania, so let's investigate this section of the dataset closer

albania_aug2014 <- mosquito %>%
  filter(
    Country == "Albania",
    year == 2014,
    month(date) == 8
  )
summary(albania_aug2014$value)
nrow(albania_aug2014)
sort(albania_aug2014$value, decreasing = TRUE)

#The outputs indicate an unusually large difference between the beginning of that month and the end

ggplot(
  mosquito %>% filter(Country == "Albania"),
  aes(x = factor(month(date)), y = value)
) +
  geom_boxplot() +
  labs(
    title = "Distribution of mosquito abundance in Albania",
    x = "Month",
    y = "Egg abundance"
  )

abundance_monthly_median <- mosquito %>%
  mutate(month = month(date)) %>%
  group_by(Country, year, month) %>%
  summarise(
    abundance = median(value),
    .groups = "drop"
  )

ggplot(
  abundance_monthly_median,
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

#The plots aren't much different to the ones that use the mean egg values
