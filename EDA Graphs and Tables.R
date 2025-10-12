library(dplyr)
library(maps)
library(ggplot2)
curr_cleaned_data <- read.csv("Cleaned_Destinations_Dates_Freely_Data.csv")
yeses <- curr_cleaned_data %>%
  filter(convert == "YES")

countcountries <- function(data) {
  countries <- na.omit(unlist(strsplit(data, ", ")))
  return(tibble(country = countries) %>%
    count(country, sort = TRUE))
}

allCountries <- countcountries(curr_cleaned_data$Countries.Only)
yesCountries <- countcountries(yeses$Countries.Only)

world_map <- map_data("world")

ggplot(allCountries) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f", linewidth = 0.25
  ) +
  geom_map(map = world_map, aes(map_id = country, fill = n), linewidth = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Total Quotes") +
  expand_limits(x = world_map$long, y = world_map$lat)

ggplot(yesCountries) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f", linewidth = 0.25
  ) +
  geom_map(map = world_map, aes(map_id = country, fill = n), linewidth = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Total Converted Quotes") +
  expand_limits(x = world_map$long, y = world_map$lat)

percentageConversion <- yesCountries %>%
  mutate(n = yesCountries$n / allCountries[match(yesCountries$country, allCountries$country), ]$n)

ggplot(percentageConversion) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f", linewidth = 0.25
  ) +
  geom_map(map = world_map, aes(map_id = country, fill = n), linewidth = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Percent Conversions") +
  expand_limits(x = world_map$long, y = world_map$lat)

