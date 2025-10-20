library(dplyr)
library(ggplot2)
library(lubridate)

cleaned_quote_df <- read.csv("Cleaned_Destinations_Dates_Freely_Data.csv")
crime_rate <- read.csv("crime-rate-by-country-2025.csv")

# Removal of Duplicates
cleaned_quote_df <- distinct(cleaned_quote_df)

# Make country destination consistent
crime_rate$country[crime_rate$country == "United States"] <- "USA"
crime_rate$country[crime_rate$country == "Sudan"] <- "South Sudan"

replacements <- c(
  "Netherlands \\(the Netherlands\\)" = "Netherlands",
  "Italy \\(Italia\\)" = "Italy",
  "Fraser Island" = "Australia",
  "Norfolk Island" = "Australia",
  "Alabama" = "USA",
  "Alberta" = "Canada",
  "Espana" = "Spain",
  "Estados Unidos" = "USA",
  "Isle of Pines" = "New Caledonia",
  "Loyalty Islands" = "New Caledonia",
  "Malvinas" = "Falkland Islands",
  "Massachusetts" = "USA",
  "Michigan" = "USA",
  "Netherlands Antilles" = "Netherlands",
  "New Mexico" = "USA",
  "Nova Scotia" = "Canada",
  "Nuremburg" = "Germany",
  "Port Villa" = "Vanuatu",
  "Quebec" = "Canada",
  "Salvador" = "Brazil",
  "Strasbourg" = "France",
  "Tennessee" = "USA",
  "El Brazil" = "Brazil"
)

# Trim white space 
cleaned_quote_df$destinations <- trimws(cleaned_quote_df$destinations)

# Loop through each replacement and apply gsub
for(old in names(replacements)) {
  new <- replacements[old]
  cleaned_quote_df$destinations <- gsub(old, new, cleaned_quote_df$destinations)
}

cleaned_quote_df <- cleaned_quote_df %>%
  mutate(destinations = str_replace(
    destinations,
    regex("\\btrinidad\\s*,\\s*tobago\\b", ignore_case = TRUE),
    "Trinidad and Tobago"
  ))

## Change traveller age into categorical data
cleaned_quote_df$traveller_categories <- strsplit(cleaned_quote_df$traveller_ages, ";")
cleaned_quote_df$num_people <- sapply(cleaned_quote_df$traveller_categories, length)

# Determine the youngest age in the group
cleaned_quote_df$youngest_age <- sapply(cleaned_quote_df$traveller_categories, function(ages) {
  ages <- as.numeric(ages)
  min(ages, na.rm = TRUE)
})

# Determine the oldest age in the group
cleaned_quote_df$oldest_age <- sapply(cleaned_quote_df$traveller_categories, function(ages) {
  ages <- as.numeric(ages)
  max(ages, na.rm = TRUE)
})

# Carer Load Ratio
cleaned_quote_df$carer_load_ratio <- sapply(cleaned_quote_df$traveller_categories, function(ages){
  ages <- as.numeric(ages)
  n_adults <- sum(ages >= 18) # Counts how many travellers are 18 or older
  n_children <- sum(ages < 18)
  if (n_adults == 0) return(NA)
  n_children/n_adults
})

# Categorise traveller groups 
cleaned_quote_df$traveller_categories <- sapply(cleaned_quote_df$traveller_categories, function(ages) {
  ages <- as.numeric(ages)
  n_adults <- sum(ages >= 18)
  n_children <- sum(ages < 18)
  youngest_age <- min(ages, na.rm = TRUE)
  oldest_age <- max(ages, na.rm = TRUE)
  
  if (n_adults == 1 && n_children == 0) {
    "Solo"
  } else if (n_adults == 2 && n_children == 0 && oldest_age < youngest_age + 20) {
    "Couple"
  } else if (n_children > 0 && n_adults >= 1 ) {
    "Family"
  } else if (n_adults >= 3 && n_children == 0) {
    "Group"
  } else {
    "Other"           # Suspect "other" could be a mixture of older couples with 20 year age gap 
                      # and families such as 18 year old and a guardian.
  }
})

## Implement crime index data
crime_rate <- crime_rate %>%
  add_row(country = "Pacific Islands", NumbeoCrimeIndex_2024 = mean(crime_rate$NumbeoCrimeIndex_2024[144:158]))

crime_lookup <- setNames(crime_rate$NumbeoCrimeIndex_2024, crime_rate$country)

cleaned_quote_df$avg_crime_rate <- sapply(cleaned_quote_df$destinations, function(row_countries) {
  countries <- unlist(strsplit(row_countries, ",\\s*"))  # split string into vector
  mean(crime_lookup[countries], na.rm = TRUE)            # look up values and take mean
})

# Summarise the crime rate data and compare with average conversion rate per country.

crime_summary <- cleaned_quote_df %>%
  # split multiple countries in the same cell into separate rows
  separate_rows(destinations, sep = ";|,") %>%
  mutate(destinations = trimws(destinations)) %>%
  group_by(destinations) %>%
  summarise(
    total_quotes = n(),
    total_converted = sum(convert == "YES"),
    conversion_rate_by_country = total_converted / total_quotes
  ) %>%
  ungroup()

merged_data <- crime_summary %>%
  left_join(crime_rate, by = c("destinations" = "country"))

# Graph density of conversions by crime index
ggplot(cleaned_quote_df, aes(x = avg_crime_rate, fill = convert)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribution of Destination Crime Index - Conversion",
    x = "Crime Index",
    y = "Density"
  ) +
  theme_minimal()

# Average Quote Price by Crime Rate
custom_breaks <- c(0, seq(5, 95, by = 2), 100)

binned_df <- cleaned_quote_df %>%
  mutate(
    # Create bins from 0 to 100 using your custom breaks
    crime_bin = cut(avg_crime_rate, breaks = custom_breaks, include.lowest = TRUE)
  ) %>%
  group_by(crime_bin) %>%
  summarise(
    total = n(),
    average_quote_price = mean(quote_price, na.rm = TRUE)
  )

ggplot(binned_df, aes(x = crime_bin, y = average_quote_price)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Crime Rate Bin",
    y = "Average Quote Price",
    title = "Average Quote Price by Crime Rate Bin"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Proportion of Conversion Graph - number of people travelling
ggplot(cleaned_quote_df, aes(x = num_people, fill = convert)) +
  geom_bar(position = "fill") +  # use "fill" for stacked proportion, or "stack" for raw counts
  labs(
    title = "Conversion Rate by Traveller Group",
    x = "Traveller Group",
    y = "Proportion of Quotes",
    fill = "Conversion Status"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# Proportion of Conversion Grapbh - classification of traveller groups
ggplot(cleaned_quote_df, aes(x = traveller_categories, fill = convert)) +
  geom_bar(position = "fill") +  # use "fill" for stacked proportion, or "stack" for raw counts
  labs(
    title = "Conversion Rate by Traveller Group",
    x = "Traveller Group",
    y = "Proportion of Quotes",
    fill = "Conversion Status"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
