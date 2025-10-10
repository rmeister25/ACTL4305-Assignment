cleaned_quote_df <- read.csv("New_Freely_destinations_fully_cleaned.csv")
crime_rate <- read.csv("crime-rate-by-country-2025.csv")

crime_rate$country[crime_rate$country == "United States"] <- "USA"
crime_rate$country[crime_rate$country == "Sudan"] <- "South Sudan"
cleaned_quote_df$Countries.Only[cleaned_quote_df$Countries.Only == "Netherlands (the Netherlands)"] <- "Netherlands"
cleaned_quote_df$Countries.Only[cleaned_quote_df$Countries.Only == "Italy (Italia)"] <- "Italy"
cleaned_quote_df$Countries.Only[cleaned_quote_df$Countries.Only == "Fraser Island"] <- "Australia"
cleaned_quote_df$Countries.Only[cleaned_quote_df$Countries.Only == "Norfolk Island"] <- "Australia"

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
    # and families such as 18 year old and his mum.
  }
})

crime_rate <- crime_rate %>%
  add_row(country = "Pacific Islands", NumbeoCrimeIndex_2024 = mean(crime_rate$NumbeoCrimeIndex_2024[144:158]))

crime_lookup <- setNames(crime_rate$NumbeoCrimeIndex_2024, crime_rate$country)

cleaned_quote_df$avg_crime_rate <- sapply(cleaned_quote_df$Countries.Only, function(row_countries) {
  countries <- unlist(strsplit(row_countries, ",\\s*"))  # split string into vector
  mean(crime_lookup[countries], na.rm = TRUE)            # look up values and take mean
})

x <- cleaned_quote_df %>% filter(is.na(avg_crime_rate))
sort(table(x$Countries.Only),desc = T)



# Proportion of Conversion - number of people travelling
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

# Proportion of Conversion - classification of traveller groups
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

# Proportion of Conversion - carer load
ggplot(cleaned_quote_df, aes(x = carer_load_ratio, fill = convert)) +
  geom_bar(position = "fill") +  # use "fill" for stacked proportion, or "stack" for raw counts
  labs(
    title = "Conversion Rate by Traveller Group",
    x = "Traveller Group",
    y = "Proportion of Quotes",
    fill = "Conversion Status"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# converted vs non-converted
ggplot(Quote_10000.df, aes(x = quote_price, fill = convert)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribution of Quote Prices by Conversion",
    x = "Quote Price",
    y = "Density"
  ) +
  theme_minimal()

