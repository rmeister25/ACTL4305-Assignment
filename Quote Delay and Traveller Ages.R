library(dplyr)
library(maps)
library(ggplot2)

timeDiff <- function(later, earlier) {
  currFormat <- "%d/%m/%Y"
  return(difftime(as.Date(later, format = currFormat), 
                  as.Date(earlier, format = currFormat),
                  units = "days"))
}

cleaned_data <- read.csv("Cleaned_Destinations_Dates_Freely_Data.csv") %>%
  mutate(quote_and_start_delay = as.numeric(timeDiff(trip_start_date_clean,
                                          quote_create_time_clean)))

removeOutliers <- function(df) {
  q3 <- quantile(df$quote_and_start_delay, 0.75, na.rm = TRUE)
  bound <- 2.5 * q3
  cleaned_df <- df[df$quote_and_start_delay <= bound, ]
  return(cleaned_df)
}

df <- removeOutliers(cleaned_data[, c("convert", "quote_and_start_delay")])

ggplot(df, aes(x = quote_and_start_delay, fill = convert)) +
  geom_density(alpha = 0.25)

write.csv(cleaned_data$quote_and_start_delay, "delay_data.csv", row.names = FALSE)
