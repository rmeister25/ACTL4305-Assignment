library(dplyr)
library(lubridate)
library(fastDummies)
library(caret)
library(glmnet)
library(smotefamily)
library(themis)

setwd("Desktop/ACTL4305-Assignment")
freely_data <- read.csv("Cleaned_Destinations_Dates_Freely_Data.csv")

freely_data <- freely_data %>%
  filter(!is.na(trip_start_date_clean),    # remove rows that have no travel dates
         !is.na(trip_end_date_clean)) %>%
  dummy_cols(
    select_columns = c("discount", "platform", "traveller_categories"),
    remove_selected_columns = TRUE
  )

#Separating Boosts
boost_cols <- paste0("boost_", 1:8, "_name")  # boost_1_name ... boost_8_name
boosts <- unique(unlist(freely_data[boost_cols]))
boosts <- boosts[!is.na(boosts)]  # remove NAs

# Initialize columns
for (b in boosts) {
  freely_data[[b]] <- 0
}

# Ensure all boost_name columns are character
for (j in 1:8) {
  col_name <- paste0("boost_", j, "_name")
  freely_data[[col_name]] <- as.character(freely_data[[col_name]])
}

# Loop over each boost column
for (j in 1:8) {
  col_name <- paste0("boost_", j, "_name")
  
  for (i in 1:nrow(freely_data)) {
    boost <- freely_data[[col_name]][i]
    if (!is.na(boost) && boost %in% names(freely_data)) {
      freely_data[i, boost] <- 1
    }
  }
}

quote_data <- freely_data %>%
  mutate(
    trip_start_date_clean = dmy(trip_start_date_clean),
    trip_end_date_clean = dmy(trip_end_date_clean),
    number_of_days_travelled = as.numeric(trip_end_date_clean - trip_start_date_clean),
    quote_delay = Quote.and.Start.Delay,
    solo = traveller_categories_Solo,
    couple = traveller_categories_Couple,
    family = traveller_categories_Family,
    group = traveller_categories_Group,
    others = traveller_categories_Other,
    extra_cancellation = if_else(is.na(extra_cancellation), 0, extra_cancellation),
    extra_cancellation_amount = extra_cancellation,
    convert = case_when(
      convert == "NO"  ~ 0,
      convert == "YES" ~ 1
    )
  ) %>%
  select("quote_price",
         "solo",
         "couple",
         "family",
         "group",
         "others",
         "number_of_days_travelled",
         "quote_delay",
         "platform_app",
         "platform_qw",
         "platform_web",
         "discount_0%",
         "discount_5%",
         "discount_10%",
         "discount_15%",
         "discount_20%",
         "Specified Items",
         "Adventure Activities",
         "Snow Sports",
         "Extra Cancellation",
         "Rental Vehicle Insurance Excess",
         "Cruise Cover",
         "Gadget Cover",
         "Existing Medical Condition(s)",
         "Motorcycle Cover",
         "extra_cancellation_amount",
         "carer_load_ratio",
         "convert")

# Partitioning Data to 80% for training and 20% for testing
set.seed(185)
quote_data <- quote_data %>%
  mutate(across(everything(), as.numeric))
inTrain <- createDataPartition(
  y = quote_data$convert,
  p = .8,
  list = FALSE
)

train <- quote_data[inTrain, ]
test <- quote_data[-inTrain, ]

# Stores response as a factor
train_r <- as.factor(train$convert)
test_r <- as.factor(test$convert)

# Scaling road data
mean_train <- colMeans(train[,-28])
sd_train <- apply(train[,-28], 2, sd)

train_scaled <- cbind(scale(train[,1:27]), convert = train[,28])
test_scaled <- as.matrix(
  cbind(scale(test[,1:27], center = mean_train, scale = sd_train),
        convert = test[,28]))


# Balancing Imbalanced Dataset
# SMOTE to oversample minority class
# x <- as.data.frame(train_scaled[,-28])
# y <- as.factor(train_scaled[,28])

x <- as.data.frame(train[,-28])
y <- as.factor(train[,28])
genData = SMOTE(x,y, K = 5, dup_size = 5)
genData <- genData$data
table(genData$class)

write.csv(genData, "training_SMOTE_dataset.csv")
write.csv(test, "test_dataset.csv")
