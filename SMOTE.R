library(dplyr)
library(lubridate)
library(fastDummies)
library(caret)
library(glmnet)
library(smotefamily)
library(themis)

setwd("Desktop/ACTL4305-Assignment")
options(scipen = 999)
freely_data <- read.csv("Cleaned_Destinations_Dates_Freely_Data.csv")
freely_data$X <- NULL

# Removing NA values ~ 8000 data points lost
freely_data <- freely_data %>%
  mutate(across(
    everything(),
    ~ {
      x <- trimws(as.character(.x))                  # remove whitespace & convert factors to character
      ifelse(x %in% c("#N/A", "#DIV/0!", "NA", ""),  # list all “fake” NA values
             NA, 
             x)
    }
  )) %>%
  filter(!is.na(trip_start_date_clean),
         !is.na(trip_end_date_clean),
         !is.na(destinations),
         !is.na(log_ppp),
         !is.na(Human.rights.index),
         !is.na(water_death_rate))    

# Separating Regions
regions <- unique(unlist(strsplit(freely_data$regions, ",\\s*")))
regions

for (reg in regions) {
  freely_data[[reg]] <- as.integer(grepl(reg, freely_data$regions))
}

# Separating Boosts
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

# ONE HOT ENCODING
freely_data <- freely_data %>%
  dummy_cols(
    select_columns = c("discount", "platform", "traveller_categories"),
    remove_selected_columns = TRUE
  )

# Further Data Clean + Selection of variables for model building
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
    human_rights_index = Human.rights.index,
    rule_of_law_index = Rule.of.Law.Index,
    extra_cancellation = as.numeric(extra_cancellation),
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
         "Asia",
         "Europe",
         "Oceania",
         "Middle East",
         "North America",
         "Africa",
         "South America",
         "Central America",
         "Caribbean",
         "Worldwide",
         #"Antarctica",
         "log_ppp",
         "average_ppp",
         "human_rights_index",
         "rule_of_law_index",
         "water_death_rate",
         "avg_crime_rate",
         "num_people",
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

# Store column numbers which contain binary variables
binary_cols <- which(sapply(quote_data, function(x) all(unique(x) %in% c(0, 1))))
non_binary_cols <- setdiff(1:ncol(train), binary_cols)

## Scaling
# Calculate mean and sd of each column
mean_train <- colMeans(train[, non_binary_cols])
sd_train <- apply(train[, non_binary_cols], 2, sd)

# Create the train and test scaled dataframes
train_scaled <- cbind(
  scale(train[, non_binary_cols]),
  train[, binary_cols]
)

test_scaled <- as.matrix(
  cbind(scale(test[, non_binary_cols], center = mean_train, scale = sd_train),
        test[, binary_cols]))

## Balancing Imbalanced Dataset
# SMOTE to oversample minority class
x_scaled <- as.data.frame(train_scaled[, !colnames(train_scaled) == "convert"])
y <- as.factor(train_scaled$convert) # I've renamed this variable. dont need to scale this. 
genData = SMOTE(x_scaled,y, K = 5, dup_size = 2)
genData <- genData$data
genData$class <- as.factor(as.character(genData$class))

train_scaled$convert <- as.factor(train_scaled$convert)
genData <- train_scaled

# Undersampling using Tomek Links to eliminate opposite class nearest neighbours
# genData <- recipe(~., train_scaled) %>%
#   step_tomek(convert) %>%
#   prep() %>%
#   bake(new_data = NULL)

# Near Miss -1 
# train_nearmiss_sample <- recipe(~., genData) %>%
#   step_nearmiss(convert, under_ratio = 3) %>%
#   prep() %>%
#   bake(new_data = NULL)
# table(train_nearmiss_sample$convert)
# genData <- train_nearmiss_sample

# Renamed class to convert.
colnames(genData)[colnames(genData) == "class"] <- "convert"
table(genData$convert)

## BOOST ANALYSIS
boost_df <- freely_data %>% 
  select(
    "Specified Items",
    "Adventure Activities",
    "Snow Sports",
    "Gadget Cover",
    "Motorcycle Cover",
    "Extra Cancellation",
    "Rental Vehicle Insurance Excess",
    "Cruise Cover",
    "Existing Medical Condition(s)",
    "convert"
  ) %>%
  mutate(
    convert = case_when(
      convert == "NO"  ~ 0,
      convert == "YES" ~ 1
    )
  )

boost_regression <- glm(convert ~ ., data = boost_df, family = binomial)
summary_obj <- summary(boost_regression)
coef_df <- as.data.frame(summary_obj$coefficients)
coef_df <- round(coef_df, 3)
coef_df$Variable <- rownames(coef_df)

writexl::write_xlsx(coef_df, "boost_regression_summary.xlsx")

boost_summary <- freely_data %>%
  mutate(convert_num = ifelse(convert == "YES", 1, 0)) %>%
  select(ends_with("_name"), convert_num, quote_price) %>%
  pivot_longer(cols = starts_with("boost"), 
               names_to = "boost_col", 
               values_to = "boost_name") %>%
  filter(!is.na(boost_name) & boost_name != "") %>%
  mutate(boost_name = str_trim(boost_name)) %>%
  separate_rows(boost_name, sep = ";|,") %>%
  group_by(boost_name) %>%
  summarise(
    total_occurrences = n(),              # how many times this boost appears
    total_converted = sum(convert_num),   # total YES conversions
    average_quote_price = mean(quote_price),
    conversion = total_converted / total_occurrences,
    expected_revenue = average_quote_price * conversion
  ) %>%
  arrange(desc(total_converted))

print(boost_summary)

# SMOTED dataset ~ use for training the model
write.csv(genData, "training_SMOTE_dataset.csv")

# The names of the sheets should be the other way round. 
write.csv(test_scaled, "test_scaled_dataset.csv")
write.csv(test, "test_original_dataset.csv")

# Original training dataset
write.csv(train, "training_dataset_80%.csv")

# Original training dataset but scaled
write.csv(train_scaled, "train_scaled_dataset.csv")

train_stats <- data.frame(
  cbind(mean = mean_train, sd = sd_train)
)

write.csv(train_stats, "mean_sd_training_data.csv")

# ## NON-SCALED training data
# x <- as.data.frame(train[,-28])
# y <- as.factor(train[,28])
# genData = SMOTE(x,y, K = 5, dup_size = 5)
# genData <- genData$data
# table(genData$class)
# 
# write.csv(genData, "training_scaled_SMOTE_dataset.csv")
# write.csv(test_scaled, "test_scaled_dataset.csv")
