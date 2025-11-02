install.packages("rpart")
install.packages("randomForest")
install.packages("rpart.plot")
install.packages("rattle")
library(rattle)
library(rpart)
library(randomForest)
library(dplyr)
library(tidyr)
library(rpart.plot)
sourceData <- read.csv("Cleaned_Destinations_Dates_Freely_Data.csv")

regionSplit <- strsplit(sourceData$regions, ", ")

regions <- unique(unlist(regionSplit))

df <- data.frame(matrix(NA, nrow = 67021, ncol = 0))

for (region in regions) {
  df[region] <- sapply(regionSplit, function(v) region %in% v)
}

boostSplit <- sourceData[grepl("^boost_.*_name$", names(sourceData))] %>%
  unite(boostNames, boost_1_name, boost_2_name, boost_3_name, boost_4_name,
        boost_5_name, boost_6_name, boost_7_name, boost_8_name, na.rm = TRUE,
        sep = ", ") %>%
  .$boostNames %>%
  strsplit(split = ", ")

boosts <- unique(unlist(boostSplit))

for (boost in boosts) {
  df[boost] <- sapply(boostSplit, function(v) boost %in% v)
}

categoricalDF <- cbind(df, sourceData[, c("platform", 
                                          "discount", 
                                          "traveller_categories", 
                                          "convert")
                                      ])

categoricalDF <- data.frame(lapply(categoricalDF, as.factor))

numericalDF <- sourceData[, c("quote_price", 
                              "country_count",
                              "log_ppp",
                              "Human.rights.index",
                              "Rule.of.Law.Index",
                              "water_death_rate",
                              "Quote.and.Start.Delay",
                              "num_people",
                              "carer_load_ratio",
                              "avg_crime_rate")]

numericalDF <- data.frame(lapply(numericalDF, as.numeric))
df <- cbind(categoricalDF, numericalDF)

convertTree <- rpart(convert ~ ., data = df, method = "class",
                     control = rpart.control(cp = 0.001))

set.seed(42)

rf_model <- randomForest(
  as.factor(convert) ~ .,
  data = df, 
  ntree = 300,
  mtry = ncol(df) - 1
)