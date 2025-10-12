install.packages("rpart")
install.packages("randomForest")
install.packages("rpart.plot")
library(rpart)
library(randomForest)
library(dplyr)
library(tidyr)
library(rpart.plot)
sourceData <- read.csv("Cleaned_Destinations_Dates_Freely_Data.csv")

regionSplit <- strsplit(sourceData$regions, ", ")

regions <- unique(unlist(regionSplit))

df <- data.frame(matrix(NA, nrow = 70000, ncol = 0))

for (region in regions) {
  df[region] <- sapply(regionSplit, function(v) region %in% v)
}

df$platform <- sourceData$platform
df$discount <- sourceData$discount

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

df$convert <- sourceData$convert

convertTree <- rpart(as.factor(convert) ~ ., data = df, method = "class",
                     control = rpart.control(cp = 0))

set.seed(42)

names(df) <- make.names(names(df))

rf_model <- randomForest(
  as.factor(convert) ~ .,
  data = df, 
  ntree = 300,
  mtry = ncol(df) - 1
)
