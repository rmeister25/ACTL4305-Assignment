install.packages("dplyr")
install.packages("stringr")
install.packages("maps")
install.packages("missForest")
install.packages("pdp")
install.packages("patchwork")
library(dplyr)
library(stringr)
library(maps)
library(tidyr)
library(missForest)
library(randomForest)
library(ranger)
library(pdp)
library(ggplot2)
library(patchwork)


dataset <- read.csv("Cleaned_Destinations_Dates_Freely_Data.csv")

countDestinations <- function(data) {
  countries <- na.omit(unlist(strsplit(data, ", ")))
  return(tibble(country = countries) %>%
           count(country, sort = TRUE))
}

destinationList <- countDestinations(dataset$destinations)

data("world.cities")

country_latlong <- world.cities %>%
  group_by(country.etc) %>%
  summarise(
    avg_lat = mean(lat, na.rm = TRUE),
    avg_long = mean(long, na.rm = TRUE)
  )

country_latlong <- data.frame(country_latlong)
rownames(country_latlong) <- country_latlong$country.etc
country_latlong$country.etc <- NULL
rownames(country_latlong)[rownames(country_latlong) == "UK"] <- "United Kingdom"
rownames(country_latlong)[rownames(country_latlong) == "Korea South"] <- "South Korea"
rownames(country_latlong)[rownames(country_latlong) == "East Timor"] <- "Timor-Leste"
rownames(country_latlong)[rownames(country_latlong) == "Saint Kitts and Nevis"] <- "St. Kitts-Nevis"
rownames(country_latlong)[rownames(country_latlong) == "US Virgin Islands"] <- "Virgin Islands"
rownames(country_latlong)[rownames(country_latlong) == "Saint Lucia"] <- "St. Lucia"
rownames(country_latlong)[rownames(country_latlong) == "Swaziland"] <- "Eswatini"
country_latlong <- rbind(country_latlong, 
                         "Hong Kong" = c(22.3193, 114.1694),
                         "Antarctica" = c(-75, 0),
                         "Macau" = c(22.1987, 113.5439))

destinationSplit <- strsplit(dataset$destinations, ", ")

north <- c("Asia", "Europe", "Middle East", "North America", "Central America",
           "Caribbean", "Worldwide", "Africa", "South America")
south <- c("South America", "Oceania", "Antarctica", "Worldwide", "Africa")
climateZones <- matrix(c(T, T, T, T, T,
                         F, T, T, T, T,
                         F, T, T, T, T,
                         T, T, T, T, F,
                         T, F, F, F, F,
                         T, T, T, T, F,
                         T, T, T, T, F,
                         T, T, T, F, F,
                         F, F, F, F, T,
                         T, T, T, T, T), nrow = 10, byrow = TRUE,
                       dimnames = list(
                         c("Asia", "Europe", "North America",
                           "South America", "Central America",
                           "Africa", "Middle East", "Oceania",
                           "Antarctica", "Worldwide"),
                         c("Tropical", "Subtropical", "Temperate", "Continental",
                           "Polar")))

makePattern <- function(r) {
  return(paste0("\\b(", paste(r, collapse = "|"), ")\\b(,|$)"))
}

getRegions <- function(zones, colname) {
  return(rownames(zones)[zones[, colname]  == T])
}

patternNorth <- makePattern(north)
patternSouth <- makePattern(south)
climatePatterns <- data.frame(Tropical = c(0),
                              Subtropical = c(0),
                              Temperate = c(0),
                              Continental = c(0),
                              Polar = c(0))
for (column in colnames(climateZones)) {
  climatePatterns[column] <- makePattern(getRegions(climateZones, column))
}

northHemisphere <- rep(FALSE, 67021)
southHemisphere <- rep(FALSE, 67021)
Temperate <- rep(FALSE, 67021)
Tropical <- rep(FALSE, 67021)
Continental <- rep(FALSE, 67021)
Subtropical <- rep(FALSE, 67021)
Polar <- rep(FALSE, 67021)

for (i in 1:length(destinationSplit)) {
  if (is.na(destinationSplit[i])) {
    currRegions <- dataset$regions[i]
    northHemisphere[i] <- grepl(patternNorth, currRegions)
    southHemisphere[i] <- grepl(patternSouth, currRegions)
    for (climate in names(climatePatterns)) {
      tmp <- get(climate)
      tmp[i] <- grepl(climatePatterns[1, climate], currRegions)
      assign(climate, tmp)
    }
    #Tropical[i] <- grepl(climatePatterns$Tropical[1], currRegions)
    #Subtropical[i] <- grepl(climatePatterns$Subtropical[1], currRegions)
    #Temperate[i] <- grepl(climatePatterns$Temperate[1], currRegions)
    #Continental[i] <- grepl(climatePatterns$Continental[1], currRegions)
    #Polar[i] <- grepl(climatePatterns$Polar[i], currRegions)
  }
  else {
    for (dest in destinationSplit[[i]]) {
      lat <- country_latlong[dest, "avg_lat"]
      abslat <- abs(lat)
      northHemisphere[i] <- ifelse(northHemisphere[i], T, lat >= 0)
      southHemisphere[i] <- ifelse(southHemisphere[i], T, lat < 0)
      Tropical[i] <- ifelse(Tropical[i], T, abslat <= 23.5)
      Subtropical[i] <- ifelse(Tropical[i], T, abslat > 23.5 & abslat <= 35)
      Temperate[i] <- ifelse(Tropical[i], T, abslat > 35 & abslat <= 55)
      Continental[i] <- ifelse(Tropical[i], T, abslat > 55 & abslat <= 66.5)
      Polar[i] <- ifelse(Tropical[i], T, abslat > 66.5)
    }
  }
}

df <- data.frame(
  platform = dataset$platform,
  north = northHemisphere,
  south = southHemisphere,
  tropical = Tropical,
  subtropical = Subtropical,
  temperate = Temperate,
  continental = Continental,
  polar = Polar,
  log_ppp = dataset$log_ppp,
  Human.rights.index = dataset$Human.rights.index,
  Rule.of.Law.Index = dataset$Rule.of.Law.Index,
  water_death_rate = dataset$water_death_rate,
  traveller_type = dataset$Traveller_type,
  oldest_age = dataset$oldest_age,
  carer_load_ratio = dataset$carer_load_ratio,
  avg_crime_rate = dataset$avg_crime_rate,
  trip_start = dataset$trip_start_date_clean,
  trip_end = dataset$trip_end_date_clean
)


regionSplit <- strsplit(dataset$regions, ", ")

regions <- unique(unlist(regionSplit))
for (region in regions) {
  df[region] <- sapply(regionSplit, function(v) region %in% v)
}

for (col in names(df)) {
  if (is.character(df[, col])) {
    df[[col]][df[, col] == "#N/A" | df[, col] == "#DIV/0!"] <- NA
  }
}

numeric_if_possible <- function(x) {
  converted_x <- as.numeric(x)
  if(!is.logical(x) && all(is.na(converted_x) == is.na(x))) {
    return(converted_x)
  }
  else {
    return(x)
  }
}

df_correct_class <- as.data.frame(lapply(df, numeric_if_possible))

boostSplit <- dataset[grepl("^boost_.*_name$", names(dataset))] %>%
  unite(boostNames, boost_1_name, boost_2_name, boost_3_name, boost_4_name,
        boost_5_name, boost_6_name, boost_7_name, boost_8_name, na.rm = TRUE,
        sep = ", ") %>%
  .$boostNames %>%
  strsplit(split = ", ")

boosts <- unique(unlist(boostSplit))

for (boost in boosts) {
  df[boost] <- sapply(boostSplit, function(v) boost %in% v)
}

Summer <- rep(F, 67021)
Autumn <- rep(F, 67021)
Winter <- rep(F, 67021)
Spring <- rep(F, 67021)

get_covered_seasons_north <- function(start_date, end_date) {
  currFormat <- "%d/%m/%Y"
  # Ensure dates
  start_date <- as.Date(start_date, format = currFormat)
  end_date <- as.Date(end_date, currFormat)
  
  # Generate months in the range
  months_seq <- seq(from = as.Date(format(start_date, "%Y-%m-01")),
                    to = as.Date(format(end_date, "%Y-%m-01")),
                    by = "month")
  
  months_num <- as.numeric(format(months_seq, "%m"))
  
  # Map to seasons
  get_season_north <- function(month) {
    season <- character(length(month))
    season[month %in% c(12, 1, 2)] <- "Winter"
    season[month %in% c(3, 4, 5)]  <- "Spring"
    season[month %in% c(6, 7, 8)]  <- "Summer"
    season[month %in% c(9, 10, 11)] <- "Autumn"
    return(unique(season))
  }
  
  return(get_season_north(months_num))
}

get_covered_seasons_south <- function(start_date, end_date) {
  currFormat <- "%d/%m/%Y"
  # Ensure dates
  start_date <- as.Date(start_date, format = currFormat)
  end_date <- as.Date(end_date, currFormat)
  
  # Generate months in the range
  months_seq <- seq(from = as.Date(format(start_date, "%Y-%m-01")),
                    to = as.Date(format(end_date, "%Y-%m-01")),
                    by = "month")
  
  months_num <- as.numeric(format(months_seq, "%m"))
  
  # Map to seasons
  get_season_south <- function(month) {
    season <- character(length(month))
    season[month %in% c(12, 1, 2)] <- "Summer"
    season[month %in% c(3, 4, 5)]  <- "Autumn"
    season[month %in% c(6, 7, 8)]  <- "Winter"
    season[month %in% c(9, 10, 11)] <- "Spring"
    return(unique(season))
  }
  
  return(get_season_south(months_num))
}

for (row in 1:nrow(df_correct_class)) {
  if (is.na(df_correct_class$trip_start[row]) | is.na(df_correct_class$trip_end[row])) {
    Summer[row] <- NA
    Winter[row] <- NA
    Autumn[row] <- NA
    Spring[row] <- NA
    next
  }
  northSeasons <- get_covered_seasons_north(df_correct_class$trip_start[row], 
                                            df_correct_class$trip_end[row])
  southSeasons <- get_covered_seasons_south(df_correct_class$trip_start[row],
                                            df_correct_class$trip_end[row])
  Summer[row] <- ((df_correct_class$north[row] & "Summer" %in% northSeasons) |
                  (df_correct_class$south[row] & "Summer" %in% southSeasons))
  Spring[row] <- ((df_correct_class$north[row] & "Spring" %in% northSeasons) | 
                  (df_correct_class$south[row] & "Spring" %in% southSeasons))
  Winter[row] <- ((df_correct_class$north[row] & "Winter" %in% northSeasons) | 
                  (df_correct_class$south[row] & "Winter" %in% southSeasons))
  Autumn[row] <- ((df_correct_class$north[row] & "Autumn" %in% northSeasons) |
                  (df_correct_class$south[row] & "Autumn" %in% southSeasons))
}

df_correct_class <- cbind(df_correct_class, Summer, Autumn, Winter, Spring)
df_correct_class$trip_start <- NULL
df_correct_class$trip_end <- NULL
df_correct_class$platform <- as.factor(df_correct_class$platform)

df_correct_class$traveller_type <- as.factor(df_correct_class$traveller_type)

for (boost in boosts) {
  df_correct_class[boost] <- df[boost]
}

logicalToFactor <- function(x) {
  if(is.logical(x)) {
    return(as.factor(x))
  }
  return(x)
}

df_correct_class <- as.data.frame(lapply(df_correct_class, logicalToFactor))

set.seed(42)
imputed <- missForest(df_correct_class)

df_cleaned <- imputed$ximp

names(df_cleaned) <- make.names(names(df_cleaned))

# Identify your target and predictor columns
targets <- c("Specified.Items", "Adventure.Activities", "Snow.Sports", 
             "Extra.Cancellation", "Existing.Medical.Condition.s.",
             "Rental.Vehicle.Insurance.Excess", "Cruise.Cover", "Gadget.Cover",
             "Motorcycle.Cover")
predictors <- setdiff(names(df_cleaned), targets)

# Loop through targets
models <- lapply(targets, function(target) {
  f <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  p <- prop.table(table(df_cleaned[target]))
  class_weights <- 1 / p
  class_weights <- class_weights /  sum(class_weights)
  ranger(f, data = df_cleaned, num.trees = 100, importance = "permutation",
         classification = TRUE, class.weights = class_weights, probability = TRUE)
})

names(models) <- targets

top6_list <- lapply(models, function(m) {
  imp <- sort(m$variable.importance, decreasing = TRUE)
  head(names(imp), 6)
})


common_vars <- Reduce(intersect, top6_list)

unique_top_vars <- lapply(top6_list, function(x) setdiff(x, common_vars))

plots <- list()

for (model_name in names(unique_top_vars)) {
  model <- models[[model_name]]
  vars  <- unique_top_vars[[model_name]]
  
  for (v in vars) {
    pd <- partial(
      model,
      pred.var = v,
      train = df_cleaned,
      which.class = "TRUE",   # for classification
      prob = TRUE
    )
    
    g <- autoplot(pd) +
      labs(title = paste(model_name),
           x = v, y = "Predicted Probability") +
      theme_minimal()
    
    plots[[paste(model_name, v, sep = "_")]] <- g
  }
}

wrap_plots(plots[1:8], ncol = 4)

grid <- expand.grid(
  log_ppp = quantile(df_cleaned$log_ppp, probs = c(0.25, 0.5, 0.75)),
  Human.rights.index = quantile(df_cleaned$Human.rights.index, probs = c(0.25, 0.5, 0.75)),
  Rule.of.Law.Index = quantile(df_cleaned$Rule.of.Law.Index, probs = c(0.25, 0.5, 0.75)),
  water_death_rate = quantile(df_cleaned$water_death_rate, probs = c(0.25, 0.5, 0.75)),
  avg_crime_rate = quantile(df_cleaned$avg_crime_rate, probs = c(0.25, 0.5, 0.75))
)

missing_vars <- setdiff(names(df_cleaned), names(grid))
for (v in missing_vars) {
  if (is.numeric(df_cleaned[[v]])) {
    grid[[v]] <- mean(df_cleaned[[v]], na.rm = TRUE)
  } else {
    grid[[v]] <- factor(levels(df_cleaned[[v]])[1], levels = levels(df_cleaned[[v]]))
  }
}

grid <- grid %>%
  mutate(
    log_pppbin = cut(log_ppp, breaks = 3,
                     labels = c("Weak", "Average", "High")),
    Human.rights.index_bin = cut(Human.rights.index, breaks = 3,
                                 c("Low index", "Medium index", "High index")),
    Rule.of.Law.Index_bin = cut(Rule.of.Law.Index, breaks = 3)
  )

for (name in names(models)) {
  grid[, name] <- predict(models[[name]], data = grid, type = "response")$predictions[, "TRUE"]
}

ggplot(grid, aes(x = water_death_rate, y = avg_crime_rate, fill = Adventure.Activities)) +
  geom_tile() +
  facet_grid(log_pppbin ~ Human.rights.index_bin) +
  scale_fill_viridis_c() +
  labs(title = "Conversion of Adventure Activities - Bins are Human Rights Index and Log PPP",
       x = "Water Death Rate", y = "Average Crime Rate", fill = "Predicted Prob")

ggplot(grid, aes(x = water_death_rate, y = avg_crime_rate, fill = Gadget.Cover)) +
  geom_tile() +
  facet_grid(log_pppbin ~ Human.rights.index_bin) +
  scale_fill_viridis_c() +
  labs(title = "Conversion of Gadget Cover - Bins are Human Rights Index and Log PPP",
       x = "Water Death Rate", y = "Average Crime Rate", fill = "Predicted Prob")

ggplot(grid, aes(x = water_death_rate, y = avg_crime_rate, fill = Rental.Vehicle.Insurance.Excess)) +
  geom_tile() +
  facet_grid(log_pppbin ~ Human.rights.index_bin) +
  scale_fill_viridis_c() +
  labs(title = "Conversion of Rental Vehicle Insurance Excess - Bins are Human Rights Index and Log PPP",
       x = "Water Death Rate", y = "Average Crime Rate", fill = "Predicted Prob")
