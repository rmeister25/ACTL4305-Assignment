# Comprehensive Destination Cleaning Script with Countries and Regions

library(dplyr)
library(stringr)
library(tidyr)

# Read the data
df <- read.csv("Freely_cleaned_destinations.csv", stringsAsFactors = FALSE)

# Define regions
regions_list <- c("Europe", "Asia", "Africa", "North America", "South America", 
                  "Oceania", "Middle East", "Caribbean", "Pacific Islands",
                  "Central America", "Southeast Asia", "East Asia", "South Asia",
                  "Central Asia", "Western Europe", "Eastern Europe", "Northern Europe",
                  "Southern Europe", "Scandinavia", "Baltic States", "Balkans", "Worldwide",
                  "Antarctica")

# Define countries by region
region_countries <- list(
  "Europe" = c("United Kingdom", "France", "Italy", "Spain", "Germany", "Netherlands", 
               "Belgium", "Switzerland", "Austria", "Portugal", "Greece", "Norway", 
               "Sweden", "Denmark", "Finland", "Iceland", "Ireland", "Poland", 
               "Czech Republic", "Hungary", "Croatia", "Slovenia", "Estonia", 
               "Latvia", "Lithuania", "Romania", "Bulgaria", "Serbia", "Montenegro",
               "Albania", "North Macedonia", "Bosnia and Herzegovina", "Slovakia",
               "Luxembourg", "Monaco", "Liechtenstein", "Andorra", "Malta", "Cyprus"),
  "Asia" = c("Japan", "China", "South Korea", "Thailand", "Vietnam", "Singapore", 
             "Malaysia", "Indonesia", "Philippines", "Cambodia", "Laos", "Myanmar",
             "Brunei", "East Timor", "India", "Nepal", "Sri Lanka", "Bhutan", 
             "Bangladesh", "Pakistan", "Afghanistan", "Maldives", "Kazakhstan",
             "Uzbekistan", "Turkmenistan", "Kyrgyzstan", "Tajikistan", "Mongolia",
             "Hong Kong", "Macau", "Taiwan", "Timor-Leste"),
  "Middle East" = c("United Arab Emirates", "Turkey", "Israel", "Jordan", "Lebanon",
                    "Saudi Arabia", "Qatar", "Bahrain", "Kuwait", "Oman", "Yemen",
                    "Iraq", "Iran", "Syria", "Armenia", "Azerbaijan", "Georgia"),
  "Africa" = c("South Africa", "Kenya", "Tanzania", "Morocco", "Egypt", "Tunisia",
               "Zimbabwe", "Zambia", "Botswana", "Namibia", "Mauritius", "Seychelles",
               "Madagascar", "Rwanda", "Uganda", "Ethiopia", "Ghana", "Nigeria",
               "Senegal", "Mozambique", "Malawi", "Angola", "Benin", "Eritrea",
               "Gambia", "Liberia", "Mali", "Sierra Leone", "South Sudan", "Togo",
               "Cape Verde", "Lesotho", "Eswatini", "Algeria", "Reunion"),
  "Oceania" = c("Australia", "New Zealand", "Fiji", "Papua New Guinea", "Solomon Islands",
                "Vanuatu", "Samoa", "Tonga", "Cook Islands", "French Polynesia",
                "New Caledonia", "Palau", "Micronesia", "Marshall Islands", "Kiribati",
                "Tuvalu", "Nauru", "Norfolk Island", "Pacific Islands"),
  "North America" = c("USA", "Canada", "Mexico", "Bermuda", "Greenland"),
  "South America" = c("Brazil", "Argentina", "Chile", "Peru", "Colombia", "Ecuador",
                      "Venezuela", "Bolivia", "Paraguay", "Uruguay", "Guyana",
                      "Suriname", "French Guiana"),
  "Caribbean" = c("Jamaica", "Bahamas", "Barbados", "Trinidad and Tobago", "Cuba",
                  "Dominican Republic", "Haiti", "Puerto Rico", "Cayman Islands",
                  "Aruba", "Curacao", "Saint Lucia", "Antigua and Barbuda",
                  "Grenada", "Saint Vincent and the Grenadines", "Saint Kitts and Nevis",
                  "Dominica", "Turks and Caicos", "British Virgin Islands",
                  "US Virgin Islands", "Martinique", "Guadeloupe", "Anguilla",
                  "Virgin Islands"),
  "Central America" = c("Costa Rica", "Panama", "Guatemala", "Belize", "Honduras",
                        "El Salvador", "Nicaragua"),
  "Antarctica" = c("Antarctica")
)

# Comprehensive city/location to country mapping
location_to_country <- list(
  # Australia - all states and major cities
  "adelaide" = "Australia", "south australia" = "Australia",
  "brisbane" = "Australia", "queensland" = "Australia",
  "cairns" = "Australia", "gold coast" = "Australia",
  "melbourne" = "Australia", "victoria" = "Australia",
  "perth" = "Australia", "western australia" = "Australia",
  "sydney" = "Australia", "new south wales" = "Australia", "nsw" = "Australia",
  "hobart" = "Australia", "tasmania" = "Australia",
  "darwin" = "Australia", "northern territory" = "Australia",
  "canberra" = "Australia", "act" = "Australia",
  "uluru" = "Australia", "whitsundays" = "Australia",
  "great barrier reef" = "Australia", "byron bay" = "Australia",
  "noosa" = "Australia", "sunshine coast" = "Australia",
  "airlie beach" = "Australia", "alice springs" = "Australia",
  "broome" = "Australia", "port douglas" = "Australia",
  "kangaroo island" = "Australia", "lord howe island" = "Australia",
  "domestic cruise" = "Australia",
  
  # New Zealand - handle NZ variation
  "auckland" = "New Zealand", "wellington" = "New Zealand",
  "christchurch" = "New Zealand", "queenstown" = "New Zealand",
  "rotorua" = "New Zealand", "dunedin" = "New Zealand",
  "hamilton" = "New Zealand", "tauranga" = "New Zealand",
  "napier" = "New Zealand", "palmerston north" = "New Zealand",
  "new zealand" = "New Zealand", "nz" = "New Zealand",
  
  # Indonesia
  "bali" = "Indonesia", "jakarta" = "Indonesia",
  "lombok" = "Indonesia", "yogyakarta" = "Indonesia",
  "sumatra" = "Indonesia", "java" = "Indonesia",
  "sulawesi" = "Indonesia", "komodo" = "Indonesia",
  "gili islands" = "Indonesia", "ubud" = "Indonesia",
  "denpasar" = "Indonesia", "kuta" = "Indonesia",
  
  # Thailand
  "bangkok" = "Thailand", "phuket" = "Thailand",
  "koh samui" = "Thailand", "krabi" = "Thailand",
  "chiang mai" = "Thailand", "pattaya" = "Thailand",
  "koh phi phi" = "Thailand", "koh tao" = "Thailand",
  "koh phangan" = "Thailand", "ayutthaya" = "Thailand",
  "hua hin" = "Thailand", "chiang rai" = "Thailand",
  
  # Japan
  "tokyo" = "Japan", "osaka" = "Japan", "kyoto" = "Japan",
  "okinawa" = "Japan", "hokkaido" = "Japan", "hiroshima" = "Japan",
  "nagoya" = "Japan", "fukuoka" = "Japan", "sapporo" = "Japan",
  "nara" = "Japan", "yokohama" = "Japan", "kobe" = "Japan",
  
  # United States - all variations map to USA
  "new york" = "USA", "los angeles" = "USA",
  "san francisco" = "USA", "las vegas" = "USA",
  "orlando" = "USA", "miami" = "USA",
  "hawaii" = "USA", "honolulu" = "USA",
  "maui" = "USA", "chicago" = "USA",
  "boston" = "USA", "seattle" = "USA",
  "washington dc" = "USA", "new orleans" = "USA",
  "san diego" = "USA", "nashville" = "USA",
  "austin" = "USA", "portland" = "USA",
  "california" = "USA", "florida" = "USA",
  "texas" = "USA", "alaska" = "USA",
  "united states" = "USA", "united states of america" = "USA",
  "usa" = "USA", "us" = "USA",
  "arizona" = "USA", "colorado" = "USA", "connecticut" = "USA",
  "indiana" = "USA", "minnesota" = "USA", "nebraska" = "USA",
  "new jersey" = "USA", "north carolina" = "USA", "oregon" = "USA",
  "utah" = "USA", "virginia" = "USA",
  
  # United Kingdom
  "london" = "United Kingdom", "scotland" = "United Kingdom",
  "wales" = "United Kingdom", "england" = "United Kingdom",
  "edinburgh" = "United Kingdom", "manchester" = "United Kingdom",
  "liverpool" = "United Kingdom", "birmingham" = "United Kingdom",
  "glasgow" = "United Kingdom", "belfast" = "United Kingdom",
  "northern ireland" = "United Kingdom", "cardiff" = "United Kingdom",
  "oxford" = "United Kingdom", "cambridge" = "United Kingdom",
  "bath" = "United Kingdom", "york" = "United Kingdom",
  "glasglow" = "United Kingdom",
  
  # France
  "paris" = "France", "nice" = "France", "lyon" = "France",
  "marseille" = "France", "bordeaux" = "France",
  "cannes" = "France", "monaco" = "France", "corsica" = "France",
  "provence" = "France", "normandy" = "France", "toulouse" = "France",
  
  # Italy
  "rome" = "Italy", "venice" = "Italy", "florence" = "Italy",
  "milan" = "Italy", "amalfi" = "Italy", "naples" = "Italy",
  "sicily" = "Italy", "sardinia" = "Italy", "turin" = "Italy",
  "pisa" = "Italy", "verona" = "Italy", "bologna" = "Italy",
  "capri" = "Italy", "cinque terre" = "Italy", "positano" = "Italy",
  
  # Spain
  "barcelona" = "Spain", "madrid" = "Spain", "seville" = "Spain",
  "ibiza" = "Spain", "mallorca" = "Spain", "valencia" = "Spain",
  "granada" = "Spain", "bilbao" = "Spain", "malaga" = "Spain",
  "canary islands" = "Spain", "tenerife" = "Spain",
  
  # Germany
  "berlin" = "Germany", "munich" = "Germany", "frankfurt" = "Germany",
  "hamburg" = "Germany", "cologne" = "Germany", "dresden" = "Germany",
  
  # Netherlands
  "amsterdam" = "Netherlands", "rotterdam" = "Netherlands",
  "the hague" = "Netherlands", "utrecht" = "Netherlands",
  "holland" = "Netherlands", "Netherlands (The Netherlands)" = "Netherlands",
  
  # Greece
  "athens" = "Greece", "santorini" = "Greece", "mykonos" = "Greece",
  "crete" = "Greece", "rhodes" = "Greece", "corfu" = "Greece",
  "thessaloniki" = "Greece", "zakynthos" = "Greece",
  
  # UAE
  "dubai" = "United Arab Emirates", "abu dhabi" = "United Arab Emirates",
  "uae" = "United Arab Emirates", "united arab emirates" = "United Arab Emirates",
  "doha" = "Qatar",
  
  # Singapore & Hong Kong & Macau & Taiwan
  "singapore" = "Singapore", 
  "hong kong" = "Hong Kong",
  "macau" = "Macau", "macao" = "Macau",
  "taiwan" = "Taiwan", "taipei" = "Taiwan",
  
  # Turkey variations
  "turkey" = "Turkey", "türkiye" = "Turkey", "turkiye" = "Turkey",
  
  # Malaysia
  "kuala lumpur" = "Malaysia", "kl" = "Malaysia",
  "penang" = "Malaysia",
  "langkawi" = "Malaysia", "malacca" = "Malaysia",
  "sabah" = "Malaysia", "sarawak" = "Malaysia",
  
  # Vietnam
  "hanoi" = "Vietnam", "ho chi minh" = "Vietnam",
  "saigon" = "Vietnam", "da nang" = "Vietnam",
  "hoi an" = "Vietnam", "nha trang" = "Vietnam",
  "halong bay" = "Vietnam", "hue" = "Vietnam",
  
  # Cambodia
  "siem reap" = "Cambodia", "phnom penh" = "Cambodia",
  "angkor" = "Cambodia", "angkor wat" = "Cambodia",
  
  # Laos
  "luang prabang" = "Laos", "vientiane" = "Laos",
  
  # Philippines
  "manila" = "Philippines", "cebu" = "Philippines",
  "boracay" = "Philippines", "palawan" = "Philippines",
  "el nido" = "Philippines", "siargao" = "Philippines",
  
  # Papua New Guinea
  "papua new guinea" = "Papua New Guinea", "png" = "Papua New Guinea",
  
  # South Korea (handle variations)
  "seoul" = "South Korea", "busan" = "South Korea",
  "jeju" = "South Korea", "korea" = "South Korea",
  "south korea" = "South Korea",
  
  # China
  "beijing" = "China", "shanghai" = "China",
  "guilin" = "China", "xi'an" = "China", "xian" = "China",
  "guangzhou" = "China",
  "shenzhen" = "China", "tibet" = "China",
  
  # India
  "delhi" = "India", "mumbai" = "India", "jaipur" = "India",
  "goa" = "India", "kerala" = "India", "agra" = "India",
  "varanasi" = "India", "bangalore" = "India",
  "kolkata" = "India", "chennai" = "India",
  
  # Nepal & Sri Lanka & Bangladesh
  "kathmandu" = "Nepal", "pokhara" = "Nepal",
  "colombo" = "Sri Lanka", "galle" = "Sri Lanka",
  "kandy" = "Sri Lanka", "srilanka" = "Sri Lanka", "sri lanka" = "Sri Lanka",
  "dhaka" = "Bangladesh",
  
  # Maldives
  "male" = "Maldives", "maldives" = "Maldives",
  
  # Middle East - additional countries
  "armenia" = "Armenia",
  "azerbaijan" = "Azerbaijan",
  "georgia" = "Georgia",
  
  # Turkey
  "istanbul" = "Turkey", "cappadocia" = "Turkey",
  "antalya" = "Turkey", "bodrum" = "Turkey",
  
  # Egypt
  "cairo" = "Egypt", "luxor" = "Egypt",
  "sharm el sheikh" = "Egypt", "hurghada" = "Egypt",
  
  # Morocco
  "marrakech" = "Morocco", "casablanca" = "Morocco",
  "fez" = "Morocco", "rabat" = "Morocco",
  
  # Africa - additional countries and cities
  "angola" = "Angola",
  "algeria" = "Algeria",
  "benin" = "Benin",
  "cape verde" = "Cape Verde",
  "eritrea" = "Eritrea",
  "gambia" = "Gambia",
  "lesotho" = "Lesotho",
  "liberia" = "Liberia",
  "mali" = "Mali",
  "sierra leone" = "Sierra Leone",
  "south sudan" = "South Sudan",
  "swaziland" = "Eswatini",
  "togo" = "Togo",
  
  # South Africa
  "cape town" = "South Africa", "johannesburg" = "South Africa",
  "durban" = "South Africa", "kruger" = "South Africa",
  
  # Kenya & Tanzania
  "nairobi" = "Kenya", "mombasa" = "Kenya",
  "zanzibar" = "Tanzania", "dar es salaam" = "Tanzania",
  "arusha" = "Tanzania", "serengeti" = "Tanzania",
  
  # Zimbabwe
  "victoria falls" = "Zimbabwe",
  
  # South America
  "rio de janeiro" = "Brazil", "sao paulo" = "Brazil",
  "brasilia" = "Brazil",
  "buenos aires" = "Argentina", "lima" = "Peru",
  "cusco" = "Peru", "machu picchu" = "Peru",
  "santiago" = "Chile", "bogota" = "Colombia",
  "cartagena" = "Colombia", "quito" = "Ecuador",
  "galapagos" = "Ecuador",
  
  # Mexico
  "cancun" = "Mexico", "playa del carmen" = "Mexico",
  "cabo" = "Mexico", "mexico city" = "Mexico",
  "tulum" = "Mexico", "puerto vallarta" = "Mexico",
  
  # Central America
  "panama city" = "Panama",
  
  # Caribbean
  "anguilla" = "Anguilla",
  "antigua" = "Antigua and Barbuda", "barbuda" = "Antigua and Barbuda",
  "dominican republic" = "Dominican Republic", "dominican rep" = "Dominican Republic",
  "virgin islands" = "Virgin Islands",
  
  # Canada
  "toronto" = "Canada", "vancouver" = "Canada",
  "montreal" = "Canada", "banff" = "Canada",
  "calgary" = "Canada", "ottawa" = "Canada",
  "quebec city" = "Canada", "whistler" = "Canada",
  "british columbia" = "Canada",
  
  # Pacific Islands
  "bora bora" = "French Polynesia", "tahiti" = "French Polynesia",
  "moorea" = "French Polynesia", "fiji" = "Fiji",
  "nadi" = "Fiji", "suva" = "Fiji",
  "rarotonga" = "Cook Islands", "aitutaki" = "Cook Islands",
  "vanuatu" = "Vanuatu", "port vila" = "Vanuatu",
  "mystery island" = "Vanuatu",
  "noumea" = "New Caledonia", "new caledonia" = "New Caledonia",
  "lifou" = "New Caledonia",
  "norfolk island" = "Norfolk Island",
  "samoa" = "Samoa", "western samoa" = "Samoa",
  
  # Europe - more cities
  "reykjavik" = "Iceland", "oslo" = "Norway", "bergen" = "Norway",
  "stockholm" = "Sweden", "copenhagen" = "Denmark",
  "helsinki" = "Finland", "tallinn" = "Estonia",
  "riga" = "Latvia", "vilnius" = "Lithuania",
  "prague" = "Czech Republic", "budapest" = "Hungary",
  "vienna" = "Austria", "salzburg" = "Austria",
  "zurich" = "Switzerland", "geneva" = "Switzerland",
  "interlaken" = "Switzerland", "brussels" = "Belgium",
  "bruges" = "Belgium", "lisbon" = "Portugal",
  "porto" = "Portugal", "algarve" = "Portugal",
  "madeira" = "Portugal",
  "dublin" = "Ireland", "galway" = "Ireland",
  "republic of ireland" = "Ireland",
  "krakow" = "Poland", "warsaw" = "Poland",
  "dubrovnik" = "Croatia", "split" = "Croatia",
  "zagreb" = "Croatia", "ljubljana" = "Slovenia",
  "belgrade" = "Serbia",
  "bosnia" = "Bosnia and Herzegovina", "herzegovina" = "Bosnia and Herzegovina",
  "macedonia" = "North Macedonia",
  "lapland" = "Finland",
  
  # Antarctica
  "antarctica" = "Antarctica",
  "falkland islands" = "Falkland Islands",
  
  # Timor-Leste
  "timor-leste" = "Timor-Leste", "east timor" = "Timor-Leste",
  
  # Reunion
  "reunion" = "Reunion"
)

# Function to determine region for a country
get_region <- function(country) {
  for (region in names(region_countries)) {
    if (country %in% region_countries[[region]]) {
      return(region)
    }
  }
  return(NA)
}

# Function to parse and clean destinations
parse_destinations <- function(dest_string) {
  if (is.na(dest_string) || dest_string == "" || trimws(dest_string) == "") {
    return(list(countries = character(0), regions = character(0)))
  }
  
  dest_string_original <- dest_string
  dest_string <- tolower(as.character(dest_string))
  
  all_countries <- c()
  all_regions <- c()
  
  # Split by common separators first to get all items
  dest_list <- unlist(strsplit(dest_string, "[,;/&+]|\\band\\b"))
  dest_list <- trimws(dest_list)
  dest_list <- dest_list[dest_list != ""]
  
  # Process each item
  for (dest in dest_list) {
    dest_clean <- trimws(dest)
    
    # Skip empty
    if (nchar(dest_clean) < 2) next
    
    # Check for "All of..." pattern
    if (grepl("^all of", dest_clean)) {
      # Extract what comes after "all of"
      region_text <- str_replace(dest_clean, "^all of\\s+", "")
      region_text <- str_replace(region_text, "^the\\s+", "")
      region_text <- trimws(region_text)
      
      # Remove anything in parentheses for matching
      region_text_clean <- gsub("\\s*\\([^)]+\\)", "", region_text)
      region_text_clean <- trimws(region_text_clean)
      
      # Map "All of..." to specific regions/countries
      if (grepl("^uk$|^united kingdom$", region_text_clean)) {
        all_countries <- c(all_countries, "United Kingdom")
        all_regions <- c(all_regions, "Europe")
        next
      } else if (grepl("^europe", region_text_clean)) {
        all_regions <- c(all_regions, "Europe")
        next
      } else if (grepl("^asia", region_text_clean)) {
        all_regions <- c(all_regions, "Asia")
        next
      } else if (grepl("^south america", region_text_clean)) {
        all_regions <- c(all_regions, "South America")
        next
      } else if (grepl("^north america", region_text_clean)) {
        all_regions <- c(all_regions, "North America")
        next
      } else if (grepl("^africa", region_text_clean)) {
        all_regions <- c(all_regions, "Africa")
        next
      } else if (grepl("^oceania", region_text_clean)) {
        all_regions <- c(all_regions, "Oceania")
        next
      } else if (grepl("^caribbean", region_text_clean)) {
        all_regions <- c(all_regions, "Caribbean")
        next
      } else if (grepl("^middle east", region_text_clean)) {
        all_regions <- c(all_regions, "Middle East")
        next
      } else if (grepl("^pacific", region_text_clean)) {
        # Check if it mentions Pacific Islands specifically
        if (grepl("pacific islands", region_text_clean)) {
          all_countries <- c(all_countries, "Pacific Islands")
          all_regions <- c(all_regions, "Oceania")
        } else {
          all_regions <- c(all_regions, "Oceania")
        }
        next
      } else if (grepl("^americas|^central america", region_text_clean)) {
        all_regions <- c(all_regions, "Central America")
        next
      }
    }
    
    # Check if it's "worldwide"
    if (grepl("^worldwide$|^world$", dest_clean)) {
      all_regions <- c(all_regions, "Worldwide")
      next
    }
    
    # Check for "South West Pacific" patterns
    if (grepl("south west pacific", dest_clean) || grepl("southwest pacific", dest_clean)) {
      all_regions <- c(all_regions, "Oceania")
      next
    }
    
    # Check if it's a region name
    dest_title <- tools::toTitleCase(dest_clean)
    if (dest_title %in% regions_list) {
      all_regions <- c(all_regions, dest_title)
      next
    }
    
    # Check if it's in our city/location mapping
    matched <- FALSE
    for (location in names(location_to_country)) {
      if (grepl(paste0("\\b", location, "\\b"), dest_clean)) {
        country <- location_to_country[[location]]
        all_countries <- c(all_countries, country)
        matched <- TRUE
        break
      }
    }
    
    # If not matched, assume it's a country name
    if (!matched) {
      country_name <- tools::toTitleCase(dest_clean)
      
      # Handle variations
      if (grepl("korea", country_name, ignore.case = TRUE) && 
          !grepl("north", country_name, ignore.case = TRUE)) {
        country_name <- "South Korea"
      } else if (grepl("united states|u\\.s\\.|u\\.s\\.a|america", country_name, ignore.case = TRUE) &&
                 !grepl("south america|central america", country_name, ignore.case = TRUE)) {
        country_name <- "USA"
      } else if (grepl("new zealand", country_name, ignore.case = TRUE) ||
                 grepl("\\bnz\\b", dest_clean)) {
        country_name <- "New Zealand"
      } else if (grepl("turkey|türkiye|turkiye", country_name, ignore.case = TRUE)) {
        country_name <- "Turkey"
      } else if (grepl("uae", dest_clean) || grepl("united arab emirates", dest_clean)) {
        country_name <- "United Arab Emirates"
      } else if (grepl("burma", dest_clean)) {
        country_name <- "Myanmar"
      } else if (grepl("png", dest_clean) || grepl("papua new guinea", dest_clean)) {
        country_name <- "Papua New Guinea"
      }
      
      all_countries <- c(all_countries, country_name)
    }
  }
  
  # Remove duplicates
  all_countries <- unique(all_countries)
  
  # Add regions for countries if not already specified
  for (country in all_countries) {
    region <- get_region(country)
    if (!is.na(region) && !(region %in% all_regions)) {
      all_regions <- c(all_regions, region)
    }
  }
  
  all_regions <- unique(all_regions)
  
  return(list(countries = all_countries, regions = all_regions))
}

# Process all rows

pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)

# Initialize result columns
df$cleaned_destinations <- character(nrow(df))
df$countries_only <- character(nrow(df))
df$regions_only <- character(nrow(df))
df$country_count <- integer(nrow(df))
df$region_count <- integer(nrow(df))

for (i in 1:nrow(df)) {
  setTxtProgressBar(pb, i)
  
  result <- parse_destinations(df$destinations[i])
  
  countries <- result$countries
  regions <- result$regions
  
  # Build cleaned destinations with both countries and regions
  all_items <- c(countries, regions)
  df$cleaned_destinations[i] <- ifelse(length(all_items) > 0, 
                                       paste(all_items, collapse = ", "), 
                                       NA)
  
  df$countries_only[i] <- ifelse(length(countries) > 0, 
                                 paste(countries, collapse = ", "), 
                                 NA)
  
  df$regions_only[i] <- ifelse(length(regions) > 0, 
                               paste(regions, collapse = ", "), 
                               NA)
  
  df$country_count[i] <- length(countries)
  df$region_count[i] <- length(regions)
}

close(pb)

# Replace empty strings with NA
df$cleaned_destinations[df$cleaned_destinations == ""] <- NA
df$countries_only[df$countries_only == ""] <- NA
df$regions_only[df$regions_only == ""] <- NA





# 1) Rename columns
df <- df %>%
  dplyr::rename(
    `Original (uncleaned destinations)` = destinations,
    `Countries Only` = countries_only,
    `Regions Only` = regions_only,
    `Destinations (Countries AND Regions)` = cleaned_destinations
  )

# 2) Reorder so they sit side-by-side in this order:
desired_block <- c(
  "Original (uncleaned destinations)",
  "Countries Only",
  "Regions Only",
  "Destinations (Countries AND Regions)"
)
df <- df %>% dplyr::relocate(dplyr::all_of(desired_block), .before = 1)

# 3) Remove unwanted columns (ignore if not present)
cols_to_drop <- c("primary_country", "country_name_list", "region_name_list", "unknown_tokens", "unknown_n")
df <- df %>% dplyr::select(-dplyr::any_of(cols_to_drop))


cat("\n\nCleaning Summary:\n")
cat("================\n")
cat("Total rows processed:", nrow(df), "\n")
cat("Rows with destinations (countries AND regions):",
    sum(!is.na(df$`Destinations (Countries AND Regions)`)), "\n")
cat("Rows with countries:", sum(!is.na(df$`Countries Only`)), "\n")
cat("Rows with regions:", sum(!is.na(df$`Regions Only`)), "\n")

cat("\n\nCountry count distribution:\n")
print(table(df$country_count))

cat("\n\nRegion count distribution:\n")
print(table(df$region_count))

# Save the cleaned data
output_file <- "Freely_destinations_fully_cleaned_v2.csv"
write.csv(df, output_file, row.names = FALSE)
cat("\n\nCleaned data saved to:", output_file, "\n")

