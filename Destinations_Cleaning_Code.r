# === Comprehensive Destination Cleaning w/ Countries + Explicit Regions (keeps older columns) ===
# This script:
# 1) Keeps ALL original output columns:
#    - cleaned_destinations, countries_only, regions_only, country_count, region_count
# 2) Adds NEW columns driven by explicit region logic:
#    - Explicit Regions Only
#    - Countries + Explicit Regions (No Auto-Region)
#    - Country-or-ExplicitRegion (Prefer Country)
#    - explicit_region_count
# 3) Preserves presentation layer (renames + relocates the key columns, drops unwanted cols)
# 4) Saves to: Freely_destinations_fully_cleaned_v3_country_plus_explicit_regions.csv

library(dplyr)
library(stringr)
library(tidyr)

# Read the data (expects a column named `destinations`)
df <- read.csv("Freely_cleaned_destinations.csv", stringsAsFactors = FALSE)

# Define regions universe
regions_list <- c(
  "Europe", "Asia", "Africa", "North America", "South America", 
  "Oceania", "Middle East", "Caribbean", "Pacific Islands",
  "Central America", "Southeast Asia", "East Asia", "South Asia",
  "Central Asia", "Western Europe", "Eastern Europe", "Northern Europe",
  "Southern Europe", "Scandinavia", "Baltic States", "Balkans", "Worldwide",
  "Antarctica"
)

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
  
  # UAE & Qatar
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
  
  # Turkey cities
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

# Helper: determine region for a given country (from region_countries map)
get_region <- function(country) {
  for (region in names(region_countries)) {
    if (country %in% region_countries[[region]]) {
      return(region)
    }
  }
  return(NA)
}

# Parser that tracks EXPLICIT region mentions separately from inferred regions
parse_destinations <- function(dest_string) {
  if (is.na(dest_string) || dest_string == "" || trimws(dest_string) == "") {
    return(list(countries = character(0), regions = character(0), explicit_regions = character(0)))
  }
  
  dest_string_lower <- tolower(as.character(dest_string))
  all_countries <- c()
  explicit_regions <- c()
  
  # Split by common separators and the word 'and'
  dest_list <- unlist(strsplit(dest_string_lower, "[,;/&+]|\\band\\b"))
  dest_list <- trimws(dest_list)
  dest_list <- dest_list[dest_list != ""]
  
  for (dest in dest_list) {
    d <- trimws(dest)
    if (nchar(d) < 2) next
    
    # Handle "All of ..." patterns (explicit region capture + special rule for Pacific)
    if (grepl("^all of", d)) {
      r <- d
      r <- str_replace(r, "^all of\\s+", "")
      r <- str_replace(r, "^the\\s+", "")
      r <- gsub("\\s*\\([^)]+\\)", "", r) # remove parentheses content
      r <- trimws(r)
      
      if (grepl("^uk$|^united kingdom$", r)) {
        all_countries <- c(all_countries, "United Kingdom")
        # treat as explicitly covering Europe
        if (!("Europe" %in% explicit_regions)) explicit_regions <- c(explicit_regions, "Europe")
        next
      } else if (grepl("^europe", r)) {
        explicit_regions <- c(explicit_regions, "Europe"); next
      } else if (grepl("^asia", r)) {
        explicit_regions <- c(explicit_regions, "Asia"); next
      } else if (grepl("^south america", r)) {
        explicit_regions <- c(explicit_regions, "South America"); next
      } else if (grepl("^north america", r)) {
        explicit_regions <- c(explicit_regions, "North America"); next
      } else if (grepl("^africa", r)) {
        explicit_regions <- c(explicit_regions, "Africa"); next
      } else if (grepl("^oceania", r)) {
        explicit_regions <- c(explicit_regions, "Oceania"); next
      } else if (grepl("^caribbean", r)) {
        explicit_regions <- c(explicit_regions, "Caribbean"); next
      } else if (grepl("^middle east", r)) {
        explicit_regions <- c(explicit_regions, "Middle East"); next
      } else if (grepl("^pacific", r)) {
        # Special rule: "All of the Pacific (Pacific Islands)" -> add country "Pacific Islands" + explicit region "Oceania"
        all_countries <- c(all_countries, "Pacific Islands")
        if (!("Oceania" %in% explicit_regions)) explicit_regions <- c(explicit_regions, "Oceania")
        next
      } else if (grepl("^americas|^central america", r)) {
        explicit_regions <- c(explicit_regions, "Central America"); next
      }
    }
    
    # Explicit region tokens (non-All-of)
    if (grepl("^worldwide$|^world$", d)) {
      explicit_regions <- c(explicit_regions, "Worldwide"); next
    }
    if (grepl("south\\s?west\\s?pacific|southwest pacific", d)) {
      explicit_regions <- c(explicit_regions, "Oceania"); next
    }
    
    # If token is an exact region name (title case)
    dest_title <- tools::toTitleCase(d)
    if (dest_title %in% regions_list) {
      explicit_regions <- c(explicit_regions, dest_title); next
    }
    
    # City/location mapping to country
    matched <- FALSE
    for (location in names(location_to_country)) {
      if (grepl(paste0("\\b", location, "\\b"), d)) {
        country <- location_to_country[[location]]
        all_countries <- c(all_countries, country)
        matched <- TRUE
        break
      }
    }
    if (matched) next
    
    # Else assume country name with normalisations
    country_name <- tools::toTitleCase(d)
    if (grepl("korea", country_name, ignore.case = TRUE) && 
        !grepl("north", country_name, ignore.case = TRUE)) {
      country_name <- "South Korea"
    } else if (grepl("united states|u\\.s\\.|u\\.s\\.a|america", country_name, ignore.case = TRUE) &&
               !grepl("south america|central america", country_name, ignore.case = TRUE)) {
      country_name <- "USA"
    } else if (grepl("new zealand", country_name, ignore.case = TRUE) ||
               grepl("\\bnz\\b", d)) {
      country_name <- "New Zealand"
    } else if (grepl("turkey|türkiye|turkiye", country_name, ignore.case = TRUE)) {
      country_name <- "Turkey"
    } else if (grepl("uae", d) || grepl("united arab emirates", d)) {
      country_name <- "United Arab Emirates"
    } else if (grepl("burma", d)) {
      country_name <- "Myanmar"
    } else if (grepl("png", d) || grepl("papua new guinea", d)) {
      country_name <- "Papua New Guinea"
    }
    all_countries <- c(all_countries, country_name)
  }
  
  # Deduplicate
  all_countries <- unique(all_countries)
  explicit_regions <- unique(explicit_regions)
  
  # Build "regions" (legacy behaviour: explicit + inferred from countries)
  all_regions <- explicit_regions
  for (country in all_countries) {
    region <- get_region(country)
    if (!is.na(region) && !(region %in% all_regions)) {
      all_regions <- c(all_regions, region)
    }
  }
  all_regions <- unique(all_regions)
  
  return(list(
    countries = all_countries,
    regions = all_regions,            # explicit + inferred (legacy semantics)
    explicit_regions = explicit_regions
  ))
}

# ==== Process all rows ====
pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)

# Initialize result columns (KEEP your original columns)
df$cleaned_destinations <- character(nrow(df))     # legacy combined (countries + regions incl. inferred)
df$countries_only <- character(nrow(df))           # countries only
df$regions_only <- character(nrow(df))             # regions (explicit + inferred)
df$country_count <- integer(nrow(df))              # count of countries
df$region_count <- integer(nrow(df))               # count of regions (explicit + inferred)

# NEW columns requested
df$explicit_regions_only <- character(nrow(df))                # regions explicitly mentioned in raw text only
df$country_plus_explicit_regions <- character(nrow(df))        # countries + explicit regions only (no auto-region)
df$country_or_explicit_region <- character(nrow(df))           # prefer countries; fallback to explicit regions
df$explicit_region_count <- integer(nrow(df))                  # count of explicit regions only

for (i in 1:nrow(df)) {
  setTxtProgressBar(pb, i)
  
  result <- parse_destinations(df$destinations[i])
  
  countries <- result$countries
  regions <- result$regions                    # explicit + inferred (legacy)
  explicit_regions <- result$explicit_regions  # explicit only
  
  # (KEPT) legacy: countries + all regions (explicit + inferred)
  all_items <- c(countries, regions)
  df$cleaned_destinations[i] <- ifelse(length(all_items) > 0, 
                                       paste(all_items, collapse = ", "), 
                                       NA)
  
  # (KEPT) Countries Only
  df$countries_only[i] <- ifelse(length(countries) > 0, 
                                 paste(countries, collapse = ", "), 
                                 NA)
  
  # (KEPT) Regions Only (explicit + inferred)
  df$regions_only[i] <- ifelse(length(regions) > 0, 
                               paste(regions, collapse = ", "), 
                               NA)
  
  # (KEPT) Counts
  df$country_count[i] <- length(countries)
  df$region_count[i]  <- length(regions)
  
  # (NEW) Explicit Regions Only
  df$explicit_regions_only[i] <- ifelse(length(explicit_regions) > 0, 
                                        paste(explicit_regions, collapse = ", "), 
                                        NA)
  
  # (NEW) Countries + Explicit Regions (No Auto-Region)
  ce <- c(countries, explicit_regions)
  df$country_plus_explicit_regions[i] <- ifelse(length(ce) > 0, 
                                                paste(ce, collapse = ", "), 
                                                NA)
  
  # (NEW) Country-or-ExplicitRegion (Prefer Country)
  df$country_or_explicit_region[i] <- ifelse(length(countries) > 0,
                                             paste(countries, collapse = ", "),
                                             ifelse(length(explicit_regions) > 0,
                                                    paste(explicit_regions, collapse = ", "),
                                                    NA))
  
  # (NEW) Explicit region count
  df$explicit_region_count[i] <- length(explicit_regions)
}
close(pb)

# Replace empty strings with NA
na_empty <- function(x) { x[x == ""] <- NA; x }
df$cleaned_destinations <- na_empty(df$cleaned_destinations)
df$countries_only <- na_empty(df$countries_only)
df$regions_only <- na_empty(df$regions_only)
df$explicit_regions_only <- na_empty(df$explicit_regions_only)
df$country_plus_explicit_regions <- na_empty(df$country_plus_explicit_regions)
df$country_or_explicit_region <- na_empty(df$country_or_explicit_region)

# ===== Presentation layer (KEEP your previous renames/order; add new columns alongside) =====

# 1) Rename columns for presentation
df <- df %>%
  dplyr::rename(
    `Original (uncleaned destinations)` = destinations,
    `Countries Only` = countries_only,
    `Regions Only` = regions_only,                                        # explicit + inferred (unchanged)
    `Explicit Regions Only` = explicit_regions_only,                      # NEW
    `Destinations (Countries AND Regions)` = cleaned_destinations,        # legacy combined
    `Countries + Explicit Regions (No Auto-Region)` = country_plus_explicit_regions, # NEW
    `Country-or-ExplicitRegion (Prefer Country)` = country_or_explicit_region       # NEW
  )

# 2) Reorder so they sit side-by-side in this order (you can tweak this order if desired)
desired_block <- c(
  "Original (uncleaned destinations)",
  "Countries Only",
  "Explicit Regions Only",
  "Countries + Explicit Regions (No Auto-Region)",
  "Regions Only",
  "Destinations (Countries AND Regions)"
)
df <- df %>% dplyr::relocate(dplyr::all_of(desired_block), .before = 1)

# 3) Remove unwanted columns (ignore if not present)
cols_to_drop <- c("primary_country", "country_name_list", "region_name_list", "unknown_tokens", "unknown_n")
df <- df %>% dplyr::select(-dplyr::any_of(cols_to_drop))

# ===== Summary =====
cat("\n\nCleaning Summary:\n")
cat("================\n")
cat("Total rows processed:", nrow(df), "\n")
cat("Rows with countries:", sum(!is.na(df$`Countries Only`)), "\n")
cat("Rows with explicit regions:", sum(!is.na(df$`Explicit Regions Only`)), "\n")
cat("Rows with countries + explicit regions:", sum(!is.na(df$`Countries + Explicit Regions (No Auto-Region)`)), "\n")
cat("Rows with regions (explicit + inferred):", sum(!is.na(df$`Regions Only`)), "\n")

cat("\n\nCountry count distribution:\n")
print(table(df$country_count))

cat("\n\nExplicit region count distribution:\n")
print(table(df$explicit_region_count))

cat("\n\n(All) region count distribution (explicit + inferred):\n")
print(table(df$region_count))

# Save the cleaned data
output_file <- "Freely_destinations_fully_cleaned_v3_country_plus_explicit_regions.csv"
write.csv(df, output_file, row.names = FALSE)
cat("\n\nCleaned data saved to:", output_file, "\n")
