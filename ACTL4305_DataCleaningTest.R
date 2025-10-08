library(dplyr)
library(stringr)
library(lubridate)

# -------- Load --------
df <- read.csv("Freely.csv", header = TRUE, stringsAsFactors = FALSE)

# -------- Normalize raw strings --------
# quote_create_time_date from quote_create_time
df$quote_create_time_date <- trimws(df$quote_create_time)
df$quote_create_time_date <- gsub("-", "/", df$quote_create_time_date)
df$quote_create_time_date <- sub(" .*", "", df$quote_create_time_date)
df$quote_create_time_date <- gsub("^([0-9]{4})/(\\d{1,2})/(\\d{1,2})$", "\\2/\\3/\\1",
                                  df$quote_create_time_date)

core_cols  <- c("quote_create_time_date", "trip_start_date", "trip_end_date")
boost_cols <- as.vector(sapply(1:8, function(k) c(paste0("boost_",k,"_start_date"),
                                                  paste0("boost_",k,"_end_date"))))
date_cols  <- intersect(c(core_cols, boost_cols), names(df))

# normalize other date-like columns cheaply
normalize_other <- function(v) trimws(sub(" .*", "", gsub("-", "/", v)))
others <- setdiff(date_cols, "quote_create_time_date")
df[others] <- lapply(df[others], normalize_other)

# -------- Pre-parse once per column (big speed-up) --------
dmy_cols <- setNames(lapply(date_cols, \(cn) suppressWarnings(dmy(df[[cn]], quiet = TRUE))), date_cols)
mdy_cols <- setNames(lapply(date_cols, \(cn) suppressWarnings(mdy(df[[cn]], quiet = TRUE))), date_cols)

# -------- Outputs --------
fmt    <- function(x) ifelse(!is.na(x), format(x, "%d/%m/%Y"), NA_character_)
in_win <- function(q) isTRUE(!is.na(q) && q >= dmy("01/10/2024") && q <= dmy("31/12/2024"))

for (cn in date_cols) df[[paste0(cn,"_clean")]] <- NA_character_
df$date_parser_choice <- NA_character_
df$date_flag <- NA

# -------- Row loop --------
n  <- nrow(df)
pb <- txtProgressBar(min = 0, max = n, style = 3)

for (i in seq_len(n)) {
  ## 1) Quote date (prefer the one in the Q4-2024 window)
  qd <- dmy_cols[["quote_create_time_date"]][i]
  qm <- mdy_cols[["quote_create_time_date"]][i]
  win_d <- in_win(qd); win_m <- in_win(qm)
  q <- if (win_d && !win_m) qd else if (!win_d && win_m) qm else if (!is.na(qd)) qd else qm
  if (is.na(q) || !in_win(q)) {                 # hard rule
    df$date_parser_choice[i] <- "BOTH_FAIL"; df$date_flag[i] <- TRUE
    if (i %% 1000 == 0 || i == n) setTxtProgressBar(pb, i); next
  }
  
  ## 2) Evaluate ALL trip combos and pick the one that fits the MOST boost pairs
  trip_combos <- list(
    list(ts=dmy_cols[["trip_start_date"]][i], te=dmy_cols[["trip_end_date"]][i], lab="DMY"),
    list(ts=dmy_cols[["trip_start_date"]][i], te=mdy_cols[["trip_end_date"]][i], lab="MIXED"),
    list(ts=mdy_cols[["trip_start_date"]][i], te=dmy_cols[["trip_end_date"]][i], lab="MIXED"),
    list(ts=mdy_cols[["trip_start_date"]][i], te=mdy_cols[["trip_end_date"]][i], lab="MDY")
  )
  
  fit_boosts_for_trip <- function(ts, te) {
    if (is.na(ts) || is.na(te) || !(q <= ts && ts <= te))
      return(list(ok=FALSE, mixed=FALSE, used=-1L, clean=list()))
    
    mixed <- FALSE
    used  <- 0L
    clean <- list()
    
    for (k in 1:8) {
      sname <- paste0("boost_",k,"_start_date")
      ename <- paste0("boost_",k,"_end_date")
      if (!(sname %in% date_cols && ename %in% date_cols)) next
      
      bs_d <- dmy_cols[[sname]][i]; be_d <- dmy_cols[[ename]][i]
      bs_m <- mdy_cols[[sname]][i]; be_m <- mdy_cols[[ename]][i]
      
      if (all(is.na(c(bs_d, bs_m, be_d, be_m)))) next     # no boost -> ignore
      # partial present -> fail this trip window
      if ((all(is.na(c(bs_d, bs_m))) && !all(is.na(c(be_d, be_m)))) ||
          (!all(is.na(c(bs_d, bs_m))) &&  all(is.na(c(be_d, be_m)))))
        return(list(ok=FALSE, mixed=mixed, used=-1L, clean=clean))
      
      bcombos <- list(
        list(bs=bs_d, be=be_d, lab="DMY"),
        list(bs=bs_d, be=be_m, lab="MIXED"),
        list(bs=bs_m, be=be_d, lab="MIXED"),
        list(bs=bs_m, be=be_m, lab="MDY")
      )
      pickb <- NULL
      for (bc in bcombos) {
        if (!is.na(bc$bs) && !is.na(bc$be) && ts <= bc$bs && bc$bs <= bc$be && bc$be <= te) {
          pickb <- bc; break
        }
      }
      if (is.null(pickb)) return(list(ok=FALSE, mixed=mixed, used=-1L, clean=clean))
      clean[[paste0(sname,"_clean")]] <- fmt(pickb$bs)
      clean[[paste0(ename,"_clean")]] <- fmt(pickb$be)
      if (pickb$lab != "DMY") mixed <- TRUE
      used <- used + 1L
    }
    list(ok=TRUE, mixed=mixed, used=used, clean=clean)
  }
  
  # score each trip combo
  best <- list(ok=FALSE, used=-2L); best_idx <- NA_integer_
  for (idx in seq_along(trip_combos)) {
    c <- trip_combos[[idx]]
    res <- fit_boosts_for_trip(c$ts, c$te)
    # choose highest 'used' (count of boost pairs that fit); tie -> earlier idx (DMY first)
    if (res$ok && res$used > best$used) { best <- res; best_idx <- idx }
  }
  
  if (is.na(best_idx)) {                           # no trip window can satisfy boosts
    df$date_parser_choice[i] <- "BOTH_FAIL"; df$date_flag[i] <- TRUE
    if (i %% 1000 == 0 || i == n) setTxtProgressBar(pb, i); next
  }
  
  chosen_trip <- trip_combos[[best_idx]]
  ts <- chosen_trip$ts; te <- chosen_trip$te
  mixed <- best$mixed || (chosen_trip$lab != "DMY")
  
  # write trip + boosts chosen
  df$quote_create_time_date_clean[i] <- fmt(q)
  df$trip_start_date_clean[i]        <- fmt(ts)
  df$trip_end_date_clean[i]          <- fmt(te)
  if (length(best$clean)) for (nm in names(best$clean)) df[[nm]][i] <- best$clean[[nm]]
  
  df$date_parser_choice[i] <- if (mixed) "MIXED" else "DMY"
  df$date_flag[i] <- FALSE
  
  if (i %% 1000 == 0 || i == n) setTxtProgressBar(pb, i)
}
close(pb)

# -------- Summary & debugging view --------
count_failed <- sum(df$date_flag, na.rm = TRUE)
cat("\nTotal rows with date_flag = TRUE:", count_failed, "of", nrow(df), "\n")

fail_tbl <- df %>%
  filter(date_flag == TRUE) %>%
  select(
    quote_create_time, quote_create_time_date,
    trip_start_date, trip_end_date,
    starts_with("boost_"),
    ends_with("_clean"),
    date_parser_choice, date_flag
  )