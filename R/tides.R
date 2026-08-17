#' Summarize NOAA tide predictions for field work planning
#'
#' Reads a NOAA High/Low tide prediction file and prints a daily summary
#' showing the primary daytime tides and a field-work quality rating based
#' on how close the low tide falls to noon.
#' 
#' Get tide predictions from https://tidesandcurrents.noaa.gov/tide_predictions.html.
#' I'm using station ESSEX, ESSEX RIVER 8441771
#'
#' @param path Path to a NOAA High/Low tide prediction text file.
#' @param daylight_start Earliest hour to consider, as decimal hours (default 6 = 6 AM).
#' @param daylight_end Latest hour to consider, as decimal hours (default 20 = 8 PM).
#' @param cutoffs Named numeric vector of hours-from-noon thresholds for
#'   \code{"excellent"}, \code{"good"}, \code{"fair"}, \code{"poor"}.
#'   Days with no daytime low, or with a low farther than \code{"poor"},
#'   are rated \code{"no go"}.
#' @return A data frame with one row per day, invisibly. Prints a formatted
#'   summary as a side effect.
#' @export



tide_summary <- function(path,
                         daylight_start = 6,
                         daylight_end   = 20,
                         cutoffs = c(excellent = 2, good = 4, fair = 5, poor = 6)) {
  df <- parse_noaa_tides(path)

  df_day <- df[df$hour_dec >= daylight_start & df$hour_dec <= daylight_end, ]

  dates <- unique(df$date)
  rows <- lapply(dates, function(d) {
    day_tides <- df_day[df_day$date == d, ]
    lows      <- day_tides[day_tides$hl == "L", ]
    highs     <- day_tides[day_tides$hl == "H", ]

    # Best low: daytime low closest to noon
    best_low <- if (nrow(lows) > 0) lows[which.min(abs(lows$hour_dec - 12)), ] else NULL

    # Paired high: next H after the best low, or previous H if none follows
    best_high <- NULL
    if (!is.null(best_low) && nrow(highs) > 0) {
      after  <- highs[highs$hour_dec > best_low$hour_dec, ]
      before <- highs[highs$hour_dec < best_low$hour_dec, ]
      best_high <- if (nrow(after)  > 0) after[1, ]              else
                   if (nrow(before) > 0) before[nrow(before), ]  else NULL
    } else if (nrow(highs) > 0) {
      best_high <- highs[which.min(abs(highs$hour_dec - 12)), ]
    }

    hrs_from_noon <- if (!is.null(best_low)) abs(best_low$hour_dec - 12) else Inf
    rating        <- rate_tide(hrs_from_noon, cutoffs)

    data.frame(
      date          = d,
      day_str       = df[df$date == d, "day_str"][1],
      low_time      = if (!is.null(best_low))  best_low$time_str  else NA_character_,
      high_time     = if (!is.null(best_high)) best_high$time_str else NA_character_,
      high_level    = if (!is.null(best_high)) best_high$height   else NA_real_,
      hrs_from_noon = hrs_from_noon,
      rating        = rating,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)

  for (i in seq_len(nrow(result))) {
    r          <- result[i, ]
    date_label  <- format(r$date, "%a %b %d")
    low_label   <- fmt_tide("L", r$low_time)
    high_label  <- fmt_tide("H", r$high_time)
    level_label <- if (!is.na(r$high_level)) sprintf("+%.1f'", r$high_level) else "    "
    cat(sprintf("%-10s %s, %s %s %s\n",
                date_label, low_label, high_label, level_label, r$rating))
  }

  invisible(result)
}

#' Parse a NOAA High/Low tide prediction file
#'
#' @param path Path to the file.
#' @return A data frame with columns: date_str, day_str, time_str, height,
#'   hl, date, hour_dec.
#' @export
parse_noaa_tides <- function(path) {
  raw   <- readLines(path, warn = FALSE)
  start <- grep("^Date", raw)
  if (length(start) == 0) stop("No data header found in file: ", path)

  lines <- raw[(start + 1):length(raw)]
  lines <- lines[nzchar(trimws(lines))]

  df <- read.table(
    text         = paste(lines, collapse = "\n"),
    sep          = "\t",
    header       = FALSE,
    col.names    = c("date_str", "day_str", "time_str", "height", "hl"),
    stringsAsFactors = FALSE,
    strip.white  = TRUE
  )

  dt          <- as.POSIXct(paste(df$date_str, df$time_str),
                             format = "%Y/%m/%d %I:%M %p")
  df$date     <- as.Date(df$date_str, format = "%Y/%m/%d")
  df$hour_dec <- as.numeric(format(dt, "%H")) + as.numeric(format(dt, "%M")) / 60
  df$height   <- as.numeric(df$height)
  df
}

# Assign a rating based on hours-from-noon thresholds
rate_tide <- function(hrs, cutoffs) {
  if      (hrs <= cutoffs["excellent"]) "excellent"
  else if (hrs <= cutoffs["good"])      "good"
  else if (hrs <= cutoffs["fair"])      "fair"
  else if (hrs <= cutoffs["poor"])      "poor"
  else                                  "no go"
}

# Format a tide label: "L  8:16 am", "H 12:42 pm"
fmt_tide <- function(hl, time_str) {
  if (is.na(time_str)) return(sprintf("%s       --", hl))
  m <- regmatches(time_str, regexec("^(\\d+):(\\d+) (AM|PM)$", time_str))[[1]]
  if (length(m) < 4) return(paste(hl, time_str))
  sprintf("%s %2d:%s %s", hl, as.integer(m[2]), m[3], tolower(m[4]))
}
