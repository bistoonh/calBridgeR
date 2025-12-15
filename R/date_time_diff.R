# Date-time difference function (uses normalize_date without calendar parameter)
date_time_diff <- function(start_date, end_date, calendar) {
  if (length(start_date) != length(end_date)) {
    stop("Lengths of start_date and end_date must be equal.")
  }
  
  # Map abbreviations to full calendar names
  normalize_calendar <- function(x) {
    x <- tolower(x)
    if (x %in% c("j", "jalali")) {
      "jalali"
    } else if (x %in% c("g", "gregorian")) {
      "gregorian"
    } else if (x %in% c("h", "hijri")) {
      "hijri"
    } else {
      stop("Unsupported calendar type. Use 'jalali'/'j', 'gregorian'/'g', or 'hijri'/'h'.")
    }
  }
  calendar <- normalize_calendar(calendar)
  
  # Safe normalization
  safe_norm_vec <- function(x) {
    out <- x
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) {
      out[ok] <- suppressWarnings(normalize_date(x[ok]))
    }
    out[!ok] <- NA_character_
    out
  }
  start_norm <- safe_norm_vec(start_date)
  end_norm   <- safe_norm_vec(end_date)
  
  # Split into date and time parts
  split_start <- strsplit(start_norm, " ")
  split_end   <- strsplit(end_norm, " ")
  
  start_date_only <- vapply(split_start, `[`, 1, FUN.VALUE = character(1))
  end_date_only   <- vapply(split_end,   `[`, 1, FUN.VALUE = character(1))
  
  start_time_only <- vapply(split_start, function(x) if (length(x) > 1) x[2] else "00:00:00", FUN.VALUE = character(1))
  end_time_only   <- vapply(split_end,   function(x) if (length(x) > 1) x[2] else "00:00:00", FUN.VALUE = character(1))
  
  # Load lookup table
  data("calendar_map", package = "calBridgeR", envir = environment())
  
  # Select appropriate column
  if (calendar == "jalali") {
    date_vec <- calendar_map$Shamsi
  } else if (calendar == "gregorian") {
    date_vec <- calendar_map$Gregorian
  } else if (calendar == "hijri") {
    date_vec <- calendar_map$Hijri
  }
  
  # Fast matching: day index
  start_idx <- match(start_date_only, date_vec)
  end_idx   <- match(end_date_only,   date_vec)
  
  # Day difference in seconds
  day_diff_sec <- (end_idx - start_idx) * 86400L
  
  # Parse times into seconds-of-day
  time_to_sec <- function(t) {
    parts <- do.call(rbind, strsplit(t, ":"))
    h <- as.integer(parts[,1]); m <- as.integer(parts[,2]); s <- as.integer(parts[,3])
    h[is.na(h)] <- 0; m[is.na(m)] <- 0; s[is.na(s)] <- 0
    h*3600 + m*60 + s
  }
  start_sec <- time_to_sec(start_time_only)
  end_sec   <- time_to_sec(end_time_only)
  
  # Total difference in seconds
  diff_sec <- day_diff_sec + (end_sec - start_sec)
  
  # Formatted output
  format_diff <- function(sec) {
    days <- sec %/% 86400
    hours <- (sec %% 86400) %/% 3600
    mins <- (sec %% 3600) %/% 60
    secs <- sec %% 60
    sprintf("%d days %02d:%02d:%02d", days, hours, mins, secs)
  }
  diff_fmt <- vapply(diff_sec, format_diff, FUN.VALUE = character(1))
  
  list(
    seconds = diff_sec,
    formatted = diff_fmt
  )
}
