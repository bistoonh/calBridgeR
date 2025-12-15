day2day <- function(start_date, end_date, calendar) {
  suppressWarnings({
    # نگاشت مخفف‌ها به اسم کامل
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
    
    # Normalize input dates
    start_norm <- normalize_date(start_date, calendar = calendar)
    end_norm   <- normalize_date(end_date, calendar = calendar)
    
    if (any(is.na(start_norm) | is.na(end_norm))) {
      stop("Invalid start or end date after normalization.")
    }
    
    # Split into date and time components
    start_split <- strsplit(start_norm, " ")
    end_split   <- strsplit(end_norm, " ")
    
    start_date_only <- vapply(start_split, `[`, 1, FUN.VALUE = character(1))
    start_time_only <- vapply(start_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
    
    end_date_only <- vapply(end_split, `[`, 1, FUN.VALUE = character(1))
    end_time_only <- vapply(end_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
    
    # Load lookup table
    data("calendar_map", package = "calBridgeR")
    
    # Select appropriate date column
    if (calendar == "jalali") {
      date_vec <- calendar_map$Shamsi
    } else if (calendar == "gregorian") {
      date_vec <- calendar_map$Gregorian
    } else if (calendar == "hijri") {
      date_vec <- calendar_map$Hijri
    }
    
    # Find index positions
    idx_start <- match(start_date_only, date_vec)
    idx_end   <- match(end_date_only, date_vec)
    
    if (is.na(idx_start) | is.na(idx_end)) {
      stop("Start or end date not found in map.")
    }
    
    # Generate date sequence
    if (idx_start <= idx_end) {
      result_dates <- date_vec[idx_start:idx_end]
    } else {
      result_dates <- rev(date_vec[idx_end:idx_start])
    }
    
    # Attach time components if present
    if (!all(is.na(start_time_only)) | !all(is.na(end_time_only))) {
      result_times <- rep(NA_character_, length(result_dates))
      result_times[1] <- start_time_only
      result_times[length(result_times)] <- end_time_only
      result <- ifelse(!is.na(result_times), paste(result_dates, result_times), result_dates)
    } else {
      result <- result_dates
    }
    
    return(result)
  })
}
