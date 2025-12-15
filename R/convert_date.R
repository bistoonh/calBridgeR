convert_date <- function(dates, from, to) {
  suppressWarnings({
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
    
    from <- normalize_calendar(from)
    to   <- normalize_calendar(to)
    if (from == to) stop("'from' and 'to' calendars must be different.")
    
    # Normalize input dates (no calendar parameter needed)
    dates_norm <- normalize_date(dates)
    
    # Split into date and time components
    dt_split  <- strsplit(dates_norm, " ")
    date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
    time_only <- vapply(dt_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
    
    # Load mapping table
    data("calendar_map", package = "calBridgeR", envir = environment())
    
    # Use data.table if available
    if (requireNamespace("data.table", quietly = TRUE)) {
      dt_map <- data.table::data.table(
        jalali    = calendar_map$Shamsi,
        gregorian = calendar_map$Gregorian,
        hijri     = calendar_map$Hijri
      )
      
      idx <- match(date_only, dt_map[[from]])
      result_date <- dt_map[[to]][idx]
      
    } else {
      # Fallback: hash lookup
      lookup_env <- list2env(
        setNames(as.list(calendar_map[[to]]), calendar_map[[from]]),
        hash = TRUE, parent = emptyenv()
      )
      
      result_date <- vapply(date_only, function(x) {
        val <- lookup_env[[x]]
        if (is.null(val)) NA_character_ else val
      }, character(1))
    }
    
    # Attach time component if present
    result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
    
    return(result)
  })
}
