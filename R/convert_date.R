convert_date <- function(dates, from, to) {
  suppressWarnings({

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

    has_dt <- requireNamespace("data.table", quietly = TRUE)

    # Normalize input dates
    dates_norm <- normalize_date(dates)

    n <- length(dates_norm)

    # Split into date and time (fast: split at first space)
    sp <- regexpr(" ", dates_norm, fixed = TRUE)
    has_sp <- !is.na(dates_norm) & sp > 0L

    date_only <- dates_norm
    time_only <- rep(NA_character_, n)

    if (any(has_sp)) {
      date_only[has_sp] <- substr(dates_norm[has_sp], 1L, sp[has_sp] - 1L)
      tp <- trimws(substr(dates_norm[has_sp], sp[has_sp] + 1L, nchar(dates_norm[has_sp])))
      tp[tp == ""] <- NA_character_
      time_only[has_sp] <- tp
    }

    # Load mapping table (avoid repeated data() cost)
    ns <- tryCatch(asNamespace("calBridgeR"), error = function(e) NULL)
    if (!is.null(ns) && exists("calendar_map", envir = ns, inherits = FALSE)) {
      calendar_map <- get("calendar_map", envir = ns, inherits = FALSE)
    } else {
      data("calendar_map", package = "calBridgeR", envir = environment())
      calendar_map <- get("calendar_map", envir = environment(), inherits = FALSE)
    }

    # Map from normalized names to columns
    from_vec <- switch(from,
      jalali    = as.character(calendar_map$Shamsi),
      gregorian = as.character(calendar_map$Gregorian),
      hijri     = as.character(calendar_map$Hijri)
    )
    to_vec <- switch(to,
      jalali    = as.character(calendar_map$Shamsi),
      gregorian = as.character(calendar_map$Gregorian),
      hijri     = as.character(calendar_map$Hijri)
    )

    # Fast lookup
    if (has_dt) {
      idx <- data.table::chmatch(date_only, from_vec, nomatch = 0L)
      result_date <- rep(NA_character_, n)
      hit <- idx > 0L
      if (any(hit)) result_date[hit] <- to_vec[idx[hit]]
    } else {
      idx <- match(date_only, from_vec)
      result_date <- to_vec[idx]  # idx NA -> NA (desired)
    }

    # Attach time if present (and date is not NA)
    result <- ifelse(is.na(time_only) | time_only == "" | is.na(result_date),
                     result_date,
                     paste(result_date, time_only))

    result
  })
}
