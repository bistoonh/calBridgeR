day2day <- function(start_date, end_date, calendar) {
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
    calendar <- normalize_calendar(calendar)

    has_dt <- requireNamespace("data.table", quietly = TRUE)

    # Normalize input dates
    start_norm <- normalize_date(start_date)
    end_norm   <- normalize_date(end_date)

    if (any(is.na(start_norm) | is.na(end_norm))) {
      stop("Invalid start or end date after normalization.")
    }

    # Fast split date/time at first space
    split_dt_fast <- function(x) {
      x <- as.character(x)
      sp <- regexpr(" ", x, fixed = TRUE)
      has_sp <- !is.na(x) & sp > 0L

      d <- x
      t <- rep(NA_character_, length(x))

      if (any(has_sp)) {
        d[has_sp] <- substr(x[has_sp], 1L, sp[has_sp] - 1L)
        tp <- trimws(substr(x[has_sp], sp[has_sp] + 1L, nchar(x[has_sp])))
        tp[tp == ""] <- NA_character_
        t[has_sp] <- tp
      }
      list(date = d, time = t)
    }

    s <- split_dt_fast(start_norm)
    e <- split_dt_fast(end_norm)

    start_date_only <- s$date
    start_time_only <- s$time
    end_date_only   <- e$date
    end_time_only   <- e$time

    # Load lookup table (avoid repeated data() cost)
    ns <- tryCatch(asNamespace("calBridgeR"), error = function(e) NULL)
    if (!is.null(ns) && exists("calendar_map", envir = ns, inherits = FALSE)) {
      calendar_map <- get("calendar_map", envir = ns, inherits = FALSE)
    } else {
      data("calendar_map", package = "calBridgeR", envir = environment())
      calendar_map <- get("calendar_map", envir = environment(), inherits = FALSE)
    }

    # Select appropriate date column
    date_vec <- switch(calendar,
      jalali    = as.character(calendar_map$Shamsi),
      gregorian = as.character(calendar_map$Gregorian),
      hijri     = as.character(calendar_map$Hijri)
    )

    # Find index positions (support scalar or vector start/end; original code assumes scalar-ish)
    # Here we keep the same semantics: take first element if vectors are provided.
    sd <- start_date_only[1L]
    ed <- end_date_only[1L]
    st <- start_time_only[1L]
    et <- end_time_only[1L]

    if (has_dt) {
      idx_start <- data.table::chmatch(sd, date_vec, nomatch = 0L)
      idx_end   <- data.table::chmatch(ed, date_vec, nomatch = 0L)
      if (idx_start == 0L) idx_start <- NA_integer_
      if (idx_end == 0L) idx_end <- NA_integer_
    } else {
      idx_start <- match(sd, date_vec)
      idx_end   <- match(ed, date_vec)
    }

    if (is.na(idx_start) || is.na(idx_end)) {
      stop("Start or end date not found in mapping table.")
    }

    # Generate date sequence
    if (idx_start <= idx_end) {
      result_dates <- date_vec[idx_start:idx_end]
    } else {
      result_dates <- rev(date_vec[idx_end:idx_start])
    }

    # Attach time components if present (only at endpoints)
    if (!is.na(st) || !is.na(et)) {
      result_times <- rep(NA_character_, length(result_dates))
      result_times[1L] <- st
      result_times[length(result_times)] <- et
      result <- ifelse(!is.na(result_times), paste(result_dates, result_times), result_dates)
    } else {
      result <- result_dates
    }

    result
  })
}
