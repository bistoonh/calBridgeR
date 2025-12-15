date_time_diff <- function(start_date, end_date, calendar) {
  if (length(start_date) != length(end_date)) {
    stop("Lengths of start_date and end_date must be equal.")
  }

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

  # Safe normalization (vectorized)
  safe_norm_vec <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) out[ok] <- suppressWarnings(normalize_date(x[ok]))
    out
  }

  start_norm <- safe_norm_vec(start_date)
  end_norm   <- safe_norm_vec(end_date)

  n <- length(start_norm)

  # Split date and time fast (at first space)
  split_dt_fast <- function(x) {
    sp <- regexpr(" ", x, fixed = TRUE)
    has_sp <- !is.na(x) & sp > 0L

    d <- x
    t <- rep("00:00:00", length(x))  # default when no time part (matches original intent)

    if (any(has_sp)) {
      d[has_sp] <- substr(x[has_sp], 1L, sp[has_sp] - 1L)
      tp <- trimws(substr(x[has_sp], sp[has_sp] + 1L, nchar(x[has_sp])))
      tp[tp == ""] <- "00:00:00"
      t[has_sp] <- tp
    }
    list(date = d, time = t)
  }

  s <- split_dt_fast(start_norm)
  e <- split_dt_fast(end_norm)

  start_date_only <- s$date
  end_date_only   <- e$date
  start_time_only <- s$time
  end_time_only   <- e$time

  # Load calendar_map efficiently
  ns <- tryCatch(asNamespace("calBridgeR"), error = function(e) NULL)
  if (!is.null(ns) && exists("calendar_map", envir = ns, inherits = FALSE)) {
    calendar_map <- get("calendar_map", envir = ns, inherits = FALSE)
  } else {
    data("calendar_map", package = "calBridgeR", envir = environment())
    calendar_map <- get("calendar_map", envir = environment(), inherits = FALSE)
  }

  date_vec <- switch(calendar,
    jalali    = as.character(calendar_map$Shamsi),
    gregorian = as.character(calendar_map$Gregorian),
    hijri     = as.character(calendar_map$Hijri)
  )

  # Day index lookup
  if (has_dt) {
    start_idx <- data.table::chmatch(start_date_only, date_vec, nomatch = 0L)
    end_idx   <- data.table::chmatch(end_date_only,   date_vec, nomatch = 0L)
    start_idx[start_idx == 0L] <- NA_integer_
    end_idx[end_idx == 0L] <- NA_integer_
  } else {
    start_idx <- match(start_date_only, date_vec)
    end_idx   <- match(end_date_only,   date_vec)
  }

  # Day difference in seconds (use numeric to avoid int overflow)
  day_diff_sec <- (end_idx - start_idx) * 86400

  # Time -> seconds-of-day (fast, no strsplit)
  time_to_sec_fast <- function(t) {
    t <- as.character(t)
    h <- suppressWarnings(as.integer(substr(t, 1L, 2L)))
    m <- suppressWarnings(as.integer(substr(t, 4L, 5L)))
    s <- suppressWarnings(as.integer(substr(t, 7L, 8L)))

    h[is.na(h)] <- 0L
    m[is.na(m)] <- 0L
    s[is.na(s)] <- 0L

    h * 3600 + m * 60 + s
  }

  start_sec <- time_to_sec_fast(start_time_only)
  end_sec   <- time_to_sec_fast(end_time_only)

  diff_sec <- day_diff_sec + (end_sec - start_sec)

  # Formatted output (NA -> NA_character_)
  formatted <- rep(NA_character_, n)
  ok <- !is.na(diff_sec)
  if (any(ok)) {
    days  <- diff_sec[ok] %/% 86400
    hours <- (diff_sec[ok] %% 86400) %/% 3600
    mins  <- (diff_sec[ok] %% 3600) %/% 60
    secs  <- diff_sec[ok] %% 60

    formatted[ok] <- sprintf(
      "%d days %02d:%02d:%02d",
      as.integer(days), as.integer(hours), as.integer(mins), as.integer(secs)
    )
  }

  list(
    seconds = diff_sec,
    formatted = formatted
  )
}
