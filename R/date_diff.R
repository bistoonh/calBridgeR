date_diff <- function(start_date, end_date, calendar) {
  # Enforce equal lengths to avoid recycling
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

  # Vectorized safe normalization
  safe_norm_vec <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) out[ok] <- suppressWarnings(normalize_date(x[ok]))
    out
  }

  start_norm <- safe_norm_vec(start_date)
  end_norm   <- safe_norm_vec(end_date)

  # Load mapping table (avoid repeated data() cost)
  ns <- tryCatch(asNamespace("calBridgeR"), error = function(e) NULL)
  if (!is.null(ns) && exists("calendar_map", envir = ns, inherits = FALSE)) {
    calendar_map <- get("calendar_map", envir = ns, inherits = FALSE)
  } else {
    data("calendar_map", package = "calBridgeR", envir = environment())
    calendar_map <- get("calendar_map", envir = environment(), inherits = FALSE)
  }

  # Select appropriate date vector
  date_vec <- switch(calendar,
    jalali    = as.character(calendar_map$Shamsi),
    gregorian = as.character(calendar_map$Gregorian),
    hijri     = as.character(calendar_map$Hijri)
  )

  # Match to 1..N indices; keep NA when unmatched
  if (has_dt) {
    # chmatch returns 0 when nomatch=0L
    start_idx <- data.table::chmatch(start_norm, date_vec, nomatch = 0L)
    end_idx   <- data.table::chmatch(end_norm,   date_vec, nomatch = 0L)

    start_idx[start_idx == 0L] <- NA_integer_
    end_idx[end_idx == 0L] <- NA_integer_
  } else {
    start_idx <- match(start_norm, date_vec)
    end_idx   <- match(end_norm,   date_vec)
  }

  # Differences; NA propagates
  as.integer(end_idx - start_idx)
}
