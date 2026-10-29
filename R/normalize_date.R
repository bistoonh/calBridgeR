# Internal package cache for calendar validation vector to avoid redundant reconstructions
.calbridge_cache <- new.env(parent = emptyenv())

#' Retrieve and cache valid calendar dates across Jalali, Gregorian, and Hijri calendars
#' @keywords internal
.get_valid_dates <- function() {
  if (exists("valid_vec", envir = .calbridge_cache, inherits = FALSE)) {
    return(get("valid_vec", envir = .calbridge_cache, inherits = FALSE))
  }

  ns <- tryCatch(asNamespace("calBridgeR"), error = function(e) NULL)
  if (!is.null(ns) && exists("calendar_map", envir = ns, inherits = FALSE)) {
    calendar_map <- get("calendar_map", envir = ns, inherits = FALSE)
  } else {
    data("calendar_map", package = "calBridgeR", envir = environment())
    calendar_map <- get("calendar_map", envir = environment(), inherits = FALSE)
  }

  valid_vec <- unique(c(
    as.character(calendar_map$Shamsi),
    as.character(calendar_map$Gregorian),
    as.character(calendar_map$Hijri)
  ))

  assign("valid_vec", valid_vec, envir = .calbridge_cache)
  valid_vec
}

#' Normalize Date and Datetime Strings
#'
#' Normalizes input date strings into standard "YYYY-MM-DD" format, optionally
#' preserving and standardizing time components to "HH:MM:SS".
#'
#' @param dates Character vector of date or datetime strings.
#' @return A character vector of normalized dates/datetimes.
#' @export
normalize_date <- function(dates) {
  n <- length(dates)
  if (n == 0L) return(character(0L))

  has_dt <- requireNamespace("data.table", quietly = TRUE)

  # Pre-clean string characters
  x <- as.character(dates)
  x <- trimws(x)
  x <- chartr("/.", "--", x) # Significantly faster than gsub

  # 1. Split date and time components by the first space
  sp <- regexpr(" ", x, fixed = TRUE)
  has_sp <- !is.na(x) & sp > 0L

  date_part <- x
  time_part <- rep(NA_character_, n)

  if (any(has_sp)) {
    date_part[has_sp] <- substr(x[has_sp], 1L, sp[has_sp] - 1L)
    tp <- trimws(substr(x[has_sp], sp[has_sp] + 1L, nchar(x[has_sp])))
    tp[tp == ""] <- NA_character_
    time_part[has_sp] <- tp
  }

  # =========================================================================
  # 2. Date Normalization using Unique-Value Optimization
  # =========================================================================
  has_date <- !is.na(date_part) & date_part != ""
  date_norm <- rep(NA_character_, n)

  if (any(has_date)) {
    # Extract only unique raw dates to avoid running regex on millions of rows
    u_raw <- unique(date_part[has_date])
    u_norm <- u_raw

    # 8-digit format: YYYYMMDD -> YYYY-MM-DD
    idx8 <- grepl("^\\d{8}$", u_raw)
    if (any(idx8)) {
      p <- u_raw[idx8]
      u_norm[idx8] <- paste0(substr(p, 1L, 4L), "-", substr(p, 5L, 6L), "-", substr(p, 7L, 8L))
    }

    # 6-digit format: YYYYMM -> YYYY-MM-01
    idx6 <- !idx8 & grepl("^\\d{6}$", u_raw)
    if (any(idx6)) {
      p <- u_raw[idx6]
      u_norm[idx6] <- paste0(substr(p, 1L, 4L), "-", substr(p, 5L, 6L), "-01")
    }

    # Non-padded dates (e.g., 1402-1-5 or 1402-1 -> 1402-01-05)
    idx_pad <- !idx8 & !idx6 &
      !grepl("^\\d{4}-\\d{2}-\\d{2}$", u_norm) &
      grepl("^\\d{4}-\\d{1,2}(-\\d{1,2})?$", u_norm)

    if (any(idx_pad)) {
      tmp <- sub("^([0-9]{4}-[0-9]{1,2})$", "\\1-01", u_norm[idx_pad], perl = TRUE)
      parts <- strsplit(tmp, "-", fixed = TRUE)

      y <- vapply(parts, `[`, character(1L), 1L)
      m <- sprintf("%02d", suppressWarnings(as.integer(vapply(parts, `[`, character(1L), 2L))))
      d <- sprintf("%02d", suppressWarnings(as.integer(vapply(parts, `[`, character(1L), 3L))))

      u_norm[idx_pad] <- paste0(y, "-", m, "-", d)
    }

    # Validate against pre-cached calendar dates
    valid_vec <- .get_valid_dates()
    if (has_dt) {
      valid_idx <- data.table::`%chin%`(u_norm, valid_vec)
    } else {
      valid_idx <- u_norm %in% valid_vec
    }
    u_norm[!valid_idx] <- NA_character_

    # Map back normalized values to the full vector using fast indexing
    if (has_dt) {
      map_back_d <- data.table::chmatch(date_part[has_date], u_raw)
    } else {
      map_back_d <- match(date_part[has_date], u_raw)
    }
    date_norm[has_date] <- u_norm[map_back_d]
  }

  # =========================================================================
  # 3. Time Normalization (Robust Parsing with Seconds Defaulting)
  # =========================================================================
  time_norm <- rep("", n)
  idx_time <- !is.na(time_part) & !is.na(date_norm)

  if (any(idx_time)) {
    raw_times <- time_part[idx_time]
    u_time_raw <- unique(raw_times)

    # Safely split by ':' regardless of missing seconds (e.g., '22:10' -> '22:10:00')
    spl <- strsplit(u_time_raw, ":", fixed = TRUE)

    h_chr <- vapply(spl, function(v) if (length(v) >= 1L) v[1L] else NA_character_, character(1L))
    m_chr <- vapply(spl, function(v) if (length(v) >= 2L) v[2L] else "0", character(1L))
    s_chr <- vapply(spl, function(v) if (length(v) >= 3L) v[3L] else "0", character(1L))

    h <- suppressWarnings(as.integer(h_chr))
    m <- suppressWarnings(as.integer(m_chr))
    s <- suppressWarnings(as.integer(s_chr))

    m[is.na(m)] <- 0L
    s[is.na(s)] <- 0L

    invalid <- is.na(h) | h < 0L | h > 23L | m < 0L | m > 59L | s < 0L | s > 59L

    u_time_norm <- rep(NA_character_, length(u_time_raw))
    good <- !invalid
    if (any(good)) {
      u_time_norm[good] <- sprintf("%02d:%02d:%02d", h[good], m[good], s[good])
    }

    # Map normalized unique time values back to the rows
    if (has_dt) {
      map_back_t <- data.table::chmatch(raw_times, u_time_raw)
    } else {
      map_back_t <- match(raw_times, u_time_raw)
    }
    time_norm[idx_time] <- u_time_norm[map_back_t]
  }

  # =========================================================================
  # 4. Final Combination
  # =========================================================================
  res <- date_norm

  # Invalidate entire datetime if time was provided but invalid
  time_is_invalid <- idx_time & is.na(time_norm)
  res[time_is_invalid] <- NA_character_

  # Re-attach standardized time component
  has_valid_time <- !is.na(res) & time_norm != "" & !is.na(time_norm)
  if (any(has_valid_time)) {
    res[has_valid_time] <- paste(res[has_valid_time], time_norm[has_valid_time])
  }

  res
}
