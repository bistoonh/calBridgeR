date_diff <- function(start_date, end_date, calendar) {
  # Optional: enforce equal lengths to avoid recycling
  if (length(start_date) != length(end_date)) {
    stop("Lengths of start_date and end_date must be equal.")
  }
  
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
  
  # Vectorized safe normalization
  safe_norm_vec <- function(x) {
    out <- x
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) {
      out[ok] <- suppressWarnings(normalize_date(x[ok], calendar = calendar))
    }
    out[!ok] <- NA_character_
    out
  }
  start_norm <- safe_norm_vec(start_date)
  end_norm   <- safe_norm_vec(end_date)
  
  # Load lookup table
  data("calendar_map", package = "calBridgeR", envir = environment())
  
  # انتخاب ستون مناسب
  if (calendar == "jalali") {
    date_vec <- calendar_map$Shamsi
  } else if (calendar == "gregorian") {
    date_vec <- calendar_map$Gregorian
  } else if (calendar == "hijri") {
    date_vec <- calendar_map$Hijri
  }
  
  # Fast matching
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt_map <- data.table::data.table(
      date = date_vec,
      idx = seq_len(length(date_vec))
    )
    start_idx <- dt_map$idx[match(start_norm, dt_map$date)]
    end_idx   <- dt_map$idx[match(end_norm,   dt_map$date)]
  } else {
    start_idx <- match(start_norm, date_vec)
    end_idx   <- match(end_norm,   date_vec)
  }
  
  # Differences; NA propagates
  as.integer(end_idx - start_idx)
}
