# تعریف بیرون از تابع برای جلوگیری از بازتعریف مکرر در هر Call
.normalize_calendar <- function(x) {
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

convert_date <- function(dates, from, to) {
  from <- .normalize_calendar(from)
  to   <- .normalize_calendar(to)
  if (from == to) stop("'from' and 'to' calendars must be different.")

  n <- length(dates)
  if (n == 0L) return(character(0))

  # 1. Normalize input dates
  dates_norm <- normalize_date(dates)

  # 2. Split date and time (فقط روی رکوردهایی که فاصله دارند)
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

  # 3. استراتژی Unique: پیدا کردن مقادیر مجزا
  # u_dates: تاریخ‌های یکتا (بدون NA)
  # map_back: ایندکس معکوس برای بازگرداندن به ابعاد بردار اصلی
  has_date <- !is.na(date_only)
  u_dates  <- unique(date_only[has_date])

  # 4. لود بهینه جدول نگاشت
  ns <- tryCatch(asNamespace("calBridgeR"), error = function(e) NULL)
  if (!is.null(ns) && exists("calendar_map", envir = ns, inherits = FALSE)) {
    calendar_map <- get("calendar_map", envir = ns, inherits = FALSE)
  } else {
    data("calendar_map", package = "calBridgeR", envir = environment())
    calendar_map <- get("calendar_map", envir = environment(), inherits = FALSE)
  }

  from_vec <- switch(from,
    jalali    = calendar_map$Shamsi,
    gregorian = calendar_map$Gregorian,
    hijri     = calendar_map$Hijri
  )
  to_vec <- switch(to,
    jalali    = calendar_map$Shamsi,
    gregorian = calendar_map$Gregorian,
    hijri     = calendar_map$Hijri
  )

  # 5. تبدیل فقط مقادیر یکتا (Unique Lookup)
  has_dt <- requireNamespace("data.table", quietly = TRUE)
  
  if (has_dt) {
    u_idx <- data.table::chmatch(u_dates, from_vec, nomatch = NA_integer_)
  } else {
    u_idx <- match(u_dates, from_vec)
  }
  
  # مقادیر تبدیل‌شده برای تاریخ‌های یکتا
  u_converted <- to_vec[u_idx]

  # 6. بازنشانی سریع روی کل بردار با Map Back
  result_date <- rep(NA_character_, n)
  if (length(u_dates) > 0L) {
    if (has_dt) {
      map_back <- data.table::chmatch(date_only[has_date], u_dates)
    } else {
      map_back <- match(date_only[has_date], u_dates)
    }
    result_date[has_date] <- u_converted[map_back]
  }

  # 7. الحاق زمان (جایگزین بهینه برای ifelse)
  has_time_to_attach <- !is.na(time_only) & !is.na(result_date)
  if (any(has_time_to_attach)) {
    result_date[has_time_to_attach] <- paste(
      result_date[has_time_to_attach], 
      time_only[has_time_to_attach]
    )
  }

  result_date
}
