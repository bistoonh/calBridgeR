normalize_date <- function(dates) {
  has_dt <- requireNamespace("data.table", quietly = TRUE)

  # گرفتن calendar_map بدون load تکراری از دیسک (اگر داخل namespace موجود است)
  ns <- tryCatch(asNamespace("calBridgeR"), error = function(e) NULL)
  if (!is.null(ns) && exists("calendar_map", envir = ns, inherits = FALSE)) {
    calendar_map <- get("calendar_map", envir = ns, inherits = FALSE)
  } else {
    data("calendar_map", package = "calBridgeR", envir = environment())
    calendar_map <- get("calendar_map", envir = environment(), inherits = FALSE)
  }

  valid_vec <- c(
    as.character(calendar_map$Shamsi),
    as.character(calendar_map$Gregorian),
    as.character(calendar_map$Hijri)
  )

  x <- as.character(dates)
  x <- trimws(x)
  x <- gsub("[/\\.]", "-", x, perl = TRUE)

  n <- length(x)

  ## split date/time (با اولین space)
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

  ## normalize date
  date_norm <- date_part

  idx8 <- !is.na(date_part) & grepl("^\\d{8}$", date_part)
  if (any(idx8)) {
    p <- date_part[idx8]
    date_norm[idx8] <- paste0(substr(p, 1, 4), "-", substr(p, 5, 6), "-", substr(p, 7, 8))
  }

  idx6 <- !is.na(date_part) & !idx8 & grepl("^\\d{6}$", date_part)
  if (any(idx6)) {
    p <- date_part[idx6]
    date_norm[idx6] <- paste0(substr(p, 1, 4), "-", substr(p, 5, 6), "-01")
  }

  idx_pad <- !is.na(date_norm) &
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_norm) &
    grepl("^\\d{4}-\\d{1,2}(-\\d{1,2})?$", date_norm)

  if (any(idx_pad)) {
    tmp <- sub("^([0-9]{4}-[0-9]{1,2})$", "\\1-01", date_norm[idx_pad], perl = TRUE)

    if (has_dt) {
      parts <- data.table::tstrsplit(tmp, "-", fixed = TRUE, keep = 1:3)
      y <- parts[[1]]
      m <- sprintf("%02d", suppressWarnings(as.integer(parts[[2]])))
      d <- sprintf("%02d", suppressWarnings(as.integer(parts[[3]])))
    } else {
      parts <- strsplit(tmp, "-", fixed = TRUE)
      mat <- do.call(rbind, parts)
      y <- mat[, 1]
      m <- sprintf("%02d", suppressWarnings(as.integer(mat[, 2])))
      d <- sprintf("%02d", suppressWarnings(as.integer(mat[, 3])))
    }

    date_norm[idx_pad] <- paste0(y, "-", m, "-", d)
  }

  ## validate date across all calendars
  valid_idx <- if (has_dt) data.table::`%chin%`(date_norm, valid_vec) else (date_norm %in% valid_vec)

  valid_idx[is.na(date_norm)] <- FALSE
  date_norm[!valid_idx] <- NA_character_

  ## normalize time (فقط اگر date معتبر است)
  time_norm <- rep("", n)
  idx_time <- !is.na(time_part) & !is.na(date_norm)

  if (any(idx_time)) {
    t <- trimws(time_part[idx_time])

    if (has_dt) {
      tt <- data.table::tstrsplit(t, ":", fixed = TRUE, keep = 1:3, fill = NA_character_)
      h <- suppressWarnings(as.integer(tt[[1]]))
      m <- suppressWarnings(as.integer(tt[[2]]))
      s <- suppressWarnings(as.integer(tt[[3]]))
    } else {
      # fallback ساده (کندتر از data.table اما فقط وقتی dt نیست)
      spl <- strsplit(t, ":", fixed = TRUE)
      len <- lengths(spl)
      geti <- function(i) vapply(spl, function(v) if (length(v) >= i) v[[i]] else NA_character_, "")
      h <- suppressWarnings(as.integer(geti(1L)))
      m <- suppressWarnings(as.integer(geti(2L)))
      s <- suppressWarnings(as.integer(geti(3L)))
    }

    m[is.na(m)] <- 0L
    s[is.na(s)] <- 0L

    invalid <- is.na(h) | h < 0L | h > 23L | m < 0L | m > 59L | s < 0L | s > 59L

    time_norm[idx_time] <- if (has_dt) {
      data.table::fifelse(invalid, NA_character_, sprintf("%02d:%02d:%02d", h, m, s))
    } else {
      ifelse(invalid, NA_character_, sprintf("%02d:%02d:%02d", h, m, s))
    }
  }

  ## combine
  if (has_dt) {
    data.table::fifelse(time_norm == "", date_norm, paste(date_norm, time_norm))
  } else {
    ifelse(time_norm == "", date_norm, paste(date_norm, time_norm))
  }
}

