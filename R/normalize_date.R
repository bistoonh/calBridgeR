normalize_date <- function(dates) {
  suppressWarnings({
    # Load mapping table from package data
    data("calendar_map", package = "calBridgeR")
    
    # Prepare input data
    dates <- trimws(as.character(dates))
    dates <- gsub("[/\\.]", "-", dates)
    
    # Split time part if present
    time_part <- sub("^[^ ]*(?: (.*))?$", "\\1", dates)
    time_part[time_part == ""] <- NA_character_
    date_part <- sub(" .*", "", dates)
    
    # Normalize date formats
    date_norm <- ifelse(grepl("^\\d{8}$", date_part),
                        sprintf("%04d-%02d-%02d",
                                as.integer(substr(date_part,1,4)),
                                as.integer(substr(date_part,5,6)),
                                as.integer(substr(date_part,7,8))),
                        ifelse(grepl("^\\d{6}$", date_part),
                               sprintf("%04d-%02d-01",
                                       as.integer(substr(date_part,1,4)),
                                       as.integer(substr(date_part,5,6))),
                               date_part))
    
    # Normalize yyyy-mm → yyyy-mm-01 and pad zeros
    idx <- !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_norm) & grepl("^\\d{4}-\\d{1,2}(-\\d{1,2})?$", date_norm)
    if (any(idx)) {
      date_norm[idx] <- sub("^([0-9]{4}-[0-9]{1,2})$", "\\1-01", date_norm[idx])
      parts <- do.call(rbind, strsplit(date_norm[idx], "-"))
      y <- parts[,1]
      m <- sprintf("%02d", as.integer(parts[,2]))
      d <- sprintf("%02d", as.integer(parts[,3]))
      date_norm[idx] <- paste0(y,"-",m,"-",d)
    }
    
    date_norm <- as.character(date_norm)
    
    # Validate dates using lookup table (across all calendars)
    valid_vec <- c(as.character(calendar_map$Shamsi),
                   as.character(calendar_map$Gregorian),
                   as.character(calendar_map$Hijri))
    
    if (requireNamespace("data.table", quietly = TRUE)) {
      dt <- data.table::data.table(valid = valid_vec)
      valid_idx <- date_norm %in% dt$valid
    } else {
      valid_idx <- date_norm %in% valid_vec
    }
    date_norm[!valid_idx] <- NA_character_
    
    # Process time part
    time_norm <- rep("", length(time_part))
    idx_time <- !is.na(time_part) & !is.na(date_norm)
    if (any(idx_time)) {
      t <- time_part[idx_time]
      h <- as.integer(sub("^([0-9]{1,2}).*", "\\1", t))
      m <- as.integer(sub("^[0-9]{1,2}:([0-9]{1,2}).*", "\\1", t))
      s <- as.integer(sub("^[0-9]{1,2}:[0-9]{1,2}:?([0-9]{1,2})?.*", "\\1", t))
      m[is.na(m)] <- 0
      s[is.na(s)] <- 0
      invalid <- is.na(h) | h > 23 | m > 59 | s > 59
      h[invalid] <- m[invalid] <- s[invalid] <- NA
      time_norm[idx_time] <- ifelse(invalid, NA_character_, sprintf("%02d:%02d:%02d", h, m, s))
    }
    
    # Combine date and time
    result <- ifelse(time_norm == "", date_norm, paste(date_norm, time_norm))
    return(result)
  })
}
