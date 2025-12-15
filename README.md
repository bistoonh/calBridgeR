# calBridgeR

**calBridgeR** is an R package for **fast and accurate conversion** between Jalali (Persian), Gregorian, and Hijri calendars.  
It efficiently handles **large datasets** using a **pre-built mapping table** covering multiple centuries across all three calendars.

The package provides five main functions:

1. `normalize_date()`: Normalize date strings to `"YYYY-MM-DD"` format, with optional time. Supports Jalali (`j`), Gregorian (`g`), and Hijri (`h`).  
2. `convert_date()`: Convert dates between any two calendars (Jalali, Gregorian, Hijri) using the pre-built mapping.  
3. `day2day()`: Generate a sequence of dates between two given dates in the chosen calendar, including optional start and end times.  
4. `date_diff()`: Compute the difference in days between two dates in any supported calendar using fast index lookup.  
5. `date_time_diff()`: Compute the difference between two date-time values in any calendar. Returns both raw seconds and a formatted `"X days HH:MM:SS"` string.

The package uses **data.table** for high-speed processing when installed, and falls back to a fast hash-based lookup if not.  

---

## Installation

```r
# install.packages("devtools") # if not installed
devtools::install_github("bistoonh/calBridgeR", force = TRUE)
```

---

## Examples

```r
library(calBridgeR)

# Normalize dates (Jalali)
normalize_date(c("14020101", "1402-01", "1402-01-01 14:30:00"), calendar = "j")
# [1] "1402-01-01" "1402-01-01" "1402-01-01 14:30:00"

# Convert Jalali to Gregorian
convert_date("1402-01-01", from = "j", to = "g")
# [1] "2023-03-21"

# Convert Gregorian to Hijri
convert_date("2023-03-21", from = "g", to = "h")
# [1] "1444-08-29"

# Generate a sequence of Hijri dates
day2day("1447-09-01", "1447-09-03", calendar = "h")
# [1] "1447-09-01" "1447-09-02" "1447-09-03"

# Include start and end times
day2day("2025-03-21 08:00:00", "2025-03-23 18:00:00", calendar = "g")
# [1] "2025-03-21 08:00:00" "2025-03-22" "2025-03-23 18:00:00"

# Multiple conversions in a vector
dates <- c("1402-01-01", "1402-05-15", "1402-12-29")
convert_date(dates, from = "j", to = "g")
# [1] "2023-03-21" "2023-08-06" "2024-03-18"

# Compute day differences (Gregorian)
date_diff("2025-03-21", "2025-03-25", calendar = "g")
# [1] 4

# Vectorized day differences (Hijri)
date_diff(c("1447-09-01","1447-09-05"), c("1447-09-03","1447-09-10"), calendar = "h")
# [1] 2 5

# Compute date-time differences (seconds + formatted)
res <- date_time_diff("1402-01-01 12:00:00", "1402-01-02 14:30:00", calendar = "j")
res$seconds   # 95400
res$formatted # "1 days 02:30:00"
```

---

## Data

The package includes a **built-in dataset**:

```r
data("calendar_map", package = "calBridgeR")
head(calendar_map)
```

This table maps Jalali, Gregorian, and Hijri dates across multiple centuries, allowing instant lookup without calculations. It is the core of the package’s high performance conversion functions.

---

## License

MIT License
