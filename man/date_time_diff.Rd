\name{date_time_diff}
\alias{date_time_diff}
\title{Compute Date-Time Differences Between Dates}
\description{
Compute the difference in days and times between two date-time values in Jalali, Gregorian, or Hijri calendars using the pre-built mapping table.
Input dates are normalized with \code{normalize_date()} to ensure consistency.
If one of the inputs has no time component, it is assumed to be \code{00:00:00}.
}
\usage{
date_time_diff(start_date, end_date, calendar)
}
\arguments{
  \item{start_date}{Character vector of start dates (with optional time).}
  \item{end_date}{Character vector of end dates (with optional time).}
  \item{calendar}{Character. Calendar type: "jalali"/"j", "gregorian"/"g", or "hijri"/"h".}
}
\value{
A list with two components:
\itemize{
  \item \code{seconds}: Numeric vector of total differences in seconds (end_date - start_date).
  \item \code{formatted}: Character vector of differences formatted as \code{"X days HH:MM:SS"}.
}
Returns NA if either input is invalid or NA.
}
\examples{
# Pure date difference (Jalali short form)
date_time_diff("1402-01-01", "1402-01-03", calendar = "j")$formatted

# Pure date difference (Jalali full form)
date_time_diff("1402-01-01", "1402-01-03", calendar = "jalali")$formatted

# Date-time difference with hours and minutes (Gregorian)
date_time_diff("2025-03-21 12:00:00", "2025-03-23 14:30:00", calendar = "gregorian")$seconds
date_time_diff("2025-03-21 12:00:00", "2025-03-23 14:30:00", calendar = "gregorian")$formatted

# Vectorized usage (Hijri short form)
date_time_diff(c("1447-09-01 08:00:00","1447-09-05"),
               c("1447-09-03 09:15:00","1447-09-10"),
               calendar = "h")$formatted

# Vectorized usage (Hijri full form)
date_time_diff(c("1447-09-01 08:00:00","1447-09-05"),
               c("1447-09-03 09:15:00","1447-09-10"),
               calendar = "hijri")$formatted

# Handling NA values (Jalali)
date_time_diff(c("1402-01-01","1402-01-05"),
               c("1402-01-03", NA),
               calendar = "jalali")$seconds
}
