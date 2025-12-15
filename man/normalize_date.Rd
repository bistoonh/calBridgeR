\name{normalize_date}
\alias{normalize_date}
\title{Normalize Date Strings}
\description{
Normalize input date strings to the standard \code{"YYYY-MM-DD"} format,
with optional \code{"HH:MM:SS"} time component. Automatically fixes incomplete
dates (e.g. \code{"YYYY-MM"} → \code{"YYYY-MM-01"}) and pads zeros.
Returns \code{NA} for invalid or unrecognized dates.
}
\usage{
normalize_date(dates)
}
\arguments{
  \item{dates}{Character vector of date strings (with optional time).}
}
\value{
Character vector of normalized dates in \code{"YYYY-MM-DD"} format,
optionally with \code{"HH:MM:SS"}.
}
\examples{
# Jalali dates
normalize_date(c("14020101", "1402-01", "1402-01-01 14:30"))

# Gregorian dates
normalize_date(c("2023/3/21", "2023-03"))

# Hijri dates
normalize_date(c("14470901", "1447-09-01 08:00:00"))

# Mixed input
normalize_date(c("1402-01-01", "2023-03-21", "1447-09-01"))
}
