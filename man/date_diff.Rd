\name{date_diff}
\alias{date_diff}
\title{Compute Day Differences Between Dates}
\description{
Compute the difference in days between two dates in Jalali, Gregorian, or Hijri calendars using the pre-built mapping table.
Input dates are normalized with \code{normalize_date()} to ensure consistency.
}
\usage{
date_diff(start_date, end_date, calendar)
}
\arguments{
  \item{start_date}{Character vector of start dates.}
  \item{end_date}{Character vector of end dates.}
  \item{calendar}{Character. Calendar type: "jalali"/"j", "gregorian"/"g", or "hijri"/"h".}
}
\value{
Integer vector of day differences (end_date - start_date). Returns NA if either input is invalid or NA.
}
\examples{
# Jalali differences (short form)
date_diff("1402-01-01", "1402-01-03", calendar = "j")

# Jalali differences (full form)
date_diff("1402-01-01", "1402-01-03", calendar = "jalali")

# Vectorized Jalali differences
date_diff(c("1402-01-01","1402-01-05"),
          c("1402-01-03","1402-01-10"),
          calendar = "jalali")

# Gregorian differences
date_diff("2023-03-21", "2023-03-25", calendar = "gregorian")

# Hijri differences
date_diff("1447-09-01", "1447-09-03", calendar = "h")

# Vectorized Hijri differences with NA
date_diff(c("1447-09-01","1447-09-05"),
          c("1447-09-03", NA),
          calendar = "hijri")
}
