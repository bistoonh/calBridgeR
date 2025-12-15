\name{day2day}
\alias{day2day}
\title{Generate a Sequence of Dates}
\description{
Generate all dates between a start and end date in Jalali, Gregorian, or Hijri calendar.
Start and end times are preserved if provided.
}
\usage{
day2day(start_date, end_date, calendar)
}
\arguments{
  \item{start_date}{Character. Start date.}
  \item{end_date}{Character. End date.}
  \item{calendar}{Character. Calendar type: "jalali"/"j", "gregorian"/"g", or "hijri"/"h".}
}
\value{
Character vector of dates (with times for start and end if present).
}
\examples{
# Jalali sequence (short form)
day2day("1402-01-01", "1402-01-03", calendar = "j")

# Jalali sequence (full form)
day2day("1402-01-01", "1402-01-03", calendar = "jalali")

# Jalali with times
day2day("1402-01-01 08:00:00", "1402-01-03 18:00:00", calendar = "jalali")

# Gregorian sequence (short form)
day2day("2023-03-21", "2023-03-23", calendar = "g")

# Gregorian sequence (full form)
day2day("2023-03-21", "2023-03-23", calendar = "gregorian")

# Hijri sequence (short form)
day2day("1447-09-01", "1447-09-03", calendar = "h")

# Hijri sequence (full form)
day2day("1447-09-01", "1447-09-03", calendar = "hijri")
}
