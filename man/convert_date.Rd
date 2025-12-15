\name{convert_date}
\alias{convert_date}
\title{Convert Dates Between Jalali, Gregorian, and Hijri}
\description{
Convert dates between Jalali (Persian), Gregorian, and Hijri calendars using the pre-built mapping table.
Supports both full names ("jalali", "gregorian", "hijri") and short forms ("j", "g", "h").
}
\usage{
convert_date(dates, from, to)
}
\arguments{
  \item{dates}{Character vector of dates.}
  \item{from}{Character. Source calendar: "jalali"/"j", "gregorian"/"g", or "hijri"/"h".}
  \item{to}{Character. Target calendar: "jalali"/"j", "gregorian"/"g", or "hijri"/"h".}
}
\value{
Character vector of converted dates.
}
\examples{
# Jalali to Gregorian
convert_date("1402-01-01", from = "j", to = "g")

# Gregorian to Jalali
convert_date("2023-03-21", from = "g", to = "j")

# Hijri to Gregorian
convert_date("1447-09-01", from = "h", to = "g")

# Vectorized conversion
convert_date(c("1402-01-01", "1402-05-15"), from = "j", to = "h")
}
