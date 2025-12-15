\name{normalize_date}
\alias{normalize_date}
\title{Normalize Date Strings}
\description{
Normalize Jalali or Gregorian date strings to "YYYY-MM-DD" format, optionally with time.
Automatically fixes incomplete dates and time components.
}
\usage{
normalize_date(dates, calendar = "jalali")
}
\arguments{
  \item{dates}{Character vector of dates.}
  \item{calendar}{Character. Either "jalali" or "gregorian". Default is "jalali".}
}
\value{
Character vector of normalized dates.
}
\examples{
normalize_date(c("14020101", "1402-01", "1402-01-01 14:30:00"))
normalize_date(c("2023/03/21", "2023-03", "2023-03-21 08:15"))
}
