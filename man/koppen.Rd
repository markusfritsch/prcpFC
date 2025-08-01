% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/koppen.R
\docType{data}
\name{koppen}
\alias{koppen}
\title{Koppen climate zone classification data}
\format{
A dataset with 91828 rows and 3 variables containing:
\describe{
\item{Lat}{latitude (CRS WGS84)}
\item{Lon}{longitude (CRS WGS84)}
\item{cls}{Koppen climate zone classification}
#' }
}
\usage{
data(koppen)
}
\description{
Dataset with Koppen climate zone classification data. The
dataset was put together by \insertCite{kottek2006}{prcpFC}
and contains the variables longitude, latitude, and the
three letter Koppen climate zone classification.
}
\details{
The dataset is employed in \insertCite{fritsch2025}{prcpFC}
to analyze the memory properties of the time series and
generate forecasts based on the memory parameter estimates.
For more details on the classification scheme, see
\insertCite{koppen1884warmezonen}{prcpFC},
\insertCite{koppen1900versuchI}{prcpFC},
\insertCite{koppen1900versuchII}{prcpFC},
\insertCite{koppen1918klassifikation}{prcpFC}, and
\insertCite{koppen1936geographische}{prcpFC}. For the
employed thresholds, see \insertCite{beldaEtAl2014climate}{prcpFC}
}
\examples{
\dontrun{
  data(koppen, package = "prcpFC")
  table(koppen$cls)
}

}
\references{
\insertAllCited{}
}
\keyword{datasets}
