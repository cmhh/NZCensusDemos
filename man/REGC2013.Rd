% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{REGC2013}
\alias{REGC2013}
\title{Regional Council 2013}
\format{A \code{SpatialPolygonsDataFrame} containing columns:
\describe{
  \item{REGC2013}{Regional council code.}
  \item{REGC2013_name}{Regional council label.}
}}
\source{
\url{http://www.stats.govt.nz/browse_for_stats/Maps_and_geography/Geographic-areas/digital-boundary-files.aspx}
}
\usage{
REGC2013
}
\description{
A \code{SpatialPolygonsDataFrame} containing the Regional Council 2013
pattern.  Features have been reprojected using a web mercator projection (
EPSG: 4326) for convenient display in a web browser.
}
\details{
The area unit features have been simplified using a 1000m tolerance to try
and keep the overall size down.
}
\keyword{datasets}

