% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Date_time_handling.R
\name{resampleInterval}
\alias{resampleInterval}
\title{Title}
\usage{
resampleInterval(traj, interval)
}
\arguments{
\item{traj}{a move file}

\item{interval}{The interval of the added datetime variable. Any character string that would be accepted by seq.Date or seq.POSIXt.}
}
\value{
A sf object resampled to the specified interval. The centroid is taken from the points within one interval.
}
\description{
Title
}
\examples{
resampleTimestamp(movefile, 'week')
}
