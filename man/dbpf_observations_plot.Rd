% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbpf_observations_plot.R
\name{dbpf_observations_plot}
\alias{dbpf_observations_plot}
\title{Plot data for one location}
\usage{
dbpf_observations_plot(
  con,
  location_name,
  unit_of_measurement = "C",
  period = "raw",
  time_b = "2018-07-01 00:00:00+00",
  time_e = "2021-08-05 23:59:59+00",
  verbose = FALSE
)
}
\arguments{
\item{con}{Database connection object, as returned by dbpf_con()}

\item{location_name}{Character string of the location to be plotted.}

\item{unit_of_measurement}{Unit of measurments, defaults to "C"}

\item{period}{Period over wich to aggregate time series [h], defaults
to no aggregation (period = 'raw').}

\item{time_b}{Begin time for the interval to be analysed. Use the format
"1950-01-01 00:00:00+00"}

\item{time_e}{End time for the interval to be analysed. Use the format
"1950-01-01 00:00:00+00"}

\item{verbose}{Provide terminal output of the query string? (defaults to FALSE)}
}
\value{
Data frame with locations in rows and columns loc_name, height,
        max, min, avg, cnt
dbpf_observations_plot
}
\description{
Opens interactive plot in browser in order to check and explore
             data series for one location.
}
\details{
Allows to set aggregation parameters.
}
\examples{
\dontrun{
dbpf_observations_plot(con, "YK16-SO06_01")
}
}
\author{
Stephan Gruber <stephan.gruber@carleton.ca>
}
