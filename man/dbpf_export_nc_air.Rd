% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbpf_export_nc_air.R
\name{dbpf_export_nc_air}
\alias{dbpf_export_nc_air}
\title{Download air temperature and humidity time series to netCDF}
\usage{
dbpf_export_nc_air(con, location_name, file_name, freq = "daily")
}
\arguments{
\item{con}{Database connection object, as returned by dbpf_con()}

\item{location_name}{Character, one or more location names to convert.}

\item{file_name}{character, path to output netCDF file (*.nc)}

\item{freq}{Character, one of ('daily', 'hourly'). Interval at which data is to
be aggregated from the database.}
}
\description{
Downloads one or more time series from the carleton permafrost database
and export as a netCDF file. Designed to work with Geoprecision LOG-HC2-RC-US
loggers
}
\details{
Requires that the PermafrostDB and ncdf4 packages be installed.
}
\author{
Nick Brown <nick.brown@carleton.ca>
}
