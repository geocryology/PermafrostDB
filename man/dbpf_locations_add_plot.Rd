% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbpf_locations_add_plot.R
\name{dbpf_locations_add_plot}
\alias{dbpf_locations_add_plot}
\title{Adds new polygons to database}
\usage{
dbpf_locations_add_plot(con, locations, mode = "test", overlap = "prevent")
}
\arguments{
\item{con}{connection to database}

\item{locations}{Data frame with these columns (type):
name (character);
NW_lat (numeric, latitude in WGMS84);
NW_lon (numeric, longitude in WGMS84);
NE_lat (numeric, latitude in WGMS84);
NE_lon (numeric, longitude in WGMS84);
SE_lat (numeric, latitude in WGMS84);
SE_lon (numeric, longitude in WGMS84);
SW_lat (numeric, latitude in WGMS84);
SW_lon (numeric, longitude in WGMS84);
elevation_in_metres (numeric);
comment (character);
record_observations (character, can only be 't' or 'f')}

\item{mode}{Can be 'test' (default) or 'insert' (requires login with insert previledges)}

\item{overlap}{Can be 'allow' or 'prevent' (default) and governs whether
a new polygon can overlap an existing one.}
}
\value{
List of all locations with a column 'inserted' indicating which
        ones were inserted, columns 'duplicate_name' indicates existing name
        conflict, ‘intersect_plot’ indicates that two plots intersect.
}
\description{
Inserts a data frame of new plots into database. Insertion
             is done one plot at a time and feedback is provided.
             Duplicate names are identified.
             The default mode (mode='test') only tests the data to be
             inserted. Insert mode (mode='insert') requires a database
             connection generated with a login that has insert priviledge.
}
\details{
These simple functions return all data as data frames. When
         making a query many times, optimise the SQL statement to only
         request the data you actually need.
}
\examples{
\dontrun{
con <- dbpf_con()
result <- dbpf_plots_add(con, locations)
}
}
\author{
Thomas Knecht <t.knecht@hotmail.com>
        Stephan Gruber <stephan.gruber@carleton.ca>
}
