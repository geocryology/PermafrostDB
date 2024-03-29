% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbpf_inventory.R
\name{dbpf_inventory}
\alias{dbpf_inventory}
\title{Inventory observations}
\usage{
dbpf_inventory(con, type = "observations_by_locations")
}
\arguments{
\item{con}{Database connection object, as returned by \code{\link{dbpf_con}}}

\item{type}{Character string indicating the type of inventory to be returned.
Defaults to 'observations_by_locations', others currently not
implemented.}
}
\description{
Return data frame with a summary of observations in DB
}
\details{
Return data frame with a summary of observations in DB
}
\examples{
\dontrun{
con <- dbpf_con()
dbpf_inventory(con) # get all GST automatically
}
}
\author{
Stephan Gruber <stephan.gruber@carleton.ca>
}
