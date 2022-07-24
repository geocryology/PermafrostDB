# =============================================================================
#'
#' @title Return table of locations, either points or polygons.
#'
#' @description Basic information on permafrost database @ Carleton University
#'
#' @details These simple functions return all data as data frames. When
#'          making a query many times, optimise the SQL statement to only
#'          request the data you actually need.
#' 
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#' 
#' @param type Identifies the geometry type for which locations are returned.
#'             The default is 'point', otherwise set type='polygon'. [character]
#'
#' @return Data frame of all [point / polygon] locations in DB
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' loc <- dbpf_locations(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_locations <- function(con, type='point') {

  if (missing(con)){
      con <- dbpf_con()
  }

    if (toupper(type) == 'POINT') {
    	query <- paste("SELECT id, name, ST_X(coordinates) AS lon,",
                   	"ST_Y(coordinates) AS lat,",
                   	"elevation_in_metres, comment, record_observations",
                   	"FROM locations WHERE ST_GeometryType(coordinates)='ST_Point'")
    } else if (toupper(type) == 'POLYGON') {
    	query <- paste("SELECT id, name, ST_AsText(coordinates) as coordinates, ",
                   	"elevation_in_metres, comment, record_observations",
                   	"FROM locations WHERE ST_GeometryType(coordinates)='ST_Polygon'")
    } else {
    	stop('Value for type not valid.')
    }

    locations <- dbGetQuery(con, query)
    return(locations)
}
