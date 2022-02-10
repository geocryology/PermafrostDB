# =============================================================================
#'
#' @title finds loggers within a plot
#'
#' @description Insert the name of a plot and the function returns the locations within a plot
#'
#' @details
#'
#' @param con connection to database
#'
#' @param locname name of a plot
#'
#'
#' @return table with all the information for the locations locations within a plot
#'
#' @export
#' @examples
#' con <- dbpf_con()
#' locations <- dbpf_loggers_in_plot(con,"NGO-DD-1004_PLOT")
#'
#' @author Thomas Knecht
# =============================================================================

dbpf_locations_in_plot <- function(con, locname) {
  con <- dbpf_con() # get connection


  query <- paste0("SELECT ST_AsText(coordinates) as coordinates ",
                 "FROM locations WHERE name ='",locname,"'")
  locations <- dbGetQuery(con, query)

  query <- paste0("SELECT * FROM locations WHERE ST_Within(locations.coordinates, ST_SetSRID(ST_GeomFromText('",locations,"'),4326))")
  loggers <- dbGetQuery(con, query)

  return(loggers)
}
