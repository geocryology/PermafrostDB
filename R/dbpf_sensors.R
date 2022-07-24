# =============================================================================
#'
#' @title Return table of sensors
#'
#' @description Basic information on permafrost database @ Carleton University
#'
#' @details These simple functions return all data as data frames. When
#'          making a query many times, optimise the SQL statement to only
#'          request the data you actually need.
#'
#' @param con Database connection object, as returned by dbpf_con()
#' @param manual Return only sensors for manual observation?
#'               This is the default. [boolean]
#'
#' @return List of sensors in DB
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' sen <- dbpf_sensors(con)
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_sensors <- function(con, manual=TRUE) {

    if (manual) {
    	query <- "SELECT * FROM sensors WHERE device_id = '5265f740-8e09-4d41-b5b6-2136f5d35ea3'"
    } else {
    	query <- "SELECT * FROM sensors"
    }

    sensors <- dbGetQuery(con, query)
    return(sensors)
}