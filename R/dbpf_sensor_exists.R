# =============================================================================
#'
#' @title Check if a sensor exists
#'
#' @description Checks if device exists based on its serial number
#'
#' @details These simple functions return all data as data frames. When
#'          making a query many times, optimise the SQL statement to only
#'          request the data you actually need.
#'
#' @param con Database connection object, retuned by dbpf_con()
#'
#' @param sensor_id Name of the sensor to query
#'
#' @return 0 (does not exist) or 1 (does exist)
#'
#' @export
#' @examples
#' con <- dbpf_con()
#' fds <- dbpf_sensor_exists(con, "Storage")
#' dbDisconnect(con)
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_sensor_exists <- function(con, sensor_id) {
  query <- paste0("SELECT COUNT(*) FROM sensors WHERE id = '", sensor_id, "'")
  exists <- dbGetQuery(con, query)
  
  return(exists$count)
}
