# =============================================================================
#'
#' @title Check if a location name exists
#'
#' @description Checks if device exists based on its serial number 
#'
#' @details These simple functions return all data as data frames. When 
#'          making a query many times, optimise the SQL statement to only 
#'          request the data you actually need.
#'
#' @param con Database connection object, retuned by dbpf_con()
#'
#' @param location_name Name of teh location to query
#' 
#' @return 0 (does not exist) or 1 (does exist) 
#' 
#' @export
#' @examples
#' con <- dbpf_con()
#' fds <- dbpf_location_name_exists()
#' dbDisconnect(con)
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_location_name_exists <- function(con, location_name) {
	query <- paste0("SELECT COUNT(*) FROM locations WHERE name = '",
	               location_name, "'")
	exists <- dbGetQuery(con, query)
	
	return(exists$count)
}
