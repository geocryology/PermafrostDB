# =============================================================================
#'
#' @title Check if a device serial number exists
#'
#' @description Checks if device exists based on its serial number 
#'
#' @details These simple functions return all data as data frames. When 
#'          making a query many times, optimise the SQL statement to only 
#'          request the data you actually need.
#'
#' @param con Database connection object, retuned by dbpf_con()
#'
#' @param serial_number Serial number of device to query
#' 
#' @return 0 (does not exist) or teh device 
#' 
#' @export
#' @examples
#' con <- dbpf_con()
#' fds <- dbpf_devices()
#' dbDisconnect(con)
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_device_exists <- function(con, serial_number) {
	query <- paste0("SELECT COUNT(*) FROM devices WHERE serial_number = '",
	               serial_number, "'")
	exists <- dbGetQuery(con, query)
	
	return(exists$count)
}