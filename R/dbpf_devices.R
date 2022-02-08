# =============================================================================
#'
#' @title Return table of devices
#'
#' @description Basic information on permafrost database @ Carleton University 
#'
#' @details These simple functions return all data as data frames. When 
#'          making a query many times, optimise the SQL statement to only 
#'          request the data you actually need.
#'
#' @param con Database connection object, retuned by dbpf_con()
#' 
#' @return List of all devices for all tables in DB 
#' 
#' @export
#' @examples
#' fds <- dbpf_devices()
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_devices <- function(con) {
  if (missing(con)){
	  con <- dbpf_con() # get connection
  }
	query <- paste("SELECT * FROM devices")
	devices <- dbGetQuery(con, query)
	devices <- subset(devices, select=-id)
	return(devices)
}