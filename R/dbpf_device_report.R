# =============================================================================
#'
#' @title Print a report on a specific device
#'
#' @description Given a device serial number, this function return a data frame
#'              showing the locations where this device has been.  
#'
#' @details 
#'
#' @param serial_number Serial number of device to query
#'
#' @return Prints information to screen. 
#' 
#' @export
#' @examples
#' dbpf_device_report("E509DA")
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_device_report <- function(serial_number) {
	con <- dbpf_con()
	exists <- dbpf_device_exists(con, serial_number)
	if (exists == 1) {
    		loc <- dbpf_device_locations(con, serial_number)
    	sen <- dbpf_device_sensors(con, serial_number)
    	query <- paste0("SELECT * FROM devices WHERE serial_number = '", serial_number, "'")        
	    dev <- dbGetQuery(con, query)
		dbDisconnect(con)
		
		writeLines("\n\n")
		print(paste("=== DETAILS for device", serial_number, "============="))	
		print(dev)
		writeLines("\n\n")
		print(paste("=== LOCATION HISTORY on device", serial_number, "============="))
		print(loc)
		writeLines("\n")
		print(paste("=== SENSORS on device", serial_number, "======================"))
		print(sen)
		writeLines("\n\n")
	} else {
		writeLines("\n\n")
		print(paste("=== Device", serial_number, " DOES NOT EXIST"))
		writeLines("\n")
	}
}