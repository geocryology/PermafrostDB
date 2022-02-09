# =============================================================================
#'
#' @title List all channels and other information for one device
#'
#' @description Given a device serial number, this function return a data frame
#'              showing the locations where this device has been.  
#'
#' @details 
#'
#' @param con Database connection object, retuned by dbpf_con()
#'
#' @param serial_number Serial number of device to query
#'
#' @return data fame with fields: label, height_in_metres, type_of_measurement,
#'         unit_of_measurement, accuracy, precision 
#' 
#' @export
#' @examples
#' con <- dbpf_con()
#' dbpf_device_sensors(con, "E509DA")
#' dbDisconnect(con)
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_device_sensors <- function(con, serial_number) {
	#make query to run
	query <- paste0("SELECT sensors.label, sensors.height_in_metres, ",
	                	"sensors.type_of_measurement, sensors.unit_of_measurement, ",
	                	"sensors.accuracy, sensors.precision ",
	                "FROM sensors ",
	                "INNER JOIN devices ON sensors.device_id = devices.id ",
	                "WHERE devices.serial_number = '", serial_number, "'")        
	dev_loc <- dbGetQuery(con, query)
	return(dev_loc)
}