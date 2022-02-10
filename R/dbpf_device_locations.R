# =============================================================================
#'
#' @title Return location history for one device
#'
#' @description Given a device serial number, this function return a data frame
#'              showing the locations where this device has been.
#'
#' @details
#'
#' @param serial_number Serial number of device to query
#'
#' @return data fame with fields: location_name, timestamp, lon, lat,
#'           elevation_in_metres, serial_number, notes. The data frame
#'           is sorted by time.
#'
#' @export
#' @examples
#' con <- dbpf_con()
#' dbpf_device_locations(con, "E509DA")
#' dbDisconnect(con)
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_device_locations <- function(con, serial_number) {
	#make query to run
	query <- paste0("SELECT locations.name AS location_name, devices_locations.timestamp, ",
	                "ST_X(locations.coordinates) AS lon, ",
	                "ST_Y(locations.coordinates) AS lat, ",
	                "locations.elevation_in_metres, ",
	                "devices.serial_number, devices_locations.notes FROM devices_locations ",
	                "INNER JOIN devices ON devices_locations.device_id = devices.id ",
	                "INNER JOIN locations ON devices_locations.location_id = locations.id ",
	                "WHERE devices.serial_number = '", serial_number, "'")

	dev_loc <- dbGetQuery(con, query)
	time <- as.POSIXct(dev_loc$timestamp)
	return(dev_loc[order(time),])
}