# =============================================================================
#'
#' @title List all sensors and other information for one device
#'
#' @description Given a device serial number, this function return a data frame
#'              showing the sensors that have been attatched to the device. 
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

dbpf_devices_sensors <- function(con, serial_number) {
    #make query to run
    q <- paste0("SELECT sensors.label AS label, devices_sensors.timestamp, ",
              "devices_sensors.notes, locations.name FROM devices_sensors ",
              "INNER JOIN devices ON devices_sensors.device_id = devices.id ",
              "INNER JOIN sensors ON devices_sensors.sensor_id = sensors.id ",
              "INNER JOIN sensors ON devices_locations.device_id = devices.id, ",
              "devices_sensors.timestamp = devices.locations.timestamp"
              "WHERE devices.serial_number = '", serial_number, "'")
    dev_sen <- dbGetQuery(con, query)
    return(dev_sen)
}