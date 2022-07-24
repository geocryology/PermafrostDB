# =============================================================================
#'
#' @title List all channels and other information for one device
#'
#' @description Given a device serial number, this function return a data frame
#'              showing the locations where this device has been.
#'
#' @param con Database connection object, retuned by dbpf_con()
#'
#' @param serial_number Serial number of device to query
#'
#' @return data fame with fields: sensor_id label, height_in_metres,
#'         type_of_measurement, unit_of_measurement, accuracy, precision
#'
#' @export
#' @examples
#'
#' con <- dbpf_con()
#' dbpf_device_sensors(con, "E509DA")
#' dbDisconnect(con)
#'
#' @author Hannah Macdonell <hannah.macdonell@@carleton.ca>
# =============================================================================

dbpf_device_sensors <- function(con, serial_number) {
    # Get device id from serial_number
    dev_id <- dbGetQuery(con, paste0("SELECT id FROM devices ",
                "WHERE serial_number ='", serial_number, "'"))

    # Get most recent sensor log timestamp
    time <- dbGetQuery(con, paste0("SELECT timestamp FROM devices_sensors",
                " WHERE device_id = '", dev_id, "' GROUP BY ",
                "devices_sensors.id ORDER BY timestamp DESC LIMIT (1)"))

    if (nrow(time) < 1){
        print(paste0("Device '", serial_number, "' has no sensors."))
        return(0)
    }

    # Find all sensors that were updated for this device at that time
    sen_ids <- dbGetQuery(con, paste0("SELECT sensor_id FROM devices_sensors ",
                "WHERE device_id = '", dev_id, "' AND timestamp = '",
                time$timestamp, "'"))

    sen_df <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
            c("id", "label", "height_in_metres", "type_of_measurement",
            "unit_of_measurement", "accuracy"))

    # Iterate through sensor ids
    for (r in 1:nrow(sen_ids)){
        query <- paste0("SELECT id, label, height_in_metres, ",
                    "type_of_measurement, unit_of_measurement, accuracy ",
                    "FROM sensors WHERE id = '", sen_ids$sensor_id[r], "'")
        db_df <- dbGetQuery(con, query)
        sen_df <- rbind(sen_df, db_df)
    }
    names(sen_df)[1] <- "sensor_id"
    return(sen_df)
}