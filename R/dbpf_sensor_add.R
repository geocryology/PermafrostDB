# =============================================================================
#'
#' @title Adds a new sensor to DB
#'
#' @description Adds a new sensor to DB and tests for existence of device_id
#'              and possible duplicate sensor lable.
#'
#' @details Run in test mode first. If you have no DB login to write data, run
#'          in test mode with your login and then pass to someone who does.
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#' 
#' @param device_id Charcter string of device that this is linked to.
#'
#' @param label Name for the new sensor.
#'
#' @param serial_number Serial number, usuall left empty
#'
#' @param type_of_measurement 'numeric' or 'text'
#'
#' @param unit_of_measurement Character string, give SI unit used.
#'
#' @param accuracy Accuracy in units used.
#'
#' @param precision Precision (absolute) given in the units used.
#'
#' @param height_in_metres Positive number above ground, negative below ground.
#'
#' @param mode = Can be 'test' (default, read-only) or 'insert' (will insert into DB)
#'
#' @return Test results as well as sensor id if data was inserted.
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' dbpf_sensor_add(con, "5265f740-8e09-4d41-b5b6-2136f5d35ea3",
#'                 "vegetation_height_max", type_of_measurement = "numeric",
#'                 unit_of_measurement = "m", accuracy = 0.5, precision=0.01,
#'                 height_in_metres = 0, serial_number = "", mode = 'test')
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================
 
dbpf_sensor_add <- function(con, device_id, label, type_of_measurement,
                            unit_of_measurement, accuracy, precision,
                            height_in_metres = 0, serial_number = "",
                            mode = "test") {

    # === RUN TESTS ===
    # initial string
    teststring <- "Test result"
    passed <- FALSE

    # check if device id exists
    query <- paste0("SELECT COUNT(id) FROM devices WHERE id = '",
            device_id, "'")
    res <- dbGetQuery(con, query)$count
    if (res == 0) {
        teststring <- paste(teststring, "device id not found", sep = ": ")
    }

    #check type_of_measurement
    ok <- (type_of_measurement == "numeric") + (type_of_measurement == "text")
    if (ok == 0) {
        teststring <- paste(teststring, "type_of_measurement must be 'text' ",
                            "or 'numeric'", sep = ": ")
    }

    if (teststring == "Test result") {
        teststring <- "Test result: OK"
        passed <- TRUE
    }

    # === INSERT ===
    if ((mode == "insert") * (passed == TRUE)) {
        query <- paste0("INSERT INTO sensors (label, type_of_measurement, ",
                            "unit_of_measurement, accuracy, precision, ",
                            "height_in_metres, serial_number) VALUES ('",
                            paste(label, type_of_measurement,
                            unit_of_measurement, accuracy, precision,
                            height_in_metres, serial_number, sep = "', '"),
                            "') RETURNING id")

        try(ins <- dbGetQuery(con, query), silent = TRUE)
        if (exists("ins")) {
            teststring <- paste0(teststring, " ==> Sensor added. Resulting ",
            "sensor id: ", ins)
        }
        query <- paste0("INSERT INTO devices_sensors (timestamp,
                            device_id, sensor_id) VALUES ('",
                            paste(lubridate::now(), device_id, ins,
                            sep = "', '"), "') RETURNING id")

        try(ins <- dbGetQuery(con, query), silent = TRUE)
        if (exists("ins")) {
            teststring <- paste0(teststring, " ==> dev_sen table updated.",
            "Resulting id: ", ins, "\n")
        }
    }
    return(teststring)
}
