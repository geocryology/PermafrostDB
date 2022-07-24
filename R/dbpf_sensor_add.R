# =============================================================================
#'
#' @title Adds a new sensor to DB
#'
#' @description Adds a new sensor to DB and tests for possible duplicate
#'              sensor lable.
#'
#' @details Run in test mode first. If you have no DB login to write data, run
#'          in test mode with your login and then pass to someone who does. Note
#'          a sensor_id must be linked to a device_id using devices_sensors_add.
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
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
#' @param mode = Can be 'test' (default, read-only) or
#'                      'insert' (will insert into DB)
#'
#' @return Test results as well as sensor id if data was inserted.
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' dbpf_sensor_add(con,
#'                 "vegetation_height_max", type_of_measurement = "numeric",
#'                 unit_of_measurement = "m", accuracy = 0.5, precision=0.01,
#'                 height_in_metres = 0, serial_number = "", mode = 'test')
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_sensor_add <- function(con, label, type_of_measurement,
                            unit_of_measurement, accuracy, precision,
                            height_in_metres = 0, serial_number = "",
                            mode = "test") {

    # === RUN TESTS ===
    # initial string
    teststring <- "Test result"
    passed <- FALSE

    #check type_of_measurement
    ok <- (type_of_measurement == "numeric") + (type_of_measurement == "text")
    if (ok == 0) {
        teststring <- paste(teststring, "type_of_measurement must be 'text' ",
                            "or 'numeric'", sep = ": ")
    }

    #check unit_of_measurement
    unit_list <- dbGetQuery(con, paste0("SELECT DISTINCT(unit_of_measurement) ",
                                        "FROM sensors"))
    if (!unit_of_measurement %in% list(unit_list$unit_of_measurement)[[1]]) {
        err <- paste0("unit_of_measurement '", unit_of_measurement,
                        "' doesn't exist in database.")
        if (!readline(paste0(err, " To add anyways or continue with testing, ",
                                "type 'Y':")) %in% c("y", "Y")) {
            teststring <- paste(teststring, err, sep = ": ")
            print(teststring)
        }
    }

    #check height_in_metres
    height_list <- dbGetQuery(con, paste0("SELECT DISTINCT(height_in_metres) ",
                                        "FROM sensors"))
    if (!height_in_metres %in%
                    list(as.character(height_list$height_in_metres))[[1]]) {
        err <- paste0("height_in_metres '", height_in_metres,
                        "' doesn't exist in database.")
        if (!readline(paste0(err, " To add anyways or continue with testing, ",
                                "type 'Y':")) %in% c("y", "Y")) {
            teststring <- paste(teststring, err, sep = ": ")
            print(teststring)
        }
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

        try(ins <- dbGetQuery(con, query), silent = FALSE)
        if (exists("ins")) {
            teststring <- paste0(teststring, " ==> Sensor added. Resulting ",
            "sensor id: ", ins)
        }
    }
    return(teststring)
}
