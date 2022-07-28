# =============================================================================
#'
#' @title Adds new devices_sensors to database
#'
#' @description Inserts a data frame of new devices_sensors into database.
#'
#' @details Devices_sensors indicate the time when a device was connected
#'          to a specific string of sensors. Until assigned to a new string of
#'          sensors, the device is then interpreted to be at that sensor.
#'          Insertion is done one row at a time and feedback is provided.
#'          The default mode (mode='test') only tests the data to be
#'          inserted. Insert mode (mode='insert') requires a database
#'          connection generated with a login that has insert priviledge.
#'          In Test Mode, duplicate rows in the input data are identified
#'          and sensors and devices are tested for existance in the DB.
#'          The input data frame is retuned with three new columns containing
#'          test results.
#'
#' @param dev_sen Data frame with these columns (type):
#'                oldSerial: logger being removed from thermistor chain
#'                newSerial: logger being newly attatched to thermistor chain
#'                comment
#'                time (POSIXct, UTC)
#'
#' @param mode Can be 'test' (default) or
#'             'insert' (requires login with insert previledges)
#'
#' @param con DB connection as returned by dbpf_con(). For insert priviledges
#'            specific user name and password need to be supplied.
#'
#' @return dev_sen dataframe with pass / fail result and message.
#'
#' @export
#' @examples
#' \dontrun{
#' con    <- dbpf_con()
#' devices_sensors <- data.frame(oldSerial='E53128',
#'                                  newSerial='E53339',
#'                                  time=2021-08-19 17:20,
#'                                  comment="")
#' dbpf_devices_sensors_add(con, devices_sensors, mode="test")
#' dbDisconnect(con)
#' }
#' @author Hannah Macdonell <hannah.macdonell@@carleton.ca>
# =============================================================================

dbpf_devices_sensors_add <- function(con, dev_sen, mode = "test") {

    # Test mode
    test_mo <- (mode == "test") + (mode == "insert")
    if (test_mo != 1) {
        stop(paste("Parameter 'mode' must be either",
                    "'test' or 'insert'."))
    }

    # Test information provided
    dev_sen <- dev_sen_formatting(dev_sen)

    # Loop over rows in table
    for (r in 1:nrow(dev_sen)) {
        print(paste0("Moving ", dev_sen$oldSerial[r],
        " sensors to ", dev_sen$newSerial[r]))
        # Do all devices exist?
        dev_sen$old[r] <- (dbpf_device_exists(con, dev_sen$oldSerial[r]) == 1)
        dev_sen$new[r] <- (dbpf_device_exists(con, dev_sen$newSerial[r]) == 1)

        if (dev_sen$old[r] + dev_sen$new[r] != 2) {
            dev_sen$result[r] <- "FAILED"
            dev_sen$message[r] <- "Logger(s) don't not exist."
        } else {

            # Check to ensure newSerial doesn't have sensors attatched to it
            if (dbpf_device_sensors(con, dev_sen$newSerial[r]) != 0) {
                dev_sen$result[r] <- "FAILED"
                dev_sen$message[r] <- "Logger has existing thermisor chain."
                next
            }

            # Get all "#1:oC"-type sensors attatched to old_dev_id
            sen_ids <- dbpf_device_sensors(con, dev_sen$oldSerial[r])
            if (nrow(sen_ids) != 0) {
                sen_ids <- sen_ids[grep("\\#\\d\\:oC", sen_ids$label), ]
            }

            for (s in 1:nrow(sen_ids)) {
                # Ensuring all sensors exist in database
                if (sensor_exists(con, sen_ids$sensor_id[s]) != 1) {
                    next
                }

                # Move sensors from old logger to new logger
                dev_sen <- migrate_sensors(con, dev_sen, r, sen_ids, s, mode)

                # Remove sensors from oldSerial
                remove_chain(con, dev_sen$time[r],
                                get_devid(con, dev_sen$oldSerial[r]),
                                dev_sen$old[r])
            }
        }
    }
    print(dev_sen[, c("oldSerial", "old", "newSerial",
                        "new", "result", "message")])
}


migrate_sensors <- function(con, dev_sen, r, sen_ids, s, mode) {
    query <- paste0("INSERT INTO devices_sensors",
                    "(timestamp, device_id, sensor_id, notes)",
                    " VALUES ('",
                        dev_sen$time[r], "', '",
                        get_devid(con, dev_sen$newSerial[r]), "', '",
                        sen_ids$sensor_id[s], "', '",
                        dev_sen$comment[r], "');")

    if (mode == "test") {

        dev_sen$result[r] <- "PASSED"
        dev_sen$message[r] <- "All devices exist. Testing Passed."

    } else {
        try({
            res <- dbSendQuery(con, query)
            }, silent = TRUE)
        try({
            dbClearResult(res)
            }, silent = TRUE)

        dev_sen$result[r] <- "PASSED"
        dev_sen$message[r] <- "Row inserted into DB"
        }
    return(dev_sen)
}


dev_sen_formatting <- function(dev_sen) {
    dev_sen <- subset(dev_sen, select = c("oldSerial", "newSerial",
                            "comment", "time"))

    # Fix/test column data type, add check columns
    dev_sen$oldSerial <- as.character(dev_sen$oldSerial)
    dev_sen$newSerial <- as.character(dev_sen$newSerial)
    dev_sen$comment      <- as.character(dev_sen$comment)

    if (lubridate::is.POSIXct(dev_sen$time) == FALSE) {
        stop("Column 'time' must be in POSIXct")
    }

    # Create feedback dataframe
    dev_sen$old <- FALSE
    dev_sen$new <- FALSE
    dev_sen$result <- FALSE
    dev_sen$message <- FALSE

    # Make time string for postgresql "2015-06-14 15:24:00+00"
    dev_sen$time <- substr(format(dev_sen$time,
                                format = "%Y-%m-%d %H:%M:%S%z"), 1, 22)
    dev_sen <- dev_sen[order(as.Date(dev_sen$time, format="%d/%m/%Y")), ]

    # Check for duplicates (in dev_sen table)
    dev_sen <- unique(dev_sen)

    return(dev_sen)
}


sensor_exists <- function(con, sensor_id) {
    try({
        res <- dbGetQuery(con, paste0("SELECT COUNT(*) FROM sensors",
                   " WHERE id = '", sensor_id, "' "))
        }, silent = TRUE)
    if (!exists("res")) {
        return(FALSE)
    } else {
        return(res)
    }
}


remove_chain <- function(con, time, dev_id, serial_number) {
    # "no_sensors" sen_id
    comment <- "No thermistor chain on device."
    sen_ids <- dbpf_device_sensors(con, serial_number)
    if ("no_sensors" %in% sen_ids$label) {
        return(c("PASSED", "No sensor chain on this device."))
    }
    sen_ids <- sen_ids[grep("\\#HK.*", sen_ids$label), ]
    if (nrow(sen_ids) > 0) {
        # Update HK sensors
        for (row in 1:nrow(sen_ids)){
            query <- paste0("UPDATE devices_sensors ",
            "SET timestamp = '", time, "', ",
            "device_id = '", dev_id, "', ",
            "sensor_id = '", sen_ids$sensor_id, "', ",
            "notes = '", comment, "' ",
            "WHERE sensor_id = '", sen_ids$sensor_id, "';")
            try({
                res <- dbSendQuery(con, query)
                }, silent = FALSE)
            try({
                dbClearResult(res)
                }, silent = TRUE)
        }
        return(c("PASSED", "Sensor chain removed for this device."))
    } else {
        # Insert no_sensor
        query <- paste0("INSERT INTO devices_sensors (timestamp, ",
                    "device_id, sensor_id, notes) VALUES ('", time,
                    "', '", dev_id, "', '", no_sensor_id(con), "', '",
                    comment, "');")
        try({
            res <- dbSendQuery(con, query)
            }, silent = FALSE)
        try({
            dbClearResult(res)
            }, silent = TRUE)
        return(c("PASSED",  "Sensor chain removed for this device."))
    }
}


get_devid <- function(con, serial_number) {
    id <- dbGetQuery(con, paste0("SELECT id FROM devices ",
        "WHERE serial_number ='", serial_number, "'"))
    return(id)
}
