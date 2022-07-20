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
#'                oldSerial (character)
#'                newSerial (character)
#'                comment (character)
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

    #test mode
    test_mo <- (mode == "test") + (mode == "insert")
    if (test_mo != 1) {
        stop(paste("Parameter 'mode' must be either",
                    "'test' or 'insert'."))
    }

    #test information provided
    dev_sen <- subset(dev_sen, select = c("oldSerial", "newSerial",
                            "comment", "sitename", "time"))

    #fix/test column data type, add check columns
    dev_sen$oldSerial <- as.character(dev_sen$oldSerial)
    dev_sen$newSerial <- as.character(dev_sen$newSerial)
    dev_sen$sitename     <- as.character(dev_sen$sitename)
    dev_sen$comment      <- as.character(dev_sen$comment)

    if (lubridate::is.POSIXct(dev_sen$time) == FALSE) {
        stop("Column 'time' must be in POSIXct")
    }

    dev_sen$old <- FALSE
    dev_sen$new <- FALSE
    dev_sen$result <- FALSE
    dev_sen$message <- FALSE

    # make time string for postgresql "2015-06-14 15:24:00+00"
    dev_sen$time <- substr(format(dev_sen$time,
                                format = "%Y-%m-%d %H:%M:%S%z"), 1, 22)

    #check for duplicates (in dev_sen table)
    dev_sen <- unique(dev_sen)

    #loop over rows in table
    for (r in 1:nrow(dev_sen)) {
        # Do all devices exist?
        dev_sen$old[r] <- (dbpf_device_exists(con, dev_sen$oldSerial[r]) == 1)
        dev_sen$new[r] <- (dbpf_device_exists(con, dev_sen$newSerial[r]) == 1)

        old_dev_id <- dbGetQuery(con, paste0("SELECT id FROM devices ",
        "WHERE serial_number ='", dev_sen$oldSerial[r], "'"))

        new_dev_id <- dbGetQuery(con, paste0("SELECT id FROM devices ",
        "WHERE serial_number ='", dev_sen$newSerial[r], "'"))

        # If both old and new loggers exist
        if (dev_sen$old[r] + dev_sen$new[r] == 2) {

            # Get all sensors attatched to old_dev_id
            sen_ids <- dbpf_device_sensors(con, dev_sen$oldSerial[r])

            for (s in 1:nrow(sen_ids)){
                a <- dbpf_sensor_exists(con, sen_ids$sensor_id[s])
                if (a != 1){
                    print(paste0(sen_ids$sensor_id[s], " DOES NOT EXIST."))
                }
            }
            for (s in 1:nrow(sen_ids)) {
                query <- paste0("INSERT INTO devices_sensors",
                "(timestamp, device_id, sensor_id, notes) VALUES ('",
                dev_sen$time[r], "', '", new_dev_id, "', '",
                sen_ids$sensor_id[s], "', '", dev_sen$comment[r], "');")
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
            }
        } else {
            dev_sen$result[r] <- "FAILED"
            dev_sen$message[r] <- "Logger(s) don't not exist."
        }
    }
    print(dev_sen[, c("oldSerial", "old", "newSerial",
                        "new", "result", "message")])
}


dbpf_sensor_exists <- function(con, sensor_id) {
    query <- paste0("SELECT COUNT(*) FROM sensors WHERE id = '",
                   sensor_id, "'")
    exists <- dbGetQuery(con, query)

    return(exists$count)
}

dev_sen <- read.csv("../dev_sen.csv")
dev_sen$time <- as.POSIXct(dev_sen$time)
dbpf_devices_sensors_add(con, dev_sen, mode = "test")