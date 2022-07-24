# =============================================================================
#'
#' @title Add manual observations to DB
#'
#' @description Tests and optionally inserts one manual observation into DB.
#'
#' @details Data is checked for existence (or duplication) of sensor and
#'          location and feedback is returned.
#'
#' @param con Database connection object, as returned by dbpf_con()
#' @param sensor_label sensor label used to obtain sensor ID [character]
#' @param location_name location name used to obtain location ID [character]
#' @param time_UTC time of observation [POSIXct]
#' @param numeric_value observed value / result [numeric]
#' @param text_value observed value / result [character]
#' @param height_min_metres [numeric]
#' @param height_max_metres [numeric]
#' @param mode Should data be inserted into DB? Defaults to 'test' so that
#'             only testing information is returned. To insert: 'insert'
#'
#'
#' @export
#' @examples
#' \dontrun{
#' con  <- dbpf_con()
#' data <- dbpf_manual_obs_add(con, sensor_label, location_name, time_UTC,
#'								value, height_min_metres, height_max_metres,
#'								insert = FALSE)
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_manual_obs_add <- function(con, sensor_label, location_name, time_UTC,
                                numeric_value, text_value,
                                height_min_metres, height_max_metres,
                                mode = "test") {

  # for 'none' sensor labels in xlsx files, just stop here
    if (sensor_label == "none") {
        return("skipping sensor: none")
    }

    # Time string
    f <- "%Y-%m-%d %H:%M:%S+00"

    #--- testing --------------------------------------------------------------
    teststring <- "Testing"

    # check/get sensor information
    q <- paste0("SELECT COUNT(id) FROM sensors WHERE label = '",
                sensor_label, "';")
    sc <- dbGetQuery(con, q)$count
    if (sc == 1) {
        q <- paste0("SELECT * FROM sensors WHERE label = '", sensor_label, "';")
        sen <- dbGetQuery(con, q)
    } else if (sc == 0) {
        teststring <- paste(teststring, ": Sensor label missing in DB:",
                            sensor_label)
    } else {
        teststring <- paste(teststring, ": Sensor label duplicated in DB:",
                            sensor_label)
    }

    # check/get location information
    q <- paste0("SELECT COUNT(id) FROM locations WHERE name = '",
                    location_name, "';")
    lc <- dbGetQuery(con, q)$count
    if (lc == 1) {
        q <- paste0("SELECT coordinates, elevation_in_metres FROM locations ",
                    "WHERE name = '", location_name, "';")
        # supress warning about unknown field type
        loc <- suppressWarnings(dbGetQuery(con, q))
    } else if (lc == 0) {
        teststring <- paste(teststring, "Location name missing in DB",
                            sep = " : ")
    } else {
        teststring <- paste(teststring, "Location name duplicated in DB",
                            sep = " : ")
    }

    # check for duplicate on time window, location, sensor
    if ((lc == 1) * (sc == 1) == 1) {
        tol <- 600 # [s] time frame within which to find duplicates
        time_b <- strftime(time_UTC - tol, f, usetz = FALSE)
        time_e <- strftime(time_UTC + tol, f, usetz = FALSE)

        #construct query
        q <- paste0("SELECT COUNT (DISTINCT id) FROM observations WHERE ",
                    "logged_time BETWEEN '", time_b, "' AND '",
                    time_e, "' AND sensor_id = '", sen$id, "' AND ",
                    "location = '", loc$coordinates, "' AND ",
                    "height_min_metres = '", height_min_metres, "' AND ",
                    "height_max_metres = '", height_max_metres, "';")
        c <- dbGetQuery(con, q)$count
        if (c > 0) {
            teststring <- paste(teststring, "Existing observation for ",
                        "sensor_id, location, time (+/- 10 min)", sep = " : ")
        }
    }

    # is test is failed, return result (and thereby exit function, here)
    if (teststring != "Testing") return(teststring)

    #--- insert data -----------------------------------------------------------
    if (toupper(mode) == "TEST") {
        return("Testing: passed")
    } else {
    iid <- new_import_record(con)

        #replace NA with "" in text_value
        if (is.na(text_value)) {text_value <- "" }

        dev_id <- dbGetQuery(con, paste0("SELECT device_id FROM ",
					"devices_sensors WHERE sensor_id = '", sen_id, "'"))

        #make data frame with data
        val <- data.frame(accuracy = as.numeric(sen$accuracy),
                          precision = as.numeric(sen$precision),
                          numeric_value = numeric_value,
                          height_min_metres = height_min_metres,
                          height_max_metres = height_max_metres,
                          elevation_in_metres = loc$elevation_in_metres,
                          device_id  = dev_id$device_id,
                          sensor_id  = sen$id,
                          import_id  = iid$id,
                          import_key = "R-script",
                          observation_type = sen$type_of_measurement,
                          unit_of_measure  = sen$unit_of_measurement,
                          text_value = as.character(text_value),
                          logged_time = strftime(time_UTC, f, usetz = FALSE),
                          corrected_utc_time = strftime(time_UTC, f,
                                                    usetz = FALSE),
                          location = as.character(loc$coordinates),
                          stringsAsFactors = FALSE)

        # import
        valstring <- paste0(paste(val[1, 1:6], collapse = ", "), ", '",
                            paste(val[1, 7:16], collapse = "', '"))
        valstring <- gsub(", NA,", ", 'NaN',", valstring)
        q <- paste0("INSERT INTO observations (",
                          paste(names(val), collapse = ", "), ") ",
                    "VALUES (", valstring, "') ",
                    "RETURNING id")
        dbExecute(con, q)
        return("Inserted")
    }
}
