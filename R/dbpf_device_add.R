# =============================================================================
#'
#' @title Adds a new device to DB
#'
#' @description Adds a new device to DB and tests for duplicate device_type and
#'              serial_number.
#'
#' @details Run in test mode first. If you have no DB login to write data, run
#'          in test mode with your login and then pass to someone who does.
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#' 
#' @param device_type Charcter string decribing type of device
#'
#' @param notes Notes for the device.
#'
#' @param serial_number Serial number, must be unique
#'
#' @param access_code Access code (used with Geoprecision loggers)
#'
#' @param manufacturer Manufacturer.
#'
#' @param manufacturer_device_name Device type as used by manufacturer.
#'
#' @param acquired_on Date when device was purchased (format: "1950-01-01 00:00:00+00")
#'
#' @param mode = Can be 'test' (default, read-only) or 'insert' (will insert into DB)
#'
#' @return Test results as well as device id if data was inserted.
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' dbpf_device_add(con, "MyDev", "Test", serial_number = "Test", access_code = "",
#'                 manufacturer = "generic device", manufacturer_device_name = "",
#'                 acquired_on = "1950-01-01 00:00:00+00", mode = 'test')
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_device_add <- function(con, device_type, notes, serial_number = "",
                            access_code = "", manufacturer = "generic device",
                            manufacturer_device_name = "",
                            acquired_on = "1950-01-01 00:00:00+00",
                            mode = 'test') {
    # === RUN TESTS
    # initial string
    teststring <- "Test result"
    passed <- FALSE

    # check how many devices of this type exists
    query <- paste0("SELECT serial_number FROM devices WHERE device_type = '", device_type ,"'")
    res <- dbGetQuery(con, query)
    if (length(res$serial_number) > 0) {
    	teststring <- paste(teststring, "device(s) with equal device_type found", sep = ": ")
    }

    # check if devices_type and serial_nunmber exist
    if (sum(res$serial_number == serial_number) > 0) {
    	teststring <- paste(teststring, "serial number already exists with the same device_type", sep = ": ")
    }

    if (teststring == "Test result") {
    	teststring <- "Test result: OK"
    	passed <- TRUE
    }

    # === INSERT
    if ((mode == 'insert') * (passed == TRUE)) {
    	query <- paste0("INSERT INTO devices (device_type, notes, serial_number, ",
    	                "access_code, manufacturer, manufacturer_device_name, ",
    	                "acquired_on) VALUES ('", paste(device_type, notes,
    	                serial_number, access_code, manufacturer,
    	                manufacturer_device_name, acquired_on, sep="', '"), "') RETURNING id")
    	try(ins <- dbGetQuery(con, query), silent = TRUE)
    	if (exists("ins")) {
    		teststring <- paste0(teststring, " ==> Row inserted. Resulting device id: ", ins)
    	}
    }

    #return result
    return(teststring)
}