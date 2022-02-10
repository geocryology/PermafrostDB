# =============================================================================
#'
#' @title Adds new devices_locations to database
#'
#' @description Inserts a data frame of new devices_locations into database.
#'
#' @details Devices_locations indicate the time when a device was put into
#'          a specific location. Until assigned to a new location, the
#'          device is then interpreted to be at that location. Insertion
#'          is done one row at a time and feedback is provided.
#'          The default mode (mode='test') only tests the data to be
#'          inserted. Insert mode (mode='insert') requires a database
#'          onnection generated with a login that has insert priviledge.
#'          In Test Mode, duplicate rows in the input data are identified.
#'          Locations and devices listed are tested for existance in the DB.
#'          The input data frame is retuned with three new columns containing
#'          test results.
#'
#' @param dev_loc Data frame with these columns (type):
#'                serialnumber (character)
#'                comment (character)
#'                sitename (character)
#'                time (POSIXct, UTC)
#'
#' @param mode Can be 'test' (default) or
#'             'insert' (requires login with insert previledges)
#'
#' @param con DB connection as returned by dbpf_con(). For insert priviledges
#'            specific user name and password need to be supplied.
#'
#' @return Original data frame with a new column 'inserted' indicating which
#'         rows were inserted. Column 'dup' indicates row for which duplicates
#'         were found and deleted, column 'dev' indicates whether a device
#'         with that serial number exits in the DB, column 'loc' indicates
#'         whether a location with that name exits in the DB. Also a column
#'         'db_dup' if an exact duplicate row exists in the database. All five
#'         new columns are Boolean (True/False or 1/0).
#'
#' @export
#' @examples
#' \dontrun{
#' con    <- dbpf_con()
#' devices_locations <- data.frame(serialnumber="E50DBD",
#'                                 comment="example",
#'                                 sitename="field_site_1",
#'                                 time=now())
#' result <- dbpf_devices_locations_add(con, devices_locations, mode="test")
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_devices_locations_add <- function(con, dev_loc, mode="test") {

    #test mode
    test_mo <- (mode == "test") + (mode == "insert")
    if (test_mo != 1) {
    	stop(paste("Parameter 'mode' must be either",
                   "'test' or 'insert'."))
    }

    #test information provided
    dev_loc <- subset(dev_loc, select = c(serialnumber, comment, sitename, time))

    #fix/test column data type, add check columns
    dev_loc$serialnumber <- as.character(dev_loc$serialnumber)
    dev_loc$sitename     <- as.character(dev_loc$sitename)
    dev_loc$comment      <- as.character(dev_loc$comment)
    if (is.POSIXct(dev_loc$time) == FALSE) {
    	stop("Column 'time' must be in POSIXct")
    }
    dev_loc$dev <- FALSE
    dev_loc$loc <- FALSE
    dev_loc$dup <- FALSE
    dev_loc$inserted <- FALSE

    # make time string for postgresql "2015-06-14 15:24:00+00"
    dev_loc$strtime <- substr(format(dev_loc$time, format="%Y-%m-%d %H:%M:%S%z"), 1, 22)

    #check for duplicates
    nr <- nrow(dev_loc)
    dev_loc$dup <- (duplicated(dev_loc) + duplicated(dev_loc, fromLast = TRUE)) != 0
    dev_loc <- unique(dev_loc)
    if ((nr - nrow(dev_loc)) > 0) {
    	print(paste(nr - nrow(dev_loc), " duplicate rows removed."))
    }


    #loop over rows in table
    for (r in 1:nrow(dev_loc)) {
    	dev_loc$dev[r] <- dbpf_device_exists(con, dev_loc$serialnumber[r]) == 1
    	dev_loc$loc[r] <- dbpf_location_name_exists(con, dev_loc$sitename[r]) == 1

        #feedback
    	print(paste("Processing:", dev_loc$sitename[r],
    	            dev_loc$serialnumber[r], dev_loc$strtime[r],
    	            "(row", r, "of", nrow(dev_loc), ")"))

    	#work on records that have existing dev and loc
    	if (dev_loc$dev[r] + dev_loc$loc[r] == 2) {
    		dev_id <- dbGetQuery(con, paste0("SELECT id
                                                FROM devices
                                               WHERE serial_number ='", dev_loc$serialnumber[r],"'"))

            loc_id <- dbGetQuery(con, paste0("SELECT id
                                                FROM locations
                                               WHERE name ='", dev_loc$sitename[r],"'"))


            query <- paste0("INSERT INTO devices_locations",
    	                "(timestamp, device_id, location_id, notes) VALUES ('",
    	                dev_loc$strtime[r], "', '", dev_id, "', '", loc_id, "', '",
    	                dev_loc$comment[r],"');")

            # check for duplicates in database
            qry <- dbGetQuery(con,
            paste0("SELECT devices_locations.id
                      FROM devices
                           INNER JOIN devices_locations ON devices_locations.device_id = devices.id
                           INNER JOIN locations on devices_locations.location_id = locations.id
                     WHERE devices.serial_number = '", dev_loc$serialnumber[r],"'
                           AND locations.name = '",    dev_loc$sitename[r],"'
                           AND devices_locations.timestamp = '",dev_loc$time[r],"'"))
            dev_loc$db_dup[r] <- nrow(qry) != 0

            if (dev_loc$db_dup[r]){
                print(paste0("Failed test: duplicate exists in database (id = ",qry$id,")"))
                next
            }

    	    if (mode == 'test') {
    	    	print(paste("  --> passed testing"))
    	    } else {
    	    	try({res <- dbSendQuery(con, query)}, silent = TRUE)
                try(dbClearResult(res))
    	    	dev_loc$inserted[r] <- TRUE
    	    	print(paste("  --> inserted into DB"))
    	    }
    	}
    }

    #finish
    return(dev_loc)
    print("=== FINISHED: dbpf_devices_locations_add() ===")
}




