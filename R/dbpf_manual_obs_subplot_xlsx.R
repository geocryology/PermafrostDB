# =============================================================================
#'
#' @title Import subplot data and manual observations from XLS file into DB
#'
#' @description Tests and optionally inserts manual observations into DB.
#'
#' @details Data is checked for existence (or duplication) of sensor and
#'          location and feedback is returned.
#'
#' @param con Database connection object, as returned by dbpf_con()
#' @param file_xlsx file name of the xlsx workbook to be imported [character]
#' @param mode Should data be inserted into DB? Defaults to 'test' so that
#'             only testing information is returned. To insert: 'insert'
#' @param dbformat Defaults to FALSE for reading complete sub-plot xls file.
#'                 When set to TRUE, it reads only a data sheet named
#'                 DB_format_manual_observation and ingests the data found there.
#'
#'
#' @export
#' @examples
#' \dontrun{
#' con  <- dbpf_con()
#' data <- dbpf_manual_obs_subplot_xlsx(con, file_xlsx)
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================
dbpf_manual_obs_subplot_xlsx <- function(con, file_xlsx, mode = "test", dbformat = FALSE) {
    # read data
    obs <- XLS_DB_observations_read(file_xlsx)

    # test location and insert if it does not exist
    if (dbformat == FALSE) {
    	loc    <- XLS_DB_location_read(file_xlsx)
    	loc.ok <- dbpf_location_report(con, loc$name, distance=10)
    } else {
    	loc.ok <- TRUE
    }

    if (loc.ok == FALSE) {
    	# does not exist and no data given
    	if (ncol(loc) == 1) {
    		stop(paste("Location in sheet DB_format_location does not exist and",
    	    	           "insufficient data for creating it found in file", file_xlsx))
    	}

    	# does not exist but information given, add location
    	result <- dbpf_locations_add(con, loc, mode=mode)
    	print(result)
    }

    # test or insert data line by line
    for (r in 1:nrow(obs)) {
    	result <- dbpf_manual_obs_add(con, obs[r,'sensor_label'],
    	                                  obs[r,'location_name'],
    	                                  obs[r,'time_UTC'],
    	                                  obs[r,'numeric_value'],
    	                                  obs[r,'text_value'],
    	                                  obs[r,'height_min_metres'],
    	                                  obs[r,'height_max_metres'],
    	                                  mode = mode)
    	print(result)
    }
}


# helper function to read and check the sheets DB_format_location
XLS_DB_location_read <- function(file_xlsx) {
    # read data
    data <- openxlsx::read.xlsx(file_xlsx, sheet = "DB_format_location", colNames = TRUE)

    # test formalities
    if (length(names(data)) != 7) {
    	stop("Exactly seven columns needed in DB_format_location")
    }
    cols <- c("name","lat","lon","elevation_in_metres","accuracy_in_metres","comment","record_observations")
    if (sum(cols %in% names(data)) != 7) {
    	stop(paste("Check columns, needed:", paste(cols, collapse=", ")))
    }

    # convert and test content
    data$record_observations <- as.logical(toupper(as.character(data$record_observations)))
    data$lat <- as.numeric(data$lat)
    data$lon <- as.numeric(data$lon)
    data$elevation_in_metres <- as.numeric(data$elevation_in_metres)
    data$accuracy_in_metres <- as.numeric(data$accuracy_in_metres)
    data$name <- as.character(data$name)
    data$comment <- as.character(data$comment)
    data$lat[data$lat == 0] <- NA
    data$lon[data$lon == 0] <- NA

    # testing
    ok.rows <- nrow(data) == 1
    ok.name <- data$name != ""
    ok.data <- sum(is.na(c(data$lat, data$lon, data$elevation_in_metres,
                   data$accuracy_in_metres))) == 0

    if (ok.rows == FALSE) {
    	stop(paste("There are no or too many valid data rows in sheet DB_format_location of file", file_xlsx))
    }

    if (ok.name == FALSE) {
    	stop(paste("No location name in sheet DB_format_location of file", file_xlsx))
    }

    if (ok.data == FALSE) {
    	print(paste("No location data, but location name present.",
    	            "Looking for existing location while importing file", file_xlsx))
    	data <- subset(data, select="name")
    }
    return(data)
}



# helper function to read and check the sheets DB_format_manual_observation
XLS_DB_observations_read <- function(file_xlsx) {
    # read data
    data <- openxlsx::read.xlsx(file_xlsx, sheet = "DB_format_manual_observation", colNames = TRUE)

    # test formalities
    if (length(names(data)) != 7) {
    	stop("Exactly seven columns needed in DB_format_location")
    }
    cols <- c("sensor_label", "location_name", "time_UTC",
              "numeric_value", "text_value", "height_min_metres",
              "height_max_metres")
    if (sum(cols %in% names(data)) != 7) {
    	stop(paste("Check columns, needed:", paste(cols, collapse=", ")))
    }

    # convert and test content
    nrow_beg <- nrow(data)
    data$location_name <- as.character(data$location_name)
    data$sensor_label  <- as.character(data$sensor_label)
    data$text_value  <- as.character(data$text_value)
    data$numeric_value <- as.numeric(data$numeric_value)
    data$height_min_metres <- as.numeric(data$height_min_metres)
    data$height_max_metres <- as.numeric(data$height_max_metres)
    data$time_UTC <- as.POSIXct(data$time_UTC*86400 + as.POSIXct("1899-12-30"))
    rowmask <- is.na(data$text_value) * is.na(data$numeric_value)
    data <- data[rowmask == 0,] # drop rows without values
    rowmask <- is.na(data$sensor_label) + is.na(data$location_name) +
               is.na(data$time_UTC) + is.na(data$height_min_metres) +
               is.na(data$height_min_metres)
    data <- data[rowmask == 0,] # drop rows without values
    if (nrow(data) == 0) {
    	stop(paste("No valid data rows in sheet DB_format_location of file", file_xlsx))
    }
    print(paste("Using", nrow(data), "rows of data (original rows:", nrow_beg,")."))
    return(data)
}
