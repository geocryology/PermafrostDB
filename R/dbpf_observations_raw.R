# =============================================================================
#'
#' @title Get raw observations
#'
#' @description Get raw observations per location, height and time slice.
#'
#' @details Fast way to access raw observation data without aggregation.
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character string or list of several with the location
#'                      name(s) to be queried for.
#'
#' @param unit_of_measurement Unit of measurments, defaults to "C"
#'
#' @param height_top Uppermost height [m] (positive above surface, negative when 
#'                   below surface) of sensor to return in this query.
#'
#' @param height_bot Lowermost height [m] (positive above surface, negative when 
#'                   below surface) of sensor to return in this query.
#'
#' @param time_b Begin time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param time_e End time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param verbose Provide terminal output of the query string? (defaults to FALSE)
#'
#' @return List of all locations in DB 
#' 
#' @export
#' @examples
#' con <- dbpf_con() 
#' obs <- dbpf_observations_raw(con, "NGO-DD-1004_ST02")
#' dbDisconnect(con)
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_observations_raw <- function(con, location_name, unit_of_measurement = "C",
                                  height_top = -0.1, 
                                  height_bot = -0.1,
                                  time_b = "1950-01-01 00:00:00+00", 
                                  time_e = "2050-01-01 00:00:00+00",
                                  verbose = FALSE) {
	
	#construct query
	q <- paste0("SELECT observations.id, observations.height_min_metres AS height, ",
	            "locations.name AS loc_name, corrected_utc_time AT TIME ZONE 'UTC' AS time, ",
	             "observations.numeric_value AS value ",
	             "FROM observations INNER JOIN ",
	             "locations ON observations.location = locations.coordinates ",
	             "WHERE observations.corrected_utc_time BETWEEN ",
	             "'", time_b, "' AND '", time_e, "' AND ",
	             "observations.height_min_metres >= ", height_bot, " AND ",
	             "observations.height_max_metres <= ", height_top, " AND ",
	             "observations.unit_of_measure = '", unit_of_measurement, "' AND ",
	             "locations.name = ANY('{", paste(location_name, collapse=", ") ,"}'::text[])",
		           "ORDER BY time ASC, height DESC;")
	
	# feedback
	if (verbose == TRUE) {
		print("=== SQL string sent ===")
		print(q)
	}

	#get data
	data <- dbGetQuery(con, q)
	
	#handle time
	data$time <- as.POSIXct(data$time)
	
	#return result
	return(data)
}
