# =============================================================================
#'
#' @title Return statistics about observations in DB
#'
#' @description Provides mean min max and count for a certain period and one
#'              or several locations.
#'
#' @details These simple functions return all data as data frames. When
#'          making a query many times, optimise the SQL statement to only
#'          request the data you actually need.
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character string or list of several with the location
#'                      name(s) to be queried for.
#'
#' @param unit_of_measurement Unit of measurments, defaults to "C"
#'
#' @param time_b Begin time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param time_e End time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param verbose Provide terminal output of the query string? (defaults to FALSE)
#'
#' @return Data frame with locations in rows and columns loc_name, height,
#'         max, min, avg, cnt
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' stat <- dbpf_observations_stats(con, "NGO-RC-163")
#' dbDisconnect(con)
#' }
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_observations_stats <- function(con, location_name, unit_of_measurement = "C",
                                    time_b = "2015-09-01 00:00:00+00",
                                    time_e = "2016-08-31 23:59:59+00",
                                    verbose = FALSE) {

    # make query
    q <- paste0("SELECT locations.name AS loc_name, ",
                "observations.height_min_metres AS height, ",
                "MAX(observations.numeric_value) AS max, ",
                "MIN(observations.numeric_value) AS min, ",
                "AVG(observations.numeric_value) as avg, ",
                "COUNT(observations.numeric_value) as cnt ",
                "FROM observations INNER JOIN ",
                "locations ON observations.location = locations.coordinates ",
                "WHERE observations.corrected_utc_time BETWEEN ",
                "'", time_b, "' AND '", time_e, "' AND ",
                "locations.name = ANY('{", paste(location_name, collapse=", ") ,"}'::text[]) ",
                "AND observations.unit_of_measure = '", unit_of_measurement, "' ",
                "GROUP BY observations.height_min_metres, locations.name ",
                "ORDER BY loc_name ASC, height DESC;")

    #query
    if (verbose == TRUE) {
    	print("=== Query sent:")
    	print(q)
    }
    obs_stat <- dbGetQuery(con, q)

    #return result
    return(obs_stat)
}
