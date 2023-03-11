# =============================================================================
#'
#' @title De-fox data in bulk, as identified by location and time.
#'
#' @description Treatment of subsurface logger that have been exposed at the
#'              terrain surface. As we suspect that animals (foxes) yanked
#'              loggers out of the ground, we call this de-foxing.
#'
#' @details The observations identified (1) are added to a set that
#'          identifies them as bulk-defoxed and (2) have their height set to
#'          0 m as the sensors are exposed on the ground surface. A check on
#'          device ID is performed: Only one device ID can be present at that
#'          location during the interval de-foxed. The installation of a new
#'          sensor would have ocurred at the correct depth. If a sensor chain
#'          is present at the location, all sensors are assumed to be pulled
#'          out. The set used is called "Exposed Temperature Sensor". A further
#'          check is performed on table observations_sets to detect if some of
#'          the observations have already been treated.
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location_name Character string or list of several with the location
#'                      name(s) to be queried for.
#'
#' @param unit_of_measurement Unit of measurments, defaults to "C"
#'
#'
#' @param time_b Begin time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @param time_e End time for the interval to be analysed. Use the format
#'               "1950-01-01 00:00:00+00"
#'
#' @export
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' dbpf_defox_bulk(con,'NGO-DD-1004_ST02', "2016-01-01 00:00:00+00","2016-01-10 23:59:00+00")
#' dbDisconnect(con)
#' }
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_defox_bulk <- function(con, location_name, time_b, time_e,
                            unit_of_measurement = "C") {

    # check for the number of devices during period, first build WHERE clause
    nwc <- paste0("observations.corrected_utc_time BETWEEN '", time_b, "' AND '", time_e, "' AND ",
                  "observations.unit_of_measure = '", unit_of_measurement, "' AND ",
                  "observations.location = (SELECT coordinates FROM locations ",
                  "WHERE name = '", location_name ,"')")

    qry <- paste0("SELECT COUNT (DISTINCT device_id) AS dev_count, ",
                  "COUNT (DISTINCT sensor_id) AS sen_count,",
                  "COUNT (DISTINCT id) AS obs_count FROM observations ",
                  "WHERE ", nwc)
    stat <- dbGetQuery(con, qry)

    # error if more or less than one device
    if (stat$dev_count > 1) {
    	stop("More than one device found, de-foxing interrupted")
    } else if (stat$dev_count < 1) {
    	stop("No device/data found, de-foxing interrupted")
    }

    # check if observations are already in observations_sets
    qset <- paste0("SELECT COUNT (DISTINCT observations.id) FROM observations INNER JOIN ",
                  "observations_sets ON observations.id = observations_sets.observation_id ",
                  "WHERE observations_sets.set_id = (SELECT id FROM sets WHERE label = 'Exposed Temperature Sensor') AND ", nwc)
    nset <- dbGetQuery(con, qset)$count
    if (nset > 0) {stop("One or more of these observations are already in set")}

    #feedback
    print(paste("==>", stat$sen_count, "sensors,", stat$obs_count, "observations."))

    #--- START TRANSACTION ----
    dbBegin(con)

    # add changes observations to table observations_sets
    #set_id <- dbGetQuery(con, "(SELECT id FROM setS WHERE label = 'Exposed Temperature Sensor')")
    #oid <- paste0("(SELECT id FROM observations WHERE ", nwc, ")")
    qry <- paste0("INSERT INTO observations_sets (observation_id, set_id) ",
                  "SELECT id AS observation_id, (SELECT id FROM sets WHERE label = 'Exposed Temperature Sensor') AS set_id FROM observations WHERE ", nwc, ";")
    # == update observations_dois
  	ncd <- dbExecute(con, qry)

  	# == update observations table
  	qry <- paste0("UPDATE observations SET height_min_metres = 0, ",
  	              "height_max_metres = 0 WHERE ", nwc ,";")
  	nco <- dbExecute(con, qry)

  	# Make another safety check: the numer of dois found now mjust equal the
  	# number of observations that had to be changed. If this is not true, the
  	# transaction will be rolled back.
  	check <- stat$obs_count - dbGetQuery(con, qset)$count
  	if (check == 0) {
  		dbCommit(con)
  		message("OK, defoxed.")
  	} else {
  		dbRollback(con)
  		message("Final check NOT PASSED, transaction rolled back.")
  	}
}
