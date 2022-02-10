# =============================================================================
#'
#' @title Add maintenance item
#'
#' @description Record maintenance or service issue at a location
#'
#' @details Adds an observation to using the 'maintenance_required' sensor
#' for a particular location. A description of what maintenance is needed should
#' be provided, and should include: who is reporting the issue, what is needed,
#' why it is needed, and a description of where the problem is, if the location is
#' not specific enough.
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#'
#' @param location_name location name used to obtain location ID [character]
#'
#' @param maintenance_text Text describing the repair or maintenance item needed [character]
#'
#' @param time_UTC (optional) When the issue was discovered. If not provided
#' the current time is used. [POSIXct]
#'
#' @param mode Should data be inserted into DB? Defaults to 'test' so that
#'             only testing information is returned. To insert: 'insert'
#'
#' @export
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

dbpf_maintenance_add <- function(con, location_name, maintenance_text, time_UTC, mode='test'){

  if (missing(time_UTC)){
    time_UTC <- lubridate::now()
  }

  dbpf_manual_obs_add(con,
                      sensor_label = 'maintenance_required',
                      time_UTC = time_UTC,
                      location_name = location_name,
                      text_value = maintenance_text,
                      height_min_metres = 0,
                      height_max_metres = 0,
                      mode = mode,
                      numeric_value = NA
                      )
}



