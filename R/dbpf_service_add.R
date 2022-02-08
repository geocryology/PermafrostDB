# =============================================================================
#'
#' @title Add service or repair need to location
#'
#' @description 
#' 
#' @details 
#'
#' @param con Database connection object, as returned by dbpf_con()
#' 
#' @param location_name location name used to obtain location ID [character]
#' 
#' @param service_text Text describing the repair or service item needed [character]
#' 
#' @param time_UTC time of observation [POSIXct]
#'  
#' @param mode Should data be inserted into DB? Defaults to 'test' so that 
#'             only testing information is returned. To insert: 'insert'
#' 
#' @export
#'
#'                  
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

dbpf_service_add <- function(con, location_name, service_text, time_UTC, mode='test'){

  dbpf_manual_obs_add(con, 
                      sensor_label = 'service_required',
                      time_UTC = time_UTC, 
                      location_name = location_name, text_value = service_text,
                      height_min_metres = 0,
                      height_max_metres = 0, 
                      mode = mode, 
                      numeric_value = NULL
                      )
}



