# =============================================================================
#'
#' @title Manual observations at location
#'
#' @description Downloads manual observations from the database
#'
#' @details Filters observations by those that have 'human observation' as the
#' device, and then only returns those corresponding to the selected location
#'
#' @param con Database connection object, as returned by dbpf_con()
#'
#' @param location A character vector of location names
#'
#' @param sensor_label (optional) character, one or more names corresponding to
#'  sensor labels such as 'veg_species_1' or 'LAI' to return.
#'   If omitted, all sensor types will be returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbpf_con()
#' dbpf_manual_obs_by_location(con, location = c("NGO-GU-1014_ST01"))
#' dbpf_manual_obs_by_location(con, location = c("NGO-GU-1014_ST01"),
#' sensor_label = c('veg_species_1', 'LAI'))
#' }
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_manual_obs_by_location <- function(con, location, sensor_label){
  
  manual_device = human_observation_id(con)
  
  if (length(location) == 1){
    location_clause = paste0(" '", location, "'")
  } else if (length(location) > 1){
    location_clause = paste0(" ANY('{",paste(sensor_label, collapse = ', '),"}'::text[]")
  } else {
    stop("location argument must have length at least 1")
  }
  
  if (missing(sensor_label)){
    sensor_clause = ""
  } else {
    sensor_clause = paste0("WHERE manual_sensors.label = ANY('{",paste(sensor_label, collapse = ', '),"}'::text[])")
  }
  
  q <- paste0(
    "SELECT intersecting_locations.name AS loc_name,
          label,
          observations.height_max_metres AS from,
          observations.height_min_metres AS to,
          text_value,
          numeric_value,
          unit_of_measurement,
          type_of_measurement,
          observations.corrected_utc_time AS time,
          ST_X(ST_Centroid(observations.location)) AS lon,
          ST_Y(ST_Centroid(observations.location)) AS lat,
          ST_GeometryType(observations.location) AS geom
          
     FROM observations 
          
          RIGHT JOIN (SELECT sensors.*, timestamp
                       FROM sensors
                            INNER JOIN devices_sensors ON devices_sensors.sensor_id = sensors.id
                            INNER JOIN devices ON devices.id = devices_sensors.device_id
                      WHERE devices.id = '", manual_device, "') AS manual_sensors
          
          ON observations.sensor_id = manual_sensors.id
          
          JOIN (SELECT locations.coordinates as crds,
                       locations.name as name
                  FROM locations 
                       JOIN (SELECT * 
                               FROM locations 
                              WHERE locations.name = ", location_clause, ") AS loc 
                       ON ST_Intersects(locations.coordinates, loc.coordinates)) AS intersecting_locations 
                  
          ON observations.location = intersecting_locations.crds
          ", sensor_clause, "")

  res <- dbGetQuery(con, q)
  
  return(res)
}



