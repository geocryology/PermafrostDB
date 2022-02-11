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
#' @param location A vector of either location names (character) or a bounding
#' box (numeric) of the form (xmin, xmax, ymin, ymax)
#'
#' @param sensor_label (optional) character, one or more names corresponding to
#'  sensor labels such as 'veg_species_1' or 'LAI' to return.
#'   If omitted, all sensor types will be returned.
#'
#' @param point_only logical, whether to include only measurements that are. If
#' False, also returns observations that apply to any plots that the
#'
#' @export
#'
#' @examples
#' dbpf_manual_obs_by_location(location_name = c("NGO-GU-1014_ST01"))
#' dbpf_manual_obs_by_location(location_name = c("NGO-GU-1014_ST01"),
#'   sensor_label = c('veg_species_1', 'LAI'))
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_manual_obs_by_location <- function(con, location, sensor_label,
                                        point_only=F){
  if (missing(con)){
    con <- dbpf_con()
  }

  if (missing(sensor_label)){
    sensor_label <- dbGetQuery(con, "SELECT DISTINCT label FROM sensors
        WHERE sensors.device_id = '5265f740-8e09-4d41-b5b6-2136f5d35ea3'")$label
  }
  return_cols <- "sensors.label, sensors.unit_of_measurement,
        sensors.height_in_metres,
observations.height_max_metres, observations.height_min_metres,
sensors.type_of_measurement,
        observations.numeric_value, observations.text_value,
  observations.corrected_utc_time,
  ST_X(ST_Centroid(observations.location)) AS lon, ST_Y(ST_Centroid(observations.location)) AS lat,
  locations.name,  ST_GeometryType(observations.location) AS geom,
  measurement_site, observations.id"

  if (class(location)=='character'){  # if searching by names
    query <- paste0(
      "
      SELECT ",return_cols,"
        FROM observations
             INNER JOIN sensors
             ON sensors.id = observations.sensor_id

             JOIN locations
             ON ST_Intersects(locations.coordinates, observations.location)

             LEFT JOIN
             (SELECT locations.name AS measurement_site, locations.coordinates AS coordinates
                FROM locations) AS obs
             ON observations.location = obs.coordinates

      WHERE observations.device_id = '5265f740-8e09-4d41-b5b6-2136f5d35ea3'
      AND sensors.label = ANY('{",paste(sensor_label, collapse = ', '),"}'::text[])
      AND locations.name = ANY('{",paste(location, collapse = ', '),"}'::text[])
      ")
  }else if (class(location)=='numeric'){
     pts <- paste(paste(
      location[c(1,2,2,1,1)], location[c(4,4,3,3,4)]), collapse=', ')

    query <- paste0(
      "
      SELECT ",return_cols,"

        FROM observations

             INNER JOIN sensors
             ON sensors.id = observations.sensor_id

             INNER JOIN locations
             ON locations.coordinates = observations.location

             LEFT JOIN
             (SELECT locations.name AS measurement_site, locations.coordinates AS coordinates
                FROM locations) AS obs
                  ON observations.location = obs.coordinates



       WHERE observations.device_id = '5265f740-8e09-4d41-b5b6-2136f5d35ea3'
         AND sensors.label = ANY('{",paste(sensor_label, collapse = ', '),"}'::text[])
         AND ST_Intersects(ST_SetSRID(ST_MakePolygon(
          ST_GeomFromText('LINESTRING(",pts,")')), 4326), observations.location)
             ")
  }else{
    stop("Unknown sensor_labels")
  }

  result <- DBI::dbGetQuery(con, query)
  if (point_only){
    result <- result[result$geom == 'ST_Point']
  }
  return(result)
}



