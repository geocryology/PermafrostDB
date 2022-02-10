# =============================================================================
#'
#' @title Get horizon lines
#'
#' @description Get a collection of (azimuth, horizon) measurements for a location.
#'
#' @details Adds a collection of horizon measurements (i.e. (azimuth, horizon) pairs)
#' to the database for a particular location.
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#'
#' @param location Name of a single location at which the horizon measurements were taken. [character]
#'
#' @return a dataframe with columns \code{azimuth} and \code{horizon} [data.frame]
#'
#' @export
#'
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_horizon_get <- function(con, location){

  sen <- horizon_angle_sensors(con)

  q <- paste0("SELECT sensors.label as azimuth,
                      obs.numeric_value as horizon
                 FROM (SELECT sensor_id, location, numeric_value
                       FROM observations
                       WHERE sensor_id IN ('", paste(sen$id, collapse="','"), "')
                       ) AS obs

                      INNER JOIN sensors ON sensors.id = obs.sensor_id
                      INNER JOIN locations ON locations.coordinates = obs.location
                WHERE obs.sensor_id IN ('", paste(sen$id, collapse="','"), "')
                  AND locations.name = '",location,"'")

  result <- dbGetQuery(con, q)

  result$azimuth <- as.numeric(substr(result$azimuth, 18,20))

  return(result)

}
