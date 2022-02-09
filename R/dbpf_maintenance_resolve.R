# =============================================================================
#'
#' @title Resolve maintenance issue
#'
#' @description Mark a maintenance item as 'complete'
#' 
#' @details Adds an observation to using the 'maintenance_completed' sensor 
#' for a particular location. The observation references the maintenance 
#' issue using the database ID of that issue.
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#' 
#' @param id Database ID of the maintenance item to resolve. Can be found using \code{\link{dbpf_maintenance_needs}} [character]
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
dbpf_maintenance_resolve <- function(con, id, time_UTC, mode='test'){
  
  if (missing(time_UTC)){
    time_UTC <- now()
  }
  
  # check if id exists
  needs <- 
    paste0("SELECT obs.id as maintenance_need,
                   locations.name as loc
              FROM (SELECT id, location
                      FROM observations 
                     WHERE sensor_id = '",maintenance_required_id(con),"') 
                   AS obs
                   LEFT JOIN locations
                   ON locations.coordinates = obs.location
             WHERE obs.id='",id,"'
            ")
  needs <- dbGetQuery(con, needs)
  
  completed <- 
    paste0("SELECT observations.text_value as maintenance_id,
                   corrected_utc_time
              FROM observations 
             WHERE sensor_id = '",maintenance_completed_id(con),"'
            ")
  completed <- dbGetQuery(con, completed)
  
  if (!(id %in% needs$maintenance_need)){
    stop("No maintenance record with that ID")
  }
  
  if (id %in% completed$maintenance_id){
    stop(paste("maintenance need already resolved", completed$corrected_utc_time))
  }
  
  res <- dbpf_manual_obs_add(con, 
                             sensor_label = 'maintenance_completed',
                             time_UTC = time_UTC, 
                             location_name = needs$loc, 
                             text_value = id,
                             height_min_metres = 0,
                             height_max_metres = 0, 
                             mode = mode, 
                             numeric_value = 0
  )
  
  return(res)
  
}