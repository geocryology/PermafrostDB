
dbpf_maintenance_resolve <- function(con, id, time_UTC, mode='test'){
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