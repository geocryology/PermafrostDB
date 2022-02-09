
service_required_id <- function(con){
  #TODO:: memoize this
  dbGetQuery(con, "
             SELECT id 
               FROM sensors 
              WHERE label='maintenance_required'")
}


service_completed_id <- function(con){
  #TODO:: memoize this
  dbGetQuery(con, "
             SELECT id
               FROM sensors
              WHERE label='maintenance_completed'")
}


human_observation_id <- function(con){
  #TODO:: memoize this
  dbGetQuery(con, "
             SELECT id
               FROM devices
              WHERE device_type='human observation'")
}
