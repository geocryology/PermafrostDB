
maintenance_required_id <- function(con){
  #TODO:: memoize this
  DBI::dbGetQuery(con, "
             SELECT id
               FROM sensors
              WHERE label='maintenance_required'")
}


maintenance_completed_id <- function(con){
  #TODO:: memoize this
  DBI::dbGetQuery(con, "
             SELECT id
               FROM sensors
              WHERE label='maintenance_completed'")
}


human_observation_id <- function(con){
  #TODO:: memoize this
  DBI::dbGetQuery(con, "
             SELECT id
               FROM devices
              WHERE device_type='human observation'")
}

no_sensor_id <- function(con) {
  DBI::dbGetQuery(con, "SELECT id FROM sensors WHERE label = 'no_sensors'")
}



