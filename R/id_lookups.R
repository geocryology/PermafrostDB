
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


horizon_angles_id <- function(con, azimuth){
  
  if (missing(azimuth)){
    azimuth <- seq(0, 359, 1)
  }
  
  azimuth <- sprintf("%.3i", as.numeric(unique(azimuth)))
  labels <- paste0("horizon_angle_at_",azimuth,"_true")
  
  sensors <- dbGetQuery(con, paste0("
                               SELECT id, label
                                 FROM sensors
                                WHERE sensors.label IN ('", paste(labels, collapse="','"), "')"))
  
  if (length(azimuth) < length(sensors$id)){
    stop("duplicate sensors detected")
  }
  
  sorted_sensors <- sensors[match(labels, sensors$label),]
  
  return(sorted_sensors)
  
}

