dbpf_service_required <- function(con, location_name, include_completed=FALSE){
  
  all <- dbpf_manual_obs_by_location(con, location_name, "service_required")
  completed <- dbpf_manual_obs_by_location(con, location_name, "service_completed")
  all$resolved <- sapply(all$id, function(id) {id %in% completed$text_value})
  
  if (!include_completed){
    all <- all[!all$resolved,]
  }
  all <- all[,c("text_value", "name", "corrected_utc_time", "lon", "lat", "id", "resolved")]
  names(all) <- c("service", "location", "recorded", "lon", "lat", "id", "resolved")
  return(all)
  
}