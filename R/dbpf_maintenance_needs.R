# =============================================================================
#'
#' @title List maintenance needs
#'
#' @description Display maintenance or service issues at a location
#' 
#' @details Adds an observation to using the 'maintenance_required' sensor 
#' for a particular location. A description of what maintenance is needed should
#' be provided, and should include: who is reporting the issue, what is needed, 
#' why it is needed, and a description of where the problem is, if the location is 
#' not specific enough.
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#' 
#' @param location_name Name of the location at maintenance needs should be returned. [character]
#' 
#' @param include_completed Whether or not to include maintenance items that have been
#' resolved. [logical]
#' 
#' @export
#'                  
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_maintenance_needs <- function(con, location_name, include_completed=FALSE){
  
  all <- dbpf_manual_obs_by_location(con,
                                     location_name, 
                                     "maintenance_required")
  
  completed <- dbpf_manual_obs_by_location(con,
                                           location_name,
                                           "maintenance_completed")
  
  all$resolved <- sapply(all$id, 
                         function(id) {id %in% completed$text_value})
  
  if (!include_completed){
    all <- all[!all$resolved, ]
  }
 
  all <- all[, c("text_value", 
                 "name", 
                 "corrected_utc_time",
                 "lon", 
                 "lat", 
                 "id", 
                 "resolved")]
  
  names(all) <- c("service",
                  "location", 
                  "recorded", 
                  "lon",
                  "lat", 
                  "id", 
                  "resolved")
  
  return(all)
  
}