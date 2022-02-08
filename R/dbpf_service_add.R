# =============================================================================
#'
#' @title Add service or repair need to location
#'
#' @description 
#' 
#' @details 
#'
#' @param con 
#' 
#' @export
#' 
#' @examples
#'                  
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================

dbpf_service_add <- function(con, location_name, service_text, time_UTC, mode='test'){

  dbpf_manual_obs_add(con, 
                      sensor_label = 'service_required',
                      time_UTC = time_UTC, 
                      location_name = location_name, text_value = service_text,
                      height_min_metres = 0,
                      height_max_metres = 0, 
                      mode = mode, 
                      numeric_value = NULL
                      )
}

dbpf_service_required <- function(con, location_name, include_completed=FALSE){
  
  all <- dbpf_manual_obs_by_location(con, location_name, "service_required")
  completed <- dbpf_manual_obs_by_location(con, location_name, "service_completed")
  
  if (!include_completed){
    return(1)
    #all <- join(all, completed$id, all[all$id %in% ]
  }
  all <- all[,c("text_value", "name", "corrected_utc_time", "lon", "lat", "id")]
  names(all) <- c("service", "location", "recorded", "lon", "lat", "id")
  return(all)
  
}



dbpf_service_resolve <- function(con, id, time_UTC, mode='test'){
  # check if id exists
  needs <- 
    paste0("SELECT obs.id as service_need,
                   locations.name as loc
              FROM (SELECT id, location
                      FROM observations 
                     WHERE sensor_id = '",service_required_id(con),"') 
                   AS obs
                   LEFT JOIN locations
                   ON locations.coordinates = obs.location
             WHERE obs.id='",id,"'
            ")
  needs <- dbGetQuery(con, needs)

  completed <- 
    paste0("SELECT observations.text_value as service_id,
                   corrected_utc_time
              FROM observations 
             WHERE sensor_id = '",service_completed_id(con),"'
            ")
  completed <- dbGetQuery(con, completed)

  if (!(id %in% needs$service_need)){
    stop("No service record with that ID")
  }
  
  if (id %in% completed$service_id){
    stop(paste("Service need already resolved", completed$corrected_utc_time))
  }

  res <- dbpf_manual_obs_add(con, 
                      sensor_label = 'service_completed',
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

dbpf_service_resolve(con, "91bca6e0-7691-48af-aacf-03d396044adb", now())
dbpf_service_required(con, "Storage", TRUE)



"91bca6e0-7691-48af-aacf-03d396044adb"
"91bca6e0-7691-48af-aacf-03d396044adb"













dbpf_service_required(con, "Storage")

t <- now()
dbpf_service_add(con, 
                 "Storage", 
                 service_text = "Label the storage area",
                 time_UTC = t,
                 mode='insert')


dbpf_service_resolve <- function(con, id, date_UTC){
  # check if id exists
  needs <- 
    paste0("SELECT observations.text_value as service_need
              FROM observations 
             WHERE sensor_id = (SELECT id 
                                FROM sensors 
                                WHERE label='service_required')
            ")
  res <- dbGetQuery(con, needs)
  
  if (!(id %in% needs)){
    stop("No service with that ID")
  }
  
  resolved <- data.frame(sensor_id=, 
                         text_value=id, 
                         corrected_utc_time=date_UTC,)
 
 q <- paste0("INSERT INTO observations(
                    sensor_id,
                    text_value,
                    corrected_utc_time,
                    device_id,
                    observation_type,
                    logged_time,
                    location,
                    elevation_in_metres)
             VALUES (",1,")
             ")
 # dbpf_service_add(con, )
}

dbpf_service_required <- function(con, location_name){
  needs <- 
    paste0("SELECT obs.text_value as service_need,
                   locations.name as location,
                   corrected_utc_time as date,
                   ST_X(locations.coordinates) as lon,
                   ST_Y(locations.coordinates) as lat
              FROM (SELECT * 
                      FROM observations 
                     WHERE sensor_id = '",service_completed_id(),"') as obs
                    LEFT JOIN locations 
                              ON obs.location = locations.coordinates
      WHERE locations.name = ANY('{", paste(location_name, collapse=", ") ,"}'::text[]) 
    ")
  res <- dbGetQuery(con, needs)
  
  complete <- 
    paste0("SELECT obs.text_value as service_completed
              FROM (SELECT * 
                      FROM observations 
                     WHERE sensor_id = '",service_completed_id(),"') as obs
                    LEFT JOIN locations 
                              ON obs.location = locations.coordinates
      WHERE locations.name = ANY('{", paste(location_name, collapse=", ") ,"}'::text[]) 
    ")
  complete <- dbGetQuery(con, complete)
  
  return(res)
}

service_required_id <- function(con){
  #TODO:: memoize this
  dbGetQuery(con, "
             SELECT id 
               FROM sensors 
              WHERE label='service_required'")
}


service_completed_id <- function(con){
  #TODO:: memoize this
  dbGetQuery(con, "
             SELECT id
               FROM sensors
              WHERE label='service_completed'")
}
# 
# 
# {
#   if (length(service_text) > 1){
#     stop("only one at a time")
#   }
#   
#   if (mode == 'test'){
#     # ensure location exists
#     # ensure time in UTC
#   }
#   
#   if (mode == 'insert'){
#     q <- paste0("
#   INSERT INTO observations(
#                            sensor_id,
#                            text_value,
#                            corrected_utc_time,
#                            device_id,
#                            observation_type,
#                            logged_time,
#                            location,
#                            elevation_in_metres)
#        VALUES (  
#                 (SELECT id
#                    FROM sensors
#                   WHERE label = 'service_required'),
#                 '",service_text,"',
#                 '",strftime(time_UTC, "%Y-%m-%d %H:%M:%S+00", usetz = FALSE),"',
#                 (SELECT device_id
#                    FROM sensors
#                   WHERE label = 'service_required'),
#                 'text',
#                 '",strftime(time_UTC, "%Y-%m-%d %H:%M:%S+00", usetz = FALSE),"',
#                 (SELECT coordinates
#                    FROM locations
#                   WHERE name =  '",location,"'),
#                 (SELECT elevation_in_metres
#                    FROM locations
#                   WHERE name =  '",location,"')
#         )    
#     RETURNING id")
#     
#     res <- dbGetQuery(con, q)
#     
#     return(res)
#   }
#   
# }