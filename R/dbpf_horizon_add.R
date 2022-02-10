# =============================================================================
#'
#' @title Add Horizon
#'
#' @description Add a collection of (azimuth, horizon) measurements to a location.
#' 
#' @details Adds a collection of horizon measurements (i.e. (azimuth, horizon) pairs) 
#' to the database for a particular location.
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#' 
#' @param azimuth integer vector of azimuths relative to true north [numeric]
#' 
#' @param horizon vector of horizon angles corresponding to azimuths provided in \code{azimuth} [character]
#' 
#' @param location Name of a single location at which the horizon measurements were taken. [character]
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
dbpf_horizon_add <- function(con, azimuth, horizon, location, time_UTC, mode='test'){
  
  teststring <- "Testing"
  
  # check inputs
  if ( length(unique(azimuth)) != length(azimuth) ){
    teststring <- paste(teststring, "Duplicate azimuth values found", sep = " : ")
  }
  
  
  if ( ( sum(floor(azimuth)) != sum(azimuth) ) ){ 
    teststring <- paste(teststring, "Azimuth values must all be integers", sep = " : ")
    azimuth <- floor(azimuth)
  }
  
  if ( (any(azimuth >= 360)) || any(azimuth < 0)){
    teststring <- paste(teststring, "Azimuth values must be on the interval [0, 360)", sep = " : ")
  }
  
  if ( (any(horizon > 90)) || any(horizon < 0)){
    teststring <- paste(teststring, "Horizon values must be on the interval [0, 90]", sep = " : ")
  }
  
  sen <- na.omit(horizon_angle_sensors(con, azimuth))

  # check for existing horizon measurements
  q <- paste0("SELECT sensors.label,
                      locations.name,
                      obs.numeric_value as horizon
                 FROM (SELECT sensor_id, location, numeric_value
                       FROM observations 
                       WHERE sensor_id IN ('", paste(sen$id, collapse="','"), "')
                       ) AS obs
                       
                      INNER JOIN sensors ON sensors.id = obs.sensor_id
                      INNER JOIN locations ON locations.coordinates = obs.location
                WHERE obs.sensor_id IN ('", paste(sen$id, collapse="','"), "')
                  AND locations.name IN ('", paste(location, collapse="','"), "')")
         
  hrz <- dbGetQuery(con, q)
  
  if (nrow(hrz) > 0){
    teststring <- paste(teststring, " Existing horizon measurements found at location (",
                        paste(substr(hrz$label, 18, 20), 
                              hrz$horizon, 
                              sep=': ', 
                              collapse=", "),")")
  }
  
  # check/get location information
  q <- paste0("SELECT name, coordinates, elevation_in_metres
                 FROM locations 
                WHERE name IN ('", paste(location, collapse="','"), "')")

  lc <- dbGetQuery(con, q)

  if (length(base::setdiff(lc$name, location) == 0)) {
  
  } else if (nrow(lc) < length(location)) {
    
    teststring <- paste(teststring, " Location name missing in DB", sep = " : ")
    
  } else if (length(unique(lc$name)) != nrow(lc)) {
    
    teststring <- paste(teststring, " One or more location names duplicated in DB", sep = " : ")
    
  } 
  
  
  if (teststring != "Testing") return(teststring)
  
  if (toupper(mode) == 'TEST'){
    return("Testing: passed")
  }
  
  if (mode == 'insert'){
    
    height_min_metres <- 0
    height_max_metres <- 0
    text_value <- ""
    
    iid <- new_import_record(con)

    val <- data.frame(accuracy = as.numeric(sen$accuracy),
                      precision = as.numeric(sen$precision),
                      numeric_value = horizon,
                      height_min_metres = height_min_metres,
                      height_max_metres = height_max_metres,
                      elevation_in_metres = lc$elevation_in_metres,
                      device_id  = human_observation_id(con)$id,
                      sensor_id  = sen$id,
                      import_id  = iid$id,
                      import_key = "R-script",
                      observation_type = sen$type_of_measurement,
                      unit_of_measure  = sen$unit_of_measurement,
                      text_value = as.character(text_value),
                      logged_time = strftime(time_UTC, "%Y-%m-%d %H:%M:%S+00", usetz = FALSE),
                      corrected_utc_time = strftime(time_UTC, "%Y-%m-%d %H:%M:%S+00", usetz = FALSE),
                      location = as.character(lc$coordinates),
                      stringsAsFactors = FALSE)

  values <- df2sql(val) 
  q <- paste0("
              INSERT INTO observations (", paste(names(val), collapse=", "),") 
              VALUES ", values)

  res <- dbExecute(con, q)
  
  return(iid$id)
  }
  
  }

row2sql <- function(row){
   quoted <- paste0(paste(row[1:6], collapse=", "), ", '", paste(row[7:16], collapse="', '"), "'")
   
   parenthesized <- paste0("(", quoted, ")", collapse = ',')
   return(parenthesized)
 }

df2sql <- function(df){
  paste(apply(df, 1, row2sql), collapse=",\n")
}


horizon_angle_sensors <- function(con, azimuth){
  
  if (missing(azimuth)){
    azimuth <- seq(0, 359, 1)
  }
  
  azimuth <- sprintf("%.3i", as.numeric(unique(azimuth)))
  labels <- paste0("horizon_angle_at_",azimuth,"_true")
  
  sensors <- dbGetQuery(con, paste0("
                               SELECT id, label, accuracy, precision, type_of_measurement, unit_of_measurement
                                 FROM sensors
                                WHERE sensors.label IN ('", paste(labels, collapse="','"), "')"))
  
  if (length(azimuth) < length(sensors$id)){
    stop("duplicate sensors detected")
  }
  
  sorted_sensors <- sensors[match(labels, sensors$label),]
  
  return(sorted_sensors)
  
}



