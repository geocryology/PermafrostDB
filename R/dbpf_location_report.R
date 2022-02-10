# =============================================================================
#'
#' @title Location report
#'
#' @description Give a location name and it returns the coordinates, the device serial number
#'              and number of observations as well as a comment if the device is still at this location
#'
#' @details With the default settings, only the information for the specific location is returned.
#'          By increasing the distance, all locations within the distance-radius are returned as well.
#'
#' @param con Connection to PermafrostDB
#'
#'
#' @param location_name   Location name as character
#'
#' @param distance    Search radius in meters for other locations, with centre at the origin location
#'
#'
#' @return Coordinates of location, details for the location such as device and number of observations and
#'         a remark if device is still at this location. Depending on the distance, locations with
#'         information within the search radius.
#'
#'
#' @export
#'
#' @examples
#' dbpf_location_report(con, "NGO-DD-1004_ST04", distance=10)
#'
#' @author Thomas Knecht <t.knecht@@hotmail.com>
# =============================================================================

dbpf_location_report <- function(con, location_name, distance=0.1){
  # test for existance
  if (dbpf_location_name_exists(con, location_name) == 0) {
  	print(paste0("Location '", location_name, "' does not exist in DB."))
  	return(FALSE)
  }

  #qery for the location name and coordinates
  query <- paste0("SELECT name, ST_X(coordinates) AS lon, ",
                "ST_Y(coordinates) AS lat ",
                "FROM locations WHERE name ='", location_name, "'")
  loc <- dbGetQuery(con, query)
  loc$lon <- format(loc$lon, digits=10)
  loc$lat <- format(loc$lat, digits=10)

  # query for the closeby locations
  query <- paste0("SELECT locations.name AS name, devices.serial_number AS device, devices_locations.timestamp AS time_installed, ",
                  "ST_Distance(ST_GeogFromText('SRID=4326;POINT(", loc$lon, " ", loc$lat, ")'), ST_Transform(locations.coordinates,4326)) ",
                  "FROM devices ",
                  "INNER JOIN devices_locations ON devices_locations.device_id = devices.id ",
                  "INNER JOIN locations ON devices_locations.location_id = locations.id ",
                  "WHERE GeometryType(ST_Centroid(locations.coordinates)) = 'POINT' AND ",
                  "ST_Distance_Sphere( ST_Point(ST_X(ST_Centroid(locations.coordinates)), ",
                  "ST_Y(ST_Centroid(locations.coordinates))), (ST_MakePoint('", loc$lon, "', '", loc$lat, "'))) <= ", distance, " ",
                  "ORDER BY name ASC, timestamp ASC;")

  #get data
  devices <- dbGetQuery(con, query)

  if (nrow(devices) > 0) {
  	devices$time_installed <- as.POSIXct(devices$time_installed)
  	devices$st_distance    <- round(devices$st_distance,3)

  	# test if the devices are really at the location
  	devices.list <- split(devices, seq(nrow(devices)))
  	devices.list1 <- lapply(devices.list, FUN = function(X) test_function1(con, X))
  	devices.table <- do.call(rbind, devices.list1)

  	devices.table1 <- do.call("rbind", as.list(
                              by(devices.table, devices.table$device,
                                 function(X){
      								if(length(X$device)>1) {
    							        mintime <- min(X$time)
      									X$device_remark[X$time != mintime] <- "wrongly assigned"
      									X
      								} else {X}
    							})))

  	devices.table2 <- devices.table1[ order(devices.table1$st_distance, devices.table1$name), ]
  	row.names(devices.table2) <- c(1:length(devices.table2$name))

  	# subset of the wanted loaction
  	location <- devices.table2[location_name==devices.table2$name,]

  	# subset of the close by locations
  	closeloc <- devices.table2[location_name!=devices.table2$name,]
  } else {
  	location <- "no further details"
  	closeloc <- "nothing found"
  }

  #printing
  writeLines('\n')
  writeLines(paste0("============== DETAILS for location ", location_name, " in DB =============",sep=""))
  writeLines("=== Coordinates ===")
  print(loc)
  writeLines('\n')
  writeLines("=== Details ===")
  print(location)
  writeLines('\n')
  writeLines(paste0("=== Locations within a radius of ", distance, "m ===",sep=""))
  print(closeloc)
  writeLines('\n')
  return(TRUE)
}




test_function1 <- function(con, serial_number){
  dev.loc <- dbpf_device_locations(con, serial_number$device)

  if (tail(dev.loc$location_name,n=1)==serial_number$name){

    serial_number$device_remark <- paste0("Good")

  }else{

    correct_loc <- tail(tail(dev.loc$location_name,n=1))
    serial_number$device_remark <- paste0("Current location: ",correct_loc)

  }


  query1 <- paste0("SELECT COUNT(observations.numeric_value) as cnt ",
                   "FROM observations ",
                   "INNER JOIN locations ON observations.location = locations.coordinates WHERE locations.name LIKE ",paste("'",serial_number$name[1],"%'",sep=""))

  cnt <- dbGetQuery(con, query1)

  serial_number$nObservations <- cnt$cnt


  return(serial_number)


}

