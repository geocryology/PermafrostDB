# =============================================================================
#'
#' @title Adds new locations to database
#'
#' @description Inserts a data frame of new locations into database. Insertion 
#'              is done one location at a time and feedback is provided. 
#'              Duplicate names are identified. 
#'              The default mode (mode='test') only tests the data to be 
#'              inserted. Insert mode (mode='insert') requires a database
#'              connection generated with a login that has insert priviledge.    
#'
#' @details These simple functions return all data as data frames. When 
#'          making a query many times, optimise the SQL statement to only 
#'          request the data you actually need.
#'
#' @param locations Data frame with these columns (type): 
#'                  name (character); 
#'                  lat (numeric, latitude in WGMS84); 
#'                  lon (numeric, longitude in WGMS84); 
#'                  accuracy_in_metres (numeric);
#'                  elevation_in_metres (numeric);  
#'                  comment (character); 
#'                  record_observations (character, can only be 't' or 'f')
#'
#' @param mode Can be 'test' or 'insert' (requires login with insert previledges)
#' 
#' @param tolerance Tolerance [m] for how close a new site can be to an existing 
#'                  site. If too close, the new site is not imported.
#' 
#' @return List of all locations with a column 'inserted' indicating which 
#'         ones were inserted, columns 'duplicate_name' indicates existing name
#'         conflict, 'duplicate_site' indicates that at least one location closer
#'         than the prescribed tolerance exists. 
#' 
#' @export
#' @examples
#' con <- dbpf_con()
#' result <- dbpf_locations_add(con, locations, mode="test")
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_locations_add <- function(con, locations, mode="test", tolerance=0.1) {

    #test mode
    test_mo <- (mode == "test") + (mode == "insert")
	if (test_mo != 1) { 
		stop(paste("Parameter 'mode' must be either",
	               "'test' or 'insert'.")) 
	}                         
	
	                              
	#test information provided
	input <- subset(locations, select = c(name, lon, lat, 
	                elevation_in_metres, comment, 
	                record_observations, accuracy_in_metres))
		
	#fix column data type, add check columns
	input$lat <- as.numeric(as.character(input$lat))
	input$lon <- as.numeric(as.character(input$lon))
	input$elevation_in_metres <- as.numeric(as.character(input$elevation_in_metres))
	input$accuracy_in_metres  <- as.numeric(as.character(input$accuracy_in_metres))
	input$comment <- as.character(input$comment)
	input$record_observations <- as.character(input$record_observations)
	input$duplicate_name <- TRUE
	input$duplicate_site <- TRUE
	input$inserted <- FALSE
	
	#test for na and for entries other than t/f in record_observations
	test_ro <- nrow(input) - sum(input$record_observation == 't') - 
	           sum(input$record_observation == 'f')
	if (test_ro != 0) stop("Entries for record_observations need to be 't' or 'f'!")         
	
	#loop over locations and test
	for (r in 1:nrow(input)) {
		loc <- input[r,]
		
		#check for existing name
		query <- paste0("SELECT * FROM locations WHERE name = '" , loc$name, "'")
		if (nrow(dbGetQuery(con, query)) == 0) input$duplicate_name[r] <- FALSE 
		
		#check for nearby coordinate 
		query <- paste0("SELECT * FROM locations WHERE " ,
         "ST_DWithin(ST_SetSRID(ST_MakePoint(", loc$lon, ", ", loc$lat,"), 4326)::geography,",
         "locations.coordinates::geography, ", format(tolerance), ")")
		if (nrow(dbGetQuery(con, query)) == 0) input$duplicate_site[r] <- FALSE	
			
		#test feedback and insert or message
		if ((input$duplicate_site[r] + input$duplicate_name[r]) == 0) {
			
			query <- paste0("INSERT into locations (name, coordinates, ",
	                 "elevation_in_metres, comment, record_observations) ",
	 				 "VALUES('", loc$name, "', ", 
				     "ST_SetSRID(ST_MakePoint(", loc$lon, ", ", loc$lat,
				     "), 4326), ", loc$elevation_in_metres, ", '",
				     loc$comment, "', '", loc$record_observations, "')")                  
	 	 	if (mode == "insert") {
	 	 		dbExecute(con, query)
	 	 		input$inserted[r] = TRUE
	 	 	}
		} else {
			print(paste0("!!! Location [", loc$name, "] not imported, check returned data frame."))
		}
	}
	return(input)
}




              




