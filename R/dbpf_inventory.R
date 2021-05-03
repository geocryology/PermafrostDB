
# =============================================================================
#'
#' @title Inventory observations
#'
#' @description Return data frame with a summary of observations in DB  
#'
#' @details Return data frame with a summary of observations in DB 
#'
#' @param type Character string indicating the type of inventory to be returned.
#'             Defaults to 'observations_by_locations', others currently not
#'             implemented.
#'
#' 
#' @export
#' @examples
#' dbpf_inventory() # get all GST automatically
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_inventory <- function(type="observations_by_locations") {
	#obs locations
	if (type == "observations_by_locations") {
		con <- dbpf_con()
		res<- dbGetQuery(con, paste0("SELECT DISTINCT locations.name ",
		                 "FROM locations INNER JOIN observations ON ",
		                 "locations.coordinates = observations.location ",
		                 "ORDER BY locations.name ASC;"))
		dbDisconnect(con)
		return(res)
	}
	
}


