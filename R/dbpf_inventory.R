
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
#' con <- dbpf_con()
#' dbpf_inventory(con) # get all GST automatically
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_inventory <- function(con, type="observations_by_locations") {
    #obs locations

  if (missing(con)){
    con <- dbpf_con()
  }

    if (type == "observations_by_locations") {
    	res<- dbGetQuery(con, paste0("SELECT DISTINCT locations.name ",
    	                 "FROM locations INNER JOIN observations ON ",
    	                 "locations.coordinates = observations.location ",
    	                 "ORDER BY locations.name ASC;"))
    	return(res)
    }

}


