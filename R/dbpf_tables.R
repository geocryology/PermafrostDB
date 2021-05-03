# =============================================================================
#'
#' @title Returns names of all tables within DB
#'
#' @description Basic information on permafrost database @ Carleton University 
#'
#' @details These simple functions return all data as data frames. When 
#'          making a query many times, optimise the SQL statement to only 
#'          request the data you actually need.
#' 
#' @return List of tables in the DB 
#' 
#' @export
#' @examples
#' tab <- dbpf_tables()
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_tables <- function() {
	con <- dbpf_con() # get connection
	tables <- dbListTables(con)
	dbDisconnect(con) # close connection
	return(tables)
}	
