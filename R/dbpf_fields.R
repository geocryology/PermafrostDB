# =============================================================================
#'
#' @title Return table of fields for all tables
#'
#' @description Basic information on permafrost database @ Carleton University
#'
#' @details These simple functions return all data as data frames. When
#'          making a query many times, optimise the SQL statement to only
#'          request the data you actually need.
#'
#' @param table Character string or vector of table name(s). If empty, fields
#'              for all tables are returned. Default is table="".
#'
#' @return List of all fields for all tables in DB
#'
#' @export
#' @examples
#' con <- dbpf_con()
#' fds   <- dbpf_fields(con, tables = "")
#' fds_o <- dbpf_fields(tables = "observations")
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================
dbpf_fields <- function(con, tables = "") {

  if (missing(con)){
    con <- dbpf_con()
  }

	if (tables[1] == "") {
		tables <- dbpf_tables(con)$table_name
	}
	fields <- NULL
	for (t in 1:length(tables)) {
		fields <- rbind(fields, dbGetFields(con,tables[t]))
	}
	return(fields)
}
