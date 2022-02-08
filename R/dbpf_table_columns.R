# =============================================================================
#'
#' @title Return all column names from a table
#'
#' @description Return all columns from a table within the
#' permafrost database @ Carleton University 
#'
#' @details These simple functions return all data as data frames. When 
#'          making a query many times, optimise the SQL statement to only 
#'          request the data you actually need.
#'          
#' @param tablename character, name of a table within the database. For a
#' complete list, use the function \link{dbpf_tables}.
#' 
#' @param detailed logical, whether or not to return a list of all column
#'  information or just names. Default to false.
#' 
#' 
#' @return List of columns in the DB table
#' 
#' @export
#' @examples
#' location_cols <- dbpf_table_columns('locations')
#' 
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_table_columns <- function(con, tablename, detailed=F){
  
  if (missing(con)){
    con <- dbpf_con()
  }
  
  query <- sprintf("SELECT * 
                   FROM information_schema.columns
                   WHERE table_name ='%s'
                   AND table_schema = 'public'", tablename)
  result <- dbGetQuery(con, query)

  if (!detailed){
    result <- result$column_name
  }
  return(result)
}