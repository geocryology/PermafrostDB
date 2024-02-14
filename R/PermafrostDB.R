#' PermafrostDB: A package for working with a database for permafrost data
#'
#' The PermafrostDB package provides three categories of important functions: adding data to the database, retrieving data from the database, and special functions for permafrost data.
#' @section adding data to the database
#' @section retrieving data from the database
#' @section special functions for permafrost data
#'
#'
#' @docType package
#' @name PermafrostDb
#'
#' @importFrom ncdf4 ncvar_put nc_close ncatt_put nc_close ncvar_def nc_create
#' @importFrom DBI dbGetQuery dbBegin dbExecute dbCommit dbRollback dbSendQuery dbClearResult dbFetch dbHasCompleted dbGetRowCount
#' @importFrom graphics abline points axis rect polygon plot par lines
#' @importFrom grDevices dev.off graphics.off pdf
#' @importFrom stats approx na.omit
#' @importFrom utils file_test tail type.convert
#' @importFrom magrittr %>%
#' 
NULL
